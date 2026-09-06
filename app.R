library(shiny)
library(bslib)
library(ellmer)
library(ragnar)
library(shinychat)

options(
  shiny.autoload.r = TRUE,
  shiny.autoreload.pattern = "\\.(r|htm|html|js|css|png|jpg|jpeg|gif|md)$"
)

store_location <- "data/ragnar.duckdb"

greeting_dynamic <- Sys.getenv("GREETING_DYNAMIC", "yes") == "yes"

greeting_header <- paste0(
  '<p class="greeting-header mb-3">',
  '<a href="https://conf.posit.co/2026/" target="_blank" rel="noopener">',
  '<img src="assets/posit-conf-header.png" alt="posit::conf(2026)" ',
  'style="max-width:100%; max-height:150px; border-radius:8px">',
  '</a></p>\n\n'
)

shiny::addResourcePath("assets", "assets")

update_query_string <- function(session, updates) {
  query <- shiny::parseQueryString(session$clientData$url_search)
  is_na <- vapply(updates, function(x) length(x) == 1 && is.na(x), logical(1))
  query[names(updates)[is_na]] <- NULL
  updates <- updates[!is_na]
  query[names(updates)] <- updates
  qs <- paste(
    vapply(
      names(query),
      function(nm) {
        paste0(
          URLencode(nm, reserved = TRUE),
          "=",
          URLencode(query[[nm]], reserved = TRUE)
        )
      },
      character(1)
    ),
    collapse = "&"
  )
  updateQueryString(paste0("?", qs), mode = "push", session = session)
}

filter_query_values <- function(values, defaults) {
  is_default <- mapply(identical, values, defaults[names(values)])
  values[is_default] <- NA_character_
  values
}

# Re-register the ragnar search tool with an added `_intent` argument so
# shinychat shows why the model called it (see the shinychat tool-ui vignette).
search_tool_with_intent <- function(chat) {
  search_tool <- chat$get_tools()[["search_schedule"]]
  ellmer::tool(
    function(text, `_intent` = NULL) search_tool(text = text),
    name = search_tool@name,
    description = search_tool@description,
    arguments = list(
      text = search_tool@arguments@properties$text,
      `_intent` = ellmer::type_string(
        "A short snippet used for display purposes to explain the call to the user.",
        required = FALSE
      )
    ),
    annotations = search_tool@annotations
  )
}

default_theme <- Sys.getenv("APP_THEME", "conf")

ui <- function(req) {
  query <- shiny::parseQueryString(req$QUERY_STRING)
  theme <- ifelse(
    query$theme %||% "" %in% c("basic", "conf"),
    query$theme,
    default_theme
  )
  conf_theme <- theme == "conf"

  page_chat(
    title = "posit::conf(2026) Schedule Assistant",
    id = "chat",
    placeholder = "Ask about sessions, workshops, or build your schedule...",
    theme = if (conf_theme) {
      bs_theme(brand = TRUE)
    } else {
      page_chat_theme(brand = FALSE)
    },
    toolbar_global = bslib::toolbar(
      bslib::toolbar_input_button(
        "my_agenda",
        "My Agenda",
        icon = bsicons::bs_icon("bookmark-star-fill"),
        tooltip = "Open your saved agenda"
      ),
      bslib::input_dark_mode()
    ),
    sidebar = chat_sidebar(
      open = FALSE,
      p(
        class = "mt-auto border-top pt-3 small",
        "We don't store your conversation history;",
        "it resets in a new app session.",
        "Your agenda is saved in your browser."
      )
    ),
    pages_navbar = list(
      nav_panel(
        "On Now",
        value = "on_now",
        div(class = "p-3", uiOutput("on_now"))
      ),
      chat_nav_panel(
        "Full Schedule",
        value = "full_schedule",
        content_width = "100%",
        sidebar = full_schedule_sidebar(),
        div(class = "p-3", uiOutput("full_schedule"))
      )
    ),
    drawer = chat_drawer(
      agenda_drawer_content(character()),
      title = "My Agenda",
      open = FALSE
    ),
    navbar_options = if (conf_theme) {
      navbar_options(bg = "#419CF5", theme = "dark")
    },
    footer = tags$head(
      tags$script(src = "assets/agenda.js"),
      tags$link(rel = "stylesheet", href = "assets/custom.css")
    ),
    history = history_options(store = "memory"),
    greeting = if (!greeting_dynamic) {
      greeting_md <- paste(
        readLines("greeting.md", warn = FALSE),
        collapse = "\n"
      )
      chat_greeting(paste0(greeting_header, greeting_md))
    }
  )
}

server <- function(input, output, session) {
  system_prompt <- ellmer::interpolate_file(
    "prompt-system.md",
    date = Sys.Date(),
    skills = skills_prompt()
  )

  client <- chat_posit(
    model = "zai-org/GLM-5.3-Flash",
    system_prompt = system_prompt
  )

  agenda_ids <- reactiveVal(character())

  observeEvent(input$chat_greeting_requested, {
    if (!greeting_dynamic) {
      return()
    }
    greeting_client <- chat_posit(
      model = "zai-org/GLM-5.3-Flash",
      system_prompt = ellmer::interpolate_file(
        "prompt-greeting.md",
        date = Sys.Date()
      )
    )
    greeting_client$register_tool(on_now_tool)
    greeting_client$register_tool(show_agenda_tool(agenda_ids))
    greeting_stream <- coro::async_generator(function() {
      yield(greeting_header)
      stream <- greeting_client$stream_async(
        paste(
          "Generate the greeting now. Start directly with the greeting text. ",
          "Do not restate, summarize, or refer to these instructions."
        ),
        stream = "content"
      )
      for (chunk in await_each(stream)) {
        if (S7::S7_inherits(chunk, ellmer::ContentThinking)) {
          next
        }
        if (S7::S7_inherits(chunk, ellmer::ContentText)) {
          yield(chunk@text)
        }
      }
    })()

    chat_set_greeting("chat", chat_greeting(greeting_stream))
  })

  observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$now)) {
      session$userData$now_override <- query$now
    }
  })

  store <- ragnar::ragnar_store_connect(store_location)
  ragnar::ragnar_register_tool_retrieve(
    client,
    store,
    paste(
      "posit::conf(2026) Schedule.",
      "",
      "Results include each item's record_id in the origin column.",
      "Use show_item() to present an item in detail,",
      "and query_schedule() for exact times, rooms, and tracks.",
      sep = "\n"
    ),
    name = "search_schedule",
    title = "Searching the conf schedule"
  )
  client$register_tool(search_tool_with_intent(client))

  client$register_tool(list_schedule_options_tool)
  client$register_tool(skills_tool())
  client$register_tool(query_schedule)
  client$register_tool(show_item_tool)
  client$register_tool(on_now_tool)
  client$register_tool(agenda_tool(agenda_ids))

  output$on_now <- renderUI({
    invalidateLater(60000)
    on_now_ui(in_agenda = agenda_ids())
  })

  output$full_schedule <- renderUI({
    full_schedule_ui(
      fmt = input$schedule_format %||% "all",
      location = input$schedule_location %||% "",
      speaker = input$schedule_speaker %||% "",
      in_agenda = agenda_ids()
    )
  })

  observeEvent(input$my_agenda, {
    # The page-chat root element is addressable as "<id>_page".
    bslib::nav_select("chat_page", "__home__", session = session)
    chat_drawer_show("chat", title = "My Agenda")
  })

  home_tab <- "__home__"
  tab_values <- c("on_now", "full_schedule")

  observeEvent(session$clientData$url_search, once = TRUE, {
    query <- shiny::parseQueryString(session$clientData$url_search)

    tab <- if (!is.null(query$tab) && query$tab %in% tab_values) {
      query$tab
    } else {
      home_tab
    }
    bslib::nav_select("chat_page", tab, session = session)

    if (!is.null(query$format) && query$format %in% format_choices) {
      updateRadioButtons(session, "schedule_format", selected = query$format)
    }

    choices <- full_schedule_choices()
    if (
      !is.null(query$location) &&
        query$location %in% c("", choices$locations)
    ) {
      updateSelectInput(session, "schedule_location", selected = query$location)
    }
    if (
      !is.null(query$speaker) &&
        query$speaker %in% c("", choices$speakers)
    ) {
      updateSelectizeInput(
        session,
        "schedule_speaker",
        selected = query$speaker
      )
    }
  })

  observeEvent(
    list(
      input$chat_page,
      input$schedule_format,
      input$schedule_location,
      input$schedule_speaker
    ),
    {
      update_query_string(
        session,
        c(
          filter_query_values(
            list(
              tab = input$chat_page,
              format = input$schedule_format %||% "all",
              location = input$schedule_location %||% "",
              speaker = input$schedule_speaker %||% ""
            ),
            list(tab = home_tab, format = "all", location = "", speaker = "")
          )
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(input$agenda_restore, {
    ids <- as.character(input$agenda_restore)
    ids <- intersect(ids, schedule_ids())
    if (length(ids)) {
      agenda_ids(ids)
    }
  })

  observeEvent(agenda_ids(), {
    ids <- isolate(agenda_ids())
    session$sendCustomMessage("agenda_save", as.list(ids))
    content <- agenda_drawer_content(ids)
    if (length(ids)) {
      chat_drawer_show("chat", content = content, title = "My Agenda")
    } else {
      chat_drawer_update("chat", content = content, title = "My Agenda")
    }
  })

  observeEvent(input$agenda_remove, {
    manage_agenda("remove", input$agenda_remove, agenda_ids = agenda_ids)
  })

  chat_server("chat", client)
}

shinyApp(ui, server)
