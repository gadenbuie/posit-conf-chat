library(shiny)
library(bslib)
library(ellmer)
library(ragnar)
library(shinychat)
library(purrr)

options(
  shiny.autoload.r = TRUE,
  shiny.autoreload.pattern = "\\.(r|htm|html|js|css|png|jpg|jpeg|gif|md)$",
  shinychat.history_options.store_auto.quiet = TRUE
)

store_location <- "data/ragnar.duckdb"

greeting_dynamic <- Sys.getenv("GREETING_DYNAMIC", "no") == "yes"

# Chat client configuration. Each spec resolves its provider and model from
# the first env var that is set, falling back to the default shown.
chat_spec <- env_chat_spec(
  provider = "POSIT_CONF_PROVIDER",
  model = "POSIT_CONF_MODEL",
  default_provider = "posit",
  default_model = "zai-org/GLM-5.3-Flash"
)

# The greeting client falls back to the main chat env vars before defaults.
greeting_spec <- env_chat_spec(
  provider = c("POSIT_CONF_GREETING_PROVIDER", "POSIT_CONF_PROVIDER"),
  model = c("POSIT_CONF_GREETING_MODEL", "POSIT_CONF_MODEL"),
  default_provider = "posit",
  default_model = "zai-org/GLM-5.3-Flash"
)

shiny::addResourcePath("assets", "assets")

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
      tags$script(src = "assets/collapsible-abstract.js"),
      tags$link(rel = "stylesheet", href = "assets/custom.css")
    ),
    history = history_options(store = "memory"),
    greeting = if (!greeting_dynamic) static_greeting()
  )
}

server <- function(input, output, session) {
  agenda_ids <- reactiveVal(character())

  # Create ellmer clients (see R/client.R) ----
  system_prompt <- ellmer::interpolate_file(
    "prompt-system.md",
    date = Sys.Date(),
    skills = skills_prompt()
  )
  greeting_prompt <- ellmer::interpolate_file(
    "prompt-greeting.md",
    date = Sys.Date()
  )

  client <- new_agent_client(
    chat_spec,
    system_prompt,
    store_location,
    agenda_ids
  )
  generate_greeting <- greeting_generator(
    greeting_spec,
    greeting_prompt,
    agenda_ids
  )

  observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$now)) {
      session$userData$now_override <- query$now
    }
  })

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

  observeEvent(input$agenda_add, {
    id <- input$agenda_add
    tryCatch(
      {
        conflicts <- agenda_conflicts_for(id, agenda_ids)
        manage_agenda("add", id, agenda_ids = agenda_ids, force = TRUE)
        toast_agenda_added(id, conflicts)
      },
      error = toast_agenda_add_error
    )
  })

  observeEvent(input$agenda_show_from_toast, {
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
    toast_agenda_removed(input$agenda_remove)
  })

  chat_server(
    "chat",
    client,
    greeting = if (greeting_dynamic) generate_greeting
  )
}

shinyApp(ui, server)
