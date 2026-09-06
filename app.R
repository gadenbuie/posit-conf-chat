library(shiny)
library(bslib)
library(ellmer)
library(ragnar)
library(shinychat)
library(purrr)

if (FALSE) {
  # for renv/Connect
  library(brand.yml)
}

options(
  shiny.autoload.r = TRUE,
  shiny.autoreload.pattern = "\\.(r|htm|html|js|css|png|jpg|jpeg|gif|md|)$|_brand[.]yml$",
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
      bslib::input_dark_mode(),
      about_trigger()
    ),
    toolbar_input = bslib::toolbar(
      align = "left",
      bslib::toolbar_input_select(
        "output_style",
        label = "Reply style",
        choices = output_styles,
        selected = default_output_style,
        icon = bsicons::bs_icon("chat-quote")
      )
    ),
    sidebar = chat_sidebar(
      open = FALSE,
      p(
        class = "mt-auto border-top pt-3 small",
        "We don't keep your conversation history between sessions.",
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
      tags$script(src = "assets/prompt-style.js"),
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
    file.path("prompts", "system.md"),
    date = Sys.Date(),
    skills = skills_prompt()
  )
  greeting_prompt <- ellmer::interpolate_file(
    file.path("prompts", "greeting.md"),
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

  # Schedule outputs ----

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

  # URL query string state ----

  observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$now)) {
      session$userData$now_override <- query$now
    }
  })

  observeEvent(session$clientData$url_search, once = TRUE, {
    state <- query_state(
      shiny::parseQueryString(session$clientData$url_search),
      full_schedule_choices()
    )

    bslib::nav_select("chat_page", state$tab, session = session)
    if (!is.null(state$format)) {
      updateRadioButtons(session, "schedule_format", selected = state$format)
    }
    if (!is.null(state$location)) {
      updateSelectInput(session, "schedule_location", selected = state$location)
    }
    if (!is.null(state$speaker)) {
      updateSelectizeInput(
        session,
        "schedule_speaker",
        selected = state$speaker
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
        state_query_values(
          tab = input$chat_page,
          format = input$schedule_format %||% "all",
          location = input$schedule_location %||% "",
          speaker = input$schedule_speaker %||% ""
        )
      )
    },
    ignoreInit = TRUE
  )

  # Agenda ----

  observeEvent(input$my_agenda, {
    # The page-chat root element is addressable as "<id>_page".
    bslib::nav_select("chat_page", "__home__", session = session)
    chat_drawer_show("chat", title = "My Agenda")
  })

  observeEvent(input$about, {
    bslib::show_offcanvas(about_offcanvas(), session = session)
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

  observeEvent(input$agenda_restore, {
    ids <- valid_agenda_ids(input$agenda_restore)
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

  # Chat ----

  chat <- chat_server(
    "chat",
    client,
    greeting = if (greeting_dynamic) generate_greeting
  )

  # Output style ----

  current_style <- reactiveVal(default_output_style)

  # Taking chat$status() here also re-fires this observer when streaming
  # ends, applying a style change that was attempted mid-stream.
  observeEvent(input$output_style, {
    if (chat$status() != "idle") {
      return()
    }
    slug <- input$output_style
    # No-op when the change came from a history restore matching the UI to
    # the conversation, or when the value didn't actually change.
    if (identical(slug, current_style())) {
      return()
    }
    current_style(slug)
    apply_prompt_style(chat$client, slug)
  })

  observe({
    session$sendCustomMessage(
      "prompt_style_disabled",
      chat$status() == "streaming"
    )
  })

  chat$history$on_save(function(values) {
    values$output_style <- isolate(current_style())
    values
  })

  chat$history$on_restore(function(values) {
    style <- values$output_style %||% default_output_style
    current_style(style)
    bslib::update_toolbar_input_select(
      "output_style",
      choices = output_styles,
      selected = style
    )
  })
}

shinyApp(ui, server)
