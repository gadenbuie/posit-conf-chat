library(shiny)
library(bslib)
library(ellmer)
library(ragnar)
library(shinychat)

options(
  shiny.autoreload.pattern = "\\.(r|htm|html|js|css|png|jpg|jpeg|gif|md)$"
)

store_location <- "data/ragnar.duckdb"

greeting_dynamic <- Sys.getenv("GREETING_DYNAMIC", "no") == "yes"

greeting_header <- paste0(
  '<div class="greeting-header mb-3">',
  '<a href="https://conf.posit.co/2026/" target="_blank" rel="noopener">',
  '<img src="assets/posit-conf-header.png" alt="posit::conf(2026)" ',
  'style="max-width:100%; max-height:150px; border-radius:8px">',
  '</a></div>\n\n'
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
    theme = if (conf_theme) bs_theme(brand = TRUE) else page_chat_theme(),
    sidebar = chat_sidebar(open = FALSE),
    navbar_options = if (conf_theme) {
      navbar_options(bg = "#419CF5", theme = "dark")
    },
    footer = if (conf_theme) {
      tags$head(tags$link(rel = "stylesheet", href = "assets/custom.css"))
    },
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
  client <- chat_posit(
    model = "zai-org/GLM-5.3-Flash",
    system_prompt = ellmer::interpolate_file(
      "prompt-system.md",
      date = Sys.Date()
    )
  )

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

  store <- ragnar::ragnar_store_connect(store_location)
  ragnar::ragnar_register_tool_retrieve(
    client,
    store,
    paste(
      "posit::conf(2026) Schedule.",
      "",
      "When presenting a summary of results from this tool, prefer using markdown tables.",
      sep = "\n"
    )
  )

  chat_server("chat", client)
}

shinyApp(ui, server)
