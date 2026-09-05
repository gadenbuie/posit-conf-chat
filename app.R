library(shiny)
library(bslib)
library(ellmer)
library(ragnar)
library(shinychat)

options(
  shiny.autoreload.pattern = "\\.(r|htm|html|js|css|png|jpg|jpeg|gif|md)$"
)

if (FALSE) {
  # For renv dependency detection
  library(paws.common)
  library(markdown)
  library(promises)
}

store_location <- "data/ragnar.duckdb"

greeting_dynamic <- Sys.getenv("GREETING_DYNAMIC", "no") == "yes"

shiny::addResourcePath("assets", "assets")

ui <- page_chat(
  title = "posit::conf(2026) Schedule Assistant",
  id = "chat",
  placeholder = "Ask about sessions, workshops, or build your schedule...",
  theme = bs_theme(brand = TRUE),
  sidebar = chat_sidebar(open = FALSE),
  navbar_options = bslib::navbar_options(bg = "#419CF5", theme = "dark"),
  footer = tags$head(tags$link(rel = "stylesheet", href = "assets/custom.css")),
  greeting = if (!greeting_dynamic) {
    shinychat::chat_greeting(
      paste(readLines("greeting.md", warn = FALSE), collapse = "\n")
    )
  }
)

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
    promises::then(
      greeting_client$chat_async(
        "Generate the greeting following your instructions."
      ),
      function(text) {
        shinychat::chat_set_greeting("chat", shinychat::chat_greeting(text))
      },
      function(error) {
        shinychat::chat_set_greeting(
          "chat",
          shinychat::chat_greeting(
            "Welcome to posit::conf(2026)! What would you like to get out of the conference?"
          )
        )
      }
    )
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
