about_trigger <- function() {
  bslib::toolbar_input_button(
    "about",
    "About",
    icon = bsicons::bs_icon("info-circle"),
    tooltip = "About posit::conf and this app"
  )
}

about_link <- function(href, label, desc, icon) {
  tags$p(
    class = "mb-2",
    tags$a(
      class = "text-decoration-none",
      href = href,
      target = "_blank",
      rel = "noopener",
      .noWS = "after",
      bsicons::bs_icon(icon),
      label
    ),
    tags$span(class = "d-block small text-body-secondary", desc)
  )
}

about_offcanvas <- function() {
  bslib::offcanvas(
    id = "about_offcanvas",
    title = "posit::conf(2026)",
    placement = "right",
    width = "400px",
    tags$p(
      "posit::conf is Posit's annual conference. Use this assistant to",
      "browse sessions and workshops, build your agenda, and get answers",
      "to general conference questions: pricing, registration, the venue,",
      "travel, accessibility, and more."
    ),
    h6(class = "mt-3 mb-2", "Links"),
    about_link(
      "https://posit.co/conference/",
      "posit::conf(2026)",
      "Conference website, registration, and program",
      icon = "calendar2-event"
    ),
    about_link(
      "https://posit.co/",
      "Posit",
      "Posit's open source tools, including R and Python products",
      icon = "building"
    ),
    about_link(
      "https://solutions.posit.co/",
      "Posit Support",
      "Documentation, articles, and support resources",
      icon = "question-circle"
    ),
    h6(class = "mt-4 mb-2", "How this app is built"),
    tags$p(
      class = "small",
      "We built this assistant using",
      tags$a(
        href = "https://shiny.posit.co/",
        target = "_blank",
        rel = "noopener",
        .noWS = "after",
        "Shiny"
      ),
      "! Its chat answers from a search index",
      "of the conference schedule, built with",
      tags$a(
        href = "https://ragnar.tidyverse.org/",
        target = "_blank",
        rel = "noopener",
        "ragnar"
      ),
      "and",
      tags$a(
        href = "https://ellmer.tidyverse.org/",
        target = "_blank",
        rel = "noopener",
        .noWS = "after",
        "ellmer"
      ),
      "."
    ),
    about_link(
      "https://posit-dev.github.io/shinychat/",
      "shinychat",
      "Chat interfaces for Shiny",
      icon = "chat-dots"
    ),
    about_link(
      "https://ragnar.tidyverse.org/",
      "ragnar",
      "Retrieval-augmented generation (RAG) in R",
      icon = "search"
    ),
    about_link(
      "https://ellmer.tidyverse.org/",
      "ellmer",
      "LLM chat clients for R",
      icon = "robot"
    ),
    about_link(
      "https://posit-dev.github.io/querychat/",
      "querychat",
      "Chat with your data in SQL, powered by ellmer and shinychat",
      icon = "database"
    ),
    about_link(
      "https://posit-dev.github.io/btw/",
      "btw",
      "Capture your R environment and docs for LLM context",
      icon = "terminal"
    )
  )
}
