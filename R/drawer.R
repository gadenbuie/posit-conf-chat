agenda_drawer_content <- function(ids) {
  if (!length(ids)) {
    return(htmltools::tags$div(
      class = "text-muted px-3 py-2",
      "No sessions saved yet.",
      htmltools::tags$br(),
      "Ask the assistant to add one, e.g. ",
      htmltools::tags$em("\u201cAdd the Quarto workshop to my agenda.\u201d")
    ))
  }
  summaries <- lapply(ids, function(id) sched_summary(resolve_item(id)))
  summaries <- summaries[order(
    vapply(summaries, function(s) s$date, character(1)),
    vapply(summaries, function(s) s$start, character(1))
  )]
  rows <- lapply(summaries, function(s) {
    id <- s$id
    when <- paste(
      trimws(format(as.Date(s$date), "%a, %b %e")),
      paste0(s$start, "\u2013", s$end)
    )
    if (!is.null(s$location) && !is.na(s$location) && nzchar(s$location)) {
      when <- paste(when, s$location, sep = " \u00b7 ")
    }
    htmltools::tags$div(
      class = "d-flex justify-content-between align-items-start gap-2 border-bottom py-2",
      htmltools::tags$div(
        class = "flex-grow-1",
        htmltools::tags$div(class = "fw-semibold", s$title),
        htmltools::tags$small(class = "text-muted d-block", when)
      ),
      htmltools::tags$button(
        class = "btn btn-sm btn-outline-danger flex-shrink-0",
        `aria-label` = paste("Remove", s$title, "from agenda"),
        onclick = sprintf(
          "Shiny.setInputValue('agenda_remove', '%s', {priority: 'event'})",
          id
        ),
        bsicons::bs_icon("x")
      )
    )
  })
  htmltools::tags$div(rows)
}
