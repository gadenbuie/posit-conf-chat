agenda_toast_title <- function(id) {
  tryCatch(sched_summary(resolve_item(id))$title, error = function(e) id)
}

show_agenda_button <- function(...) {
  bslib::toolbar_input_button(
    "agenda_show_from_toast",
    "Show agenda",
    icon = bsicons::bs_icon("bookmark-star-fill"),
    ...
  )
}

toast_agenda_added <- function(id, conflicts = character()) {
  if (length(conflicts)) {
    bslib::show_toast(
      bslib::toast(
        tags$div(
          tags$p(
            "Added",
            tags$b(agenda_toast_title(id)),
            "to your agenda."
          ),
          tags$p(
            "Heads up!",
            purrr::map(conflicts, agenda_conflict_phrase)
          ),
          show_agenda_button(show_label = TRUE, tooltip = FALSE, border = TRUE)
        ),
        header = "Added with a conflict",
        icon = bsicons::bs_icon("exclamation-triangle-fill"),
        type = "warning",
        duration_s = NA
      )
    )
  } else {
    bslib::show_toast(
      bslib::toast(
        tags$span("Added", tags$b(agenda_toast_title(id))),
        header = "Agenda updated",
        icon = bsicons::bs_icon("bookmark-plus-fill"),
        type = "success"
      )
    )
  }
}

toast_agenda_add_error <- function(e) {
  bslib::show_toast(
    bslib::toast(
      tags$div(e$message, show_agenda_button()),
      header = "Couldn't add to agenda",
      icon = bsicons::bs_icon("exclamation-triangle-fill"),
      type = "warning",
      duration_s = NA
    )
  )
}

toast_agenda_removed <- function(id) {
  bslib::show_toast(
    bslib::toast(
      tags$span("Removed", tags$b(agenda_toast_title(id))),
      header = "Agenda updated",
      icon = bsicons::bs_icon("bookmark-x-fill"),
      type = "secondary"
    )
  )
}
