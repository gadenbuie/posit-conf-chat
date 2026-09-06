# Items shown on the Full Schedule page: track sessions (which roll up their
# talks), workshops, events, and talks that don't belong to a session.
full_schedule_items <- function() {
  d <- schedule_data()
  standalone <- d$talks |>
    dplyr::filter(
      is_keynote | is.na(parent_session_id) | parent_session_id == ""
    )
  dplyr::bind_rows(
    sched_items(c("sessions", "workshops", "events"), data = d),
    sched_items("talks", data = d) |>
      dplyr::filter(id %in% standalone$record_id)
  ) |>
    dplyr::filter(!is.na(date), date != "") |>
    dplyr::arrange(date, start, location, title)
}

full_schedule_ui <- function() {
  items <- full_schedule_items()
  days <- sort(unique(items$date))
  today <- format(conf_now(), "%Y-%m-%d")
  open_days <- if (today %in% days) {
    today
  } else if (today < days[1]) {
    days[1]
  } else {
    character()
  }
  panels <- purrr::map(days, function(day) {
    day_items <- items |>
      dplyr::filter(date == .env$day)
    bslib::accordion_panel(
      title = card_date(day),
      value = day,
      purrr::map(day_items$id, function(id) {
        contents_shinychat(show_item(id))
      })
    )
  })
  htmltools::tags$div(
    class = "full-schedule",
    bslib::accordion(
      id = "full_schedule_days",
      multiple = TRUE,
      open = open_days,
      !!!panels
    )
  )
}
