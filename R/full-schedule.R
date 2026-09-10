# Items shown on the Full Schedule page: track sessions (which roll up their
# talks), workshops, events, and talks that don't belong to a session.
full_schedule_items <- function() {
  d <- schedule_data()
  standalone <- d$talks |>
    dplyr::filter(
      kind == "keynote" |
        is.na(parent_session_id) |
        parent_session_id == ""
    )
  items <- dplyr::bind_rows(
    sched_items(c("sessions", "workshops", "events"), data = d),
    sched_items("talks", data = d) |>
      dplyr::filter(id %in% standalone$record_id)
  ) |>
    dplyr::filter(!is.na(date), date != "")

  formats <- dplyr::bind_rows(
    d$sessions |>
      dplyr::transmute(id = record_id, format = session_format),
    d$workshops |>
      dplyr::transmute(id = record_id, format = session_format),
    d$events |>
      dplyr::transmute(id = record_id, format = session_format),
    d$talks |>
      dplyr::transmute(id = record_id, format = effective_session_format)
  )

  sp <- sched_speaker_names(d)
  session_speakers <- d$talks |>
    dplyr::filter(
      !is.na(parent_session_id),
      parent_session_id != "",
      record_id %in% sp$record_id
    ) |>
    dplyr::distinct(record_id, parent_session_id) |>
    dplyr::left_join(sp, by = "record_id") |>
    dplyr::group_by(record_id = parent_session_id) |>
    dplyr::summarise(
      speakers = paste(speakers, collapse = ", "),
      .groups = "drop"
    )
  item_speakers <- dplyr::bind_rows(sp, session_speakers) |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(
      speakers = paste(speakers, collapse = ", "),
      .groups = "drop"
    )

  items |>
    dplyr::left_join(formats, by = "id") |>
    dplyr::left_join(item_speakers, by = c("id" = "record_id")) |>
    dplyr::arrange(date, start, location, title)
}

format_choices <- c(
  "All" = "all",
  "In-Person" = "inperson",
  "Virtual" = "virtual"
)

format_allowed <- function(choice) {
  switch(
    choice,
    inperson = c("IN_PERSON", "HYBRID"),
    virtual = c("VIRTUAL", "HYBRID"),
    character()
  )
}

full_schedule_choices <- function() {
  items <- full_schedule_items()
  locations <- sort(unique(items$location[
    !is.na(items$location) & items$location != ""
  ]))
  speakers <- items$speakers[!is.na(items$speakers) & items$speakers != ""] |>
    strsplit(", ", fixed = TRUE) |>
    unlist() |>
    unique() |>
    sort()
  list(locations = locations, speakers = speakers)
}

full_schedule_sidebar <- function() {
  choices <- full_schedule_choices()
  bslib::sidebar(
    width = 280,
    div(
      class = "full-schedule-filters",
      radioButtons(
        "schedule_format",
        "Format",
        choices = format_choices,
        selected = "all"
      ),
      conditionalPanel(
        condition = "input.schedule_format != 'virtual'",
        selectInput(
          "schedule_location",
          "Location",
          choices = c("All" = "", choices$locations),
          selected = ""
        )
      ),
      selectizeInput(
        "schedule_speaker",
        "Speaker",
        choices = c("All" = "", choices$speakers),
        selected = "",
        options = list(placeholder = "All speakers")
      )
    )
  )
}

full_schedule_ui <- function(
  fmt = "all",
  location = "",
  speaker = "",
  in_agenda = character()
) {
  items <- full_schedule_items()
  allowed <- format_allowed(fmt)
  items <- items |>
    dplyr::filter(
      if (length(allowed)) {
        is.na(format) | format == "" | format %in% allowed
      } else {
        TRUE
      },
      !nzchar(.env$location) |
        (!is.na(location) & location == .env$location),
      !nzchar(.env$speaker) |
        (!is.na(speakers) & grepl(.env$speaker, speakers, fixed = TRUE))
    )
  days <- sort(unique(items$date))
  today <- format(conf_now(), "%Y-%m-%d")
  open_days <- if (today %in% days) {
    today
  } else if (length(days) && today < days[1]) {
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
        card_with_agenda_controls(
          contents_shinychat(show_item(id, in_agenda = id %in% in_agenda)),
          id,
          id %in% in_agenda
        )
      })
    )
  })
  htmltools::tags$div(
    class = "full-schedule",
    if (!length(panels)) {
      htmltools::tags$div(
        class = "text-muted fst-italic p-4 text-center",
        "No sessions match the selected filters."
      )
    } else {
      bslib::accordion(
        id = "full_schedule_days",
        multiple = TRUE,
        open = open_days,
        !!!panels
      )
    }
  )
}
