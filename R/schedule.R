.schedule_cache <- new.env(parent = emptyenv())

schedule_data <- function() {
  if (is.null(.schedule_cache$data)) {
    read <- function(name) {
      readr::read_csv(
        file.path("data", "derived", paste0(name, ".csv")),
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE,
        progress = FALSE
      )
    }
    data <- list(
      talks = read("talks") |>
        dplyr::mutate(is_keynote = is_keynote == "TRUE"),
      sessions = read("talk_sessions") |>
        dplyr::mutate(talk_count = as.integer(talk_count)),
      workshops = read("workshops"),
      events = read("events"),
      speakers = read("speakers")
    )
    # sched_date()/sched_clock() assume "YYYY-MM-DD HH:MM" local timestamps
    time_pattern <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}"
    tidyr::expand_grid(
      name = c("talks", "sessions", "workshops", "events"),
      col = c("start_time_event_local", "end_time_event_local")
    ) |>
      purrr::pwalk(function(name, col) {
        x <- data[[name]][[col]]
        bad <- !is.na(x) & x != "" & !grepl(time_pattern, x)
        if (any(bad)) {
          stop(
            "Unexpected timestamp format in ",
            name,
            "$",
            col,
            ": ",
            paste(utils::head(x[bad], 3), collapse = ", ")
          )
        }
      })
    record_ids <- sched_items(data = data)$id
    if (any(record_ids %in% data$speakers$speaker_id)) {
      stop(
        "record_id and speaker_id values overlap; item lookups would be ambiguous"
      )
    }
    .schedule_cache$data <- data
  }
  .schedule_cache$data
}

kind_labels <- c(
  talks = "talk",
  sessions = "session",
  workshops = "workshop",
  events = "event"
)

sched_items <- function(
  kinds = c("talks", "sessions", "workshops", "events"),
  track = FALSE,
  data = NULL
) {
  data <- data %||% schedule_data()
  dplyr::bind_rows(purrr::map(kinds, function(name) {
    df <- data[[name]]
    dplyr::tibble(
      id = df$record_id,
      kind = unname(kind_labels[[name]]),
      title = df$title,
      date = sched_date(df$start_time_event_local),
      start = sched_clock(df$start_time_event_local),
      end = sched_clock(df$end_time_event_local),
      location = df$effective_location_name,
      track = if (track && name == "talks") df$track_title else NA_character_
    )
  }))
}

sched_speaker_names <- function(data = NULL) {
  data <- data %||% schedule_data()
  data$speakers |>
    dplyr::filter(!is.na(full_name), full_name != "") |>
    dplyr::arrange(as.integer(speaker_order)) |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(
      speakers = paste(full_name, collapse = ", "),
      .groups = "drop"
    )
}

schedule_ids <- function() {
  unique(sched_items()$id)
}

sched_date <- function(x) substr(x, 1, 10)

sched_clock <- function(x) substr(x, 12, 16)

sched_days <- function() {
  sched_items() |>
    dplyr::filter(!is.na(date), date != "") |>
    dplyr::pull(date) |>
    unique() |>
    sort()
}

sched_resolve_date <- function(date) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
    return(date)
  }
  days <- sched_days()
  weekdays <- format(as.Date(days), "%A")
  i <- pmatch(tolower(date), tolower(weekdays))
  if (is.na(i)) {
    stop(
      "Unknown date '",
      date,
      "'. Valid days: ",
      paste(weekdays, collapse = ", ")
    )
  }
  days[i]
}

sched_match <- function(x, choices) {
  choices <- unique(choices[!is.na(choices) & choices != ""])
  hit <- choices[tolower(choices) == tolower(x)]
  if (length(hit)) {
    return(hit[1])
  }
  hit <- choices[grepl(tolower(x), tolower(choices), fixed = TRUE)]
  if (length(hit) == 1) {
    return(hit)
  }
  if (length(hit) > 1) {
    stop("Ambiguous value '", x, "'. Matches: ", paste(hit, collapse = ", "))
  }
  stop(
    "Unknown value '",
    x,
    "'. Valid values: ",
    paste(utils::head(choices, 20), collapse = ", ")
  )
}

sched_speakers_for <- function(record_id) {
  schedule_data()$speakers |>
    dplyr::filter(record_id == .env$record_id) |>
    dplyr::arrange(as.integer(speaker_order))
}

sched_sessions_by_speaker <- function(speaker_id) {
  ids <- schedule_data()$speakers |>
    dplyr::filter(speaker_id == .env$speaker_id) |>
    dplyr::pull(record_id) |>
    unique()
  purrr::map(ids, function(id) sched_summary(resolve_item(id)))
}

resolve_item <- function(id) {
  d <- schedule_data()
  sources <- list(
    talk = d$talks,
    session = d$sessions,
    workshop = d$workshops,
    event = d$events
  )
  for (kind in names(sources)) {
    hit <- dplyr::filter(sources[[kind]], record_id == .env$id)
    if (nrow(hit)) {
      return(list(
        kind = kind,
        item = purrr::map(hit, 1),
        speakers = sched_speakers_for(id)
      ))
    }
  }
  sp <- dplyr::filter(d$speakers, speaker_id == .env$id)
  if (nrow(sp)) {
    item <- purrr::map(sp, function(col) {
      col <- col[!is.na(col) & col != ""]
      if (length(col)) col[1] else NA_character_
    })
    item$record_id <- NULL
    return(list(
      kind = "speaker",
      item = item,
      speakers = dplyr::slice(sp, 1),
      sessions = sched_sessions_by_speaker(id)
    ))
  }
  stop("Unknown schedule item id: ", id)
}

sched_summary <- function(res) {
  item <- res$item
  if (identical(res$kind, "speaker")) {
    return(list(
      id = item$speaker_id,
      kind = "speaker",
      name = item$full_name,
      title = item$title_affiliation,
      sessions = sched_sessions_by_speaker(item$speaker_id)
    ))
  }
  summary <- list(
    id = item$record_id,
    kind = res$kind,
    title = item$title,
    date = sched_date(item$start_time_event_local),
    start = sched_clock(item$start_time_event_local),
    end = sched_clock(item$end_time_event_local),
    location = item$effective_location_name,
    speakers = if (nrow(res$speakers)) {
      dplyr::pull(res$speakers, full_name)
    } else {
      NULL
    }
  )
  if (identical(res$kind, "talk")) {
    summary$track <- item$track_title
    summary$is_keynote <- isTRUE(item$is_keynote)
  }
  summary
}
