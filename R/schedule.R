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
    talks <- read("talks")
    talks$is_keynote <- talks$is_keynote == "TRUE"
    sessions <- read("talk_sessions")
    sessions$talk_count <- as.integer(sessions$talk_count)
    data <- list(
      talks = talks,
      sessions = sessions,
      workshops = read("workshops"),
      events = read("events"),
      speakers = read("speakers"),
      locations = read("locations")
    )
    record_ids <- unique(c(
      data$talks$record_id,
      data$sessions$record_id,
      data$workshops$record_id,
      data$events$record_id
    ))
    if (length(intersect(record_ids, data$speakers$speaker_id))) {
      stop("record_id and speaker_id values overlap; item lookups would be ambiguous")
    }
    .schedule_cache$data <- data
  }
  .schedule_cache$data
}

sched_date <- function(x) substr(x, 1, 10)

sched_clock <- function(x) substr(x, 12, 16)

sched_days <- function() {
  d <- schedule_data()
  dates <- c(
    sched_date(d$talks$start_time_event_local),
    sched_date(d$sessions$start_time_event_local),
    sched_date(d$workshops$start_time_event_local),
    sched_date(d$events$start_time_event_local)
  )
  sort(unique(dates[dates != ""]))
}

sched_resolve_date <- function(date) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
    return(date)
  }
  days <- sched_days()
  weekdays <- format(as.Date(days), "%A")
  i <- pmatch(tolower(date), tolower(weekdays))
  if (is.na(i)) {
    stop("Unknown date '", date, "'. Valid days: ", paste(weekdays, collapse = ", "))
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
    "Unknown value '", x, "'. Valid values: ",
    paste(utils::head(choices, 20), collapse = ", ")
  )
}

sched_speakers_for <- function(record_id) {
  d <- schedule_data()
  sp <- d$speakers[d$speakers$record_id == record_id, , drop = FALSE]
  if (nrow(sp)) {
    sp <- sp[order(as.integer(sp$speaker_order)), , drop = FALSE]
  }
  sp
}

sched_sessions_by_speaker <- function(speaker_id) {
  d <- schedule_data()
  ids <- unique(d$speakers$record_id[d$speakers$speaker_id == speaker_id])
  lapply(ids, function(id) sched_summary(resolve_item(id)))
}

resolve_item <- function(id) {
  d <- schedule_data()
  find <- function(df) which(df$record_id == id)
  i <- find(d$talks)
  if (length(i)) {
    return(list(
      kind = "talk",
      item = as.list(d$talks[i[1], ]),
      speakers = sched_speakers_for(id)
    ))
  }
  i <- find(d$sessions)
  if (length(i)) {
    return(list(
      kind = "session",
      item = as.list(d$sessions[i[1], ]),
      speakers = sched_speakers_for(id)
    ))
  }
  i <- find(d$workshops)
  if (length(i)) {
    return(list(
      kind = "workshop",
      item = as.list(d$workshops[i[1], ]),
      speakers = sched_speakers_for(id)
    ))
  }
  i <- find(d$events)
  if (length(i)) {
    return(list(
      kind = "event",
      item = as.list(d$events[i[1], ]),
      speakers = sched_speakers_for(id)
    ))
  }
  sp <- d$speakers[d$speakers$speaker_id == id, , drop = FALSE]
  if (nrow(sp)) {
    item <- lapply(sp, function(col) {
      col <- col[!is.na(col) & col != ""]
      if (length(col)) col[1] else NA_character_
    })
    item$record_id <- NULL
    return(list(
      kind = "speaker",
      item = item,
      speakers = sp[1, , drop = FALSE],
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
    speakers = if (nrow(res$speakers)) res$speakers$full_name else NULL
  )
  if (identical(res$kind, "talk")) {
    summary$track <- item$track_title
    summary$is_keynote <- isTRUE(item$is_keynote)
  }
  summary
}
