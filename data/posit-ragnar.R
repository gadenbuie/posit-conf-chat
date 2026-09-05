library(ragnar)

store_location <- here::here("data/ragnar.duckdb")
derived_dir <- here::here("data/derived")

talks <- readr::read_csv(
  file.path(derived_dir, "talks.csv"),
  show_col_types = FALSE
)
workshops <- readr::read_csv(
  file.path(derived_dir, "workshops.csv"),
  show_col_types = FALSE
)
speakers <- readr::read_csv(
  file.path(derived_dir, "speakers.csv"),
  show_col_types = FALSE
)

speaker_list <- aggregate(
  full_name ~ record_id,
  speakers,
  FUN = paste,
  collapse = ", "
)
names(speaker_list)[2] <- "speakers"

strip_html <- function(x) {
  x <- gsub("<[^>]*>", " ", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

format_sessions <- function(sessions) {
  glue::glue_data(
    sessions,
    r"---(
Title: {title}
Kind: {kind}
Session: {track_title}
Speakers: {speakers}
Time: {start_time_event_local} to {end_time_event_local}
Format: {session_format}
Room: {effective_location_name}
Abstract: {abstract}
)---",
    .na = ""
  ) |>
    strip_html()
}

talk_sessions_df <- merge(talks, speaker_list, by = "record_id", all.x = TRUE)
talk_sessions_df$kind <- ifelse(
  talk_sessions_df$is_keynote,
  "keynote",
  "talk"
)
talk_sessions_df$track_title <- ifelse(
  talk_sessions_df$is_keynote,
  "Keynote",
  talk_sessions_df$track_title
)

workshop_sessions_df <- merge(
  workshops,
  speaker_list,
  by = "record_id",
  all.x = TRUE
)
workshop_sessions_df$kind <- "workshop"
workshop_sessions_df$track_title <- NA_character_

chunks <- data.frame(
  origin = c(talks$record_id, workshops$record_id),
  hash = vapply(
    c(talks$title, workshops$title),
    rlang::hash,
    character(1)
  ),
  text = c(
    format_sessions(talk_sessions_df),
    format_sessions(workshop_sessions_df)
  )
)

store <- ragnar_store_create(
  store_location,
  embed = NULL,
  name = "posit_conf_schedule",
  title = "posit::conf(2026) Schedule",
  version = 1,
  overwrite = TRUE
)

ragnar_store_insert(store, chunks)
ragnar_store_build_index(store, type = "fts")
