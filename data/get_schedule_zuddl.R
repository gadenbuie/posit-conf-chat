# install.packages(c("chromote", "httr2", "jsonlite", "digest"))

library(chromote)
library(httr2)
library(jsonlite)
library(digest)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(tidyr)

sessions_url <- "https://conf.posit.co/2026/sessions/"
event_timezone <- "America/Indiana/Knox"
output_dir <- "data"
source_file <- file.path(output_dir, "posit-conf-2026-schedule-source.json")
timestamp_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

as_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  out <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(out) == 0) NA_character_ else out[[1]]
}

is_valid_schedule <- function(schedule) {
  is.list(schedule) &&
    all(c("sessionDetails", "eventDetails") %in% names(schedule)) &&
    identical(schedule$eventDetails$title, "posit::conf(2026)") &&
    identical(schedule$eventDetails$identifier, "2026") &&
    identical(schedule$eventDetails$status, "PUBLISHED") &&
    is.list(schedule$sessionDetails) &&
    length(schedule$sessionDetails) > 0
}

html_to_markdown <- function(html) {
  needs_conversion <- !is.na(html) & html != ""

  html[needs_conversion] <- map_chr(
    html[needs_conversion],
    function(x) {
      converted <- system2(
        "pandoc",
        c("--from", "html", "--to", "gfm-raw_html"),
        input = x,
        stdout = TRUE
      )
      trimws(paste(converted, collapse = "\n"))
    }
  )

  html
}

fetch_schedule <- function(schedule_url) {
  tryCatch(
    {
      response <- request(schedule_url) |>
        req_user_agent("posit-conf-schedule-downloader/1.0") |>
        req_perform()

      resp_check_status(response)

      raw <- resp_body_string(response)
      parsed <- fromJSON(raw, simplifyVector = FALSE)

      if (!is_valid_schedule(parsed)) {
        return(NULL)
      }

      list(url = schedule_url, raw = raw, schedule = parsed)
    },
    error = function(e) NULL
  )
}

normalize_schedule_url <- function(schedule_url, timezone) {
  timezone <- URLencode(timezone, reserved = TRUE)

  if (grepl("userTimeZone=", schedule_url, fixed = TRUE)) {
    return(sub(
      "userTimeZone=[^&]*",
      paste0("userTimeZone=", timezone),
      schedule_url
    ))
  }

  paste0(schedule_url, "&userTimeZone=", timezone)
}

cached_schedule_url <- NULL

if (file.exists(source_file)) {
  cached_source <- tryCatch(
    fromJSON(source_file, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (!is.null(cached_source$schedule_api_url)) {
    cached_schedule_url <- normalize_schedule_url(
      cached_source$schedule_api_url,
      event_timezone
    )
  }
}

schedule_result <- NULL

if (!is.null(cached_schedule_url)) {
  schedule_result <- fetch_schedule(cached_schedule_url)
}

if (is.null(schedule_result)) {
  session <- ChromoteSession$new()
  schedule_url <- NULL

  session$Network$responseReceived(
    callback_ = function(params) {
      response_url <- params$response$url

      if (
        grepl(
          "/api/custom-widget/v2/schedule/",
          response_url,
          fixed = TRUE
        )
      ) {
        schedule_url <<- response_url
      }
    }
  )

  session$go_to(
    sessions_url,
    delay = 5,
    timeout_ = 60
  )

  if (is.null(schedule_url)) {
    session$close()

    stop(
      "Could not discover a schedule API request from the Sessions page."
    )
  }

  schedule_url <- normalize_schedule_url(schedule_url, event_timezone)
  session$close()

  schedule_result <- fetch_schedule(schedule_url)

  if (is.null(schedule_result)) {
    stop(
      "Discovered the schedule API URL, but could not retrieve a valid schedule."
    )
  }

  writeLines(
    toJSON(
      list(
        sessions_page_url = sessions_url,
        schedule_api_url = schedule_result$url,
        discovered_at_utc = timestamp_utc,
        event_id = schedule_result$schedule$eventDetails$eventId,
        event_title = schedule_result$schedule$eventDetails$title,
        event_identifier = schedule_result$schedule$eventDetails$identifier
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    source_file
  )
}

schedule_url <- schedule_result$url
schedule_raw <- schedule_result$raw
schedule <- schedule_result$schedule

required_top_level_fields <- c("sessionDetails", "eventDetails")

if (!all(required_top_level_fields %in% names(schedule))) {
  stop("Schedule response is missing required top-level fields.")
}

event <- schedule$eventDetails

if (!identical(event$title, "posit::conf(2026)")) {
  stop("Unexpected event title: ", event$title)
}

if (!identical(event$identifier, "2026")) {
  stop("Unexpected event identifier: ", event$identifier)
}

if (!identical(event$status, "PUBLISHED")) {
  stop("Event is not published: ", event$status)
}

records <- imap(
  schedule$sessionDetails,
  function(day_records, schedule_date) {
    map(day_records, function(record) {
      record$schedule_date <- schedule_date
      record
    })
  }
) |>
  flatten()

if (length(records) < 50) {
  stop("Schedule response has implausibly few records: ", length(records))
}

record_ids <- map_chr(records, "id")

if (anyDuplicated(record_ids)) {
  stop("Schedule response contains duplicate record IDs.")
}

dir.create(file.path(output_dir, "raw"), recursive = TRUE, showWarnings = FALSE)

writeLines(
  schedule_raw,
  file.path(output_dir, "raw", "posit-conf-2026-schedule-latest.json"),
  useBytes = TRUE
)

as_record_row <- function(record) {
  location <- compact(record$location)

  tibble(
    record_id = as_scalar(record$id),
    session_id = as_scalar(record$sessionId),
    ref_id = as_scalar(record$refId),
    parent_session_id = as_scalar(record$parentSegmentId),
    schedule_date = as_scalar(record$schedule_date),
    title = as_scalar(record$sessionName),
    abstract = as_scalar(record$description),
    start_time_utc = as_scalar(record$startTime),
    end_time_utc = as_scalar(record$endTime),
    session_type = as_scalar(record$sessionType),
    session_format = as_scalar(record$sessionFormat),
    venue = as_scalar(record$venue),
    sub_venue = as_scalar(record$subVenue),
    status = as_scalar(record$status),
    location_id = as_scalar(location$eventInPersonLocationId),
    location_name = as_scalar(location$name),
    location_city = as_scalar(location$city),
    location_state = as_scalar(location$state),
    location_country = as_scalar(location$country),
    location_map_link = as_scalar(location$mapLink)
  )
}

schedule_records <- map_dfr(records, as_record_row) |>
  mutate(
    start_time_utc = as.POSIXct(
      start_time_utc,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    end_time_utc = as.POSIXct(
      end_time_utc,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    start_time_event_local = format(
      start_time_utc,
      tz = event_timezone,
      usetz = TRUE
    ),
    end_time_event_local = format(
      end_time_utc,
      tz = event_timezone,
      usetz = TRUE
    ),
    abstract = html_to_markdown(abstract)
  )

as_speaker_row <- function(record, speaker) {
  social_links <- compact(speaker$socialLinks)

  tibble(
    record_id = as_scalar(record$id),
    session_id = as_scalar(record$sessionId),
    speaker_id = as_scalar(speaker$speakerId),
    account_id = as_scalar(speaker$accountId),
    speaker_order = as.integer(as_scalar(speaker$order)),
    first_name = as_scalar(speaker$firstName),
    last_name = as_scalar(speaker$lastName),
    title_affiliation = as_scalar(speaker$title),
    biography = as_scalar(speaker$description),
    image_url = as_scalar(speaker$speakerImage),
    linkedin_url = as_scalar(social_links$linkedIn)
  )
}

speakers <- map2_dfr(
  records,
  map(records, "speakers"),
  function(record, record_speakers) {
    if (length(record_speakers) == 0) {
      return(NULL)
    }
    map_dfr(record_speakers, ~ as_speaker_row(record, .x))
  }
) |>
  mutate(
    full_name = trimws(paste(first_name, last_name)),
    .after = last_name
  ) |>
  mutate(biography = html_to_markdown(biography))

parent_session_ids <- unique(na.omit(schedule_records$parent_session_id))

parent_records <- schedule_records |>
  filter(session_id %in% parent_session_ids) |>
  select(
    parent_session_id = session_id,
    track_title = title,
    track_start_time_utc = start_time_utc,
    track_end_time_utc = end_time_utc,
    track_start_time_event_local = start_time_event_local,
    track_end_time_event_local = end_time_event_local,
    track_session_format = session_format,
    track_venue = venue,
    track_sub_venue = sub_venue,
    track_location_id = location_id,
    track_location_name = location_name
  )

presentations <- schedule_records |>
  left_join(parent_records, by = "parent_session_id") |>
  mutate(
    duration_hours = as.numeric(difftime(
      end_time_utc,
      start_time_utc,
      units = "hours"
    )),
    is_child_record = !is.na(parent_session_id) & parent_session_id != "",
    presentation_kind = case_when(
      is_child_record ~ "talk",
      schedule_date == "2026-09-14" &
        (session_format == "VIRTUAL" | duration_hours >= 3) ~ "workshop",
      grepl("^Keynote", title) ~ "keynote",
      venue == "STAGE" ~ "talk_session",
      .default = "event"
    ),
    effective_location_name = coalesce(
      na_if(location_name, ""),
      track_location_name
    ),
    effective_session_format = coalesce(
      na_if(session_format, ""),
      track_session_format
    )
  )

locations <- schedule_records |>
  filter(!is.na(location_id), location_id != "") |>
  distinct(
    location_id,
    location_name,
    location_city,
    location_state,
    location_country,
    location_map_link
  )

content_hash <- digest(
  toJSON(
    list(schedule_records = schedule_records, speakers = speakers),
    auto_unbox = TRUE,
    null = "null"
  ),
  algo = "sha256"
)

manifest <- list(
  retrieved_at_utc = timestamp_utc,
  sessions_page_url = sessions_url,
  schedule_api_url = schedule_url,
  event_id = event$eventId,
  event_title = event$title,
  event_identifier = event$identifier,
  event_timezone = event$tz,
  event_updated_at = event$updatedAt,
  record_count = nrow(schedule_records),
  unique_speaker_count = n_distinct(speakers$speaker_id),
  raw_sha256 = digest(schedule_raw, algo = "sha256"),
  schedule_content_sha256 = content_hash
)

writeLines(
  toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
  file.path(output_dir, "raw", "posit-conf-2026-manifest-latest.json")
)

schedule_records_columns <- c(
  "record_id",
  "session_id",
  "parent_session_id",
  "title",
  "abstract",
  "start_time_utc",
  "end_time_utc",
  "start_time_event_local",
  "end_time_event_local",
  "session_type",
  "session_format",
  "venue",
  "sub_venue",
  "location_name",
  "location_id",
  "status"
)

write.csv(
  select(schedule_records, all_of(schedule_records_columns)),
  file.path(output_dir, "derived", "schedule_records.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  speakers,
  file.path(output_dir, "derived", "speakers.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  locations,
  file.path(output_dir, "derived", "locations.csv"),
  row.names = FALSE,
  na = ""
)

talks_columns <- c(
  schedule_records_columns,
  "track_title",
  "track_start_time_utc",
  "track_end_time_utc",
  "track_start_time_event_local",
  "track_end_time_event_local",
  "track_session_format",
  "track_venue",
  "track_sub_venue",
  "track_location_id",
  "track_location_name",
  "effective_location_name",
  "effective_session_format"
)

program_columns <- c(
  "record_id",
  "session_id",
  "title",
  "abstract",
  "start_time_utc",
  "end_time_utc",
  "start_time_event_local",
  "end_time_event_local",
  "session_type",
  "session_format",
  "effective_location_name"
)

talk_sessions_columns <- c(
  "record_id",
  "session_id",
  "title",
  "abstract",
  "start_time_utc",
  "end_time_utc",
  "start_time_event_local",
  "end_time_event_local",
  "session_format",
  "venue",
  "sub_venue",
  "effective_location_name",
  "talk_count"
)

workshops <- filter(presentations, presentation_kind == "workshop")
events <- filter(presentations, presentation_kind == "event")
talks <- filter(presentations, presentation_kind %in% c("talk", "keynote"))
talk_sessions <- filter(presentations, presentation_kind == "talk_session") |>
  left_join(
    filter(presentations, presentation_kind == "talk") |>
      count(parent_session_id, name = "talk_count"),
    by = c("session_id" = "parent_session_id")
  ) |>
  replace_na(list(talk_count = 0L)) |>
  select(all_of(talk_sessions_columns))

dir.create(
  file.path(output_dir, "derived"),
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  select(talks, all_of(talks_columns), kind = presentation_kind),
  file.path(output_dir, "derived", "talks.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  talk_sessions,
  file.path(output_dir, "derived", "talk_sessions.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  select(workshops, all_of(program_columns)),
  file.path(output_dir, "derived", "workshops.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  select(events, all_of(program_columns)),
  file.path(output_dir, "derived", "events.csv"),
  row.names = FALSE,
  na = ""
)
