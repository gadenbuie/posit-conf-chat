# install.packages(c("chromote", "httr2", "jsonlite"))

library(chromote)
library(httr2)
library(jsonlite)

sessions_url <- "https://conf.posit.co/2026/sessions/"
event_timezone <- "America/Indiana/Knox"
output_dir <- "data"
source_file <- file.path(output_dir, "posit-conf-2026-schedule-source.json")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

is_valid_schedule <- function(schedule) {
  is.list(schedule) &&
    all(c("sessionDetails", "eventDetails") %in% names(schedule)) &&
    identical(schedule$eventDetails$title, "posit::conf(2026)") &&
    identical(schedule$eventDetails$identifier, "2026") &&
    identical(schedule$eventDetails$status, "PUBLISHED") &&
    is.list(schedule$sessionDetails) &&
    length(schedule$sessionDetails) > 0
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

      list(
        url = schedule_url,
        raw = raw,
        schedule = parsed
      )
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
        discovered_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
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

records <- unlist(
  Map(
    function(schedule_date, day_records) {
      lapply(day_records, function(record) {
        record$schedule_date <- schedule_date
        record
      })
    },
    names(schedule$sessionDetails),
    schedule$sessionDetails
  ),
  recursive = FALSE
)

if (length(records) < 50) {
  stop("Schedule response has implausibly few records: ", length(records))
}

record_ids <- vapply(records, function(x) as_scalar(x$id), character(1))

if (anyDuplicated(record_ids)) {
  stop("Schedule response contains duplicate record IDs.")
}

raw_snapshot_file <- file.path(
  output_dir,
  "raw",
  paste0("posit-conf-2026-schedule-", snapshot_id, ".json")
)

writeLines(schedule_raw, raw_snapshot_file, useBytes = TRUE)

writeLines(
  schedule_raw,
  file.path(output_dir, "raw", "posit-conf-2026-schedule-latest.json"),
  useBytes = TRUE
)

schedule_records <- do.call(
  rbind,
  lapply(records, function(record) {
    location <- record$location %||% list()

    data.frame(
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
      location_map_link = as_scalar(location$mapLink),
      stringsAsFactors = FALSE
    )
  })
)

schedule_records$start_time_utc <- as.POSIXct(
  schedule_records$start_time_utc,
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)

schedule_records$end_time_utc <- as.POSIXct(
  schedule_records$end_time_utc,
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)

schedule_records$start_time_event_local <- format(
  schedule_records$start_time_utc,
  tz = event_timezone,
  usetz = TRUE
)

schedule_records$end_time_event_local <- format(
  schedule_records$end_time_utc,
  tz = event_timezone,
  usetz = TRUE
)

speaker_rows <- unlist(
  lapply(records, function(record) {
    if (length(record$speakers) == 0) {
      return(list())
    }

    lapply(record$speakers, function(speaker) {
      social_links <- speaker$socialLinks %||% list()

      data.frame(
        record_id = as_scalar(record$id),
        session_id = as_scalar(record$sessionId),
        speaker_id = as_scalar(speaker$speakerId),
        account_id = as_scalar(speaker$accountId),
        speaker_order = as.integer(speaker$order %||% NA_integer_),
        first_name = as_scalar(speaker$firstName),
        last_name = as_scalar(speaker$lastName),
        full_name = trimws(paste(
          as_scalar(speaker$firstName),
          as_scalar(speaker$lastName)
        )),
        title_affiliation = as_scalar(speaker$title),
        biography = as_scalar(speaker$description),
        image_url = as_scalar(speaker$speakerImage),
        linkedin_url = as_scalar(social_links$linkedIn),
        stringsAsFactors = FALSE
      )
    })
  }),
  recursive = FALSE
)

speakers <- if (length(speaker_rows) > 0) {
  do.call(rbind, speaker_rows)
} else {
  data.frame(
    record_id = character(),
    session_id = character(),
    speaker_id = character(),
    account_id = character(),
    speaker_order = integer(),
    first_name = character(),
    last_name = character(),
    full_name = character(),
    title_affiliation = character(),
    biography = character(),
    image_url = character(),
    linkedin_url = character()
  )
}

parent_session_ids <- unique(na.omit(schedule_records$parent_session_id))

parent_records <- schedule_records[
  schedule_records$session_id %in% parent_session_ids,
  c(
    "session_id",
    "title",
    "start_time_utc",
    "end_time_utc",
    "start_time_event_local",
    "end_time_event_local",
    "session_format",
    "venue",
    "sub_venue",
    "location_id",
    "location_name"
  )
]

names(parent_records) <- c(
  "parent_session_id",
  "track_title",
  "track_start_time_utc",
  "track_end_time_utc",
  "track_start_time_event_local",
  "track_end_time_event_local",
  "track_session_format",
  "track_venue",
  "track_sub_venue",
  "track_location_id",
  "track_location_name"
)

schedule_records$row_order <- seq_len(nrow(schedule_records))

presentations <- merge(
  schedule_records,
  parent_records,
  by = "parent_session_id",
  all.x = TRUE,
  sort = FALSE
)

presentations <- presentations[order(presentations$row_order), ]
presentations$row_order <- NULL

is_child_record <- !is.na(presentations$parent_session_id) &
  presentations$parent_session_id != ""

duration_hours <- as.numeric(
  difftime(
    presentations$end_time_utc,
    presentations$start_time_utc,
    units = "hours"
  )
)

is_pre_conference_workshop <- presentations$schedule_date == "2026-09-14" &
  (presentations$session_format == "VIRTUAL" | duration_hours >= 3)

presentations$presentation_kind <- ifelse(
  is_child_record,
  "talk",
  ifelse(
    is_pre_conference_workshop,
    "workshop",
    ifelse(
      grepl("^Keynote", presentations$title),
      "keynote",
      ifelse(presentations$venue == "STAGE", "talk_session", "event")
    )
  )
)

presentations$is_keynote <- presentations$presentation_kind == "keynote"

presentations$effective_location_name <- ifelse(
  is.na(presentations$location_name) | presentations$location_name == "",
  presentations$track_location_name,
  presentations$location_name
)

presentations$effective_session_format <- ifelse(
  is.na(presentations$session_format) | presentations$session_format == "",
  presentations$track_session_format,
  presentations$session_format
)

locations <- unique(
  schedule_records[
    !is.na(schedule_records$location_id) &
      schedule_records$location_id != "",
    c(
      "location_id",
      "location_name",
      "location_city",
      "location_state",
      "location_country",
      "location_map_link"
    )
  ]
)

content_hash <- digest(
  toJSON(
    list(
      schedule_records = schedule_records,
      speakers = speakers
    ),
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
  unique_speaker_count = length(unique(speakers$speaker_id)),
  raw_sha256 = digest(schedule_raw, algo = "sha256"),
  schedule_content_sha256 = content_hash,
  raw_snapshot_file = raw_snapshot_file
)

writeLines(
  toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
  file.path(
    output_dir,
    "raw",
    paste0("posit-conf-2026-manifest-", snapshot_id, ".json")
  )
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
  schedule_records[schedule_records_columns],
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
  "effective_session_format",
  "is_keynote"
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

workshops <- presentations[presentations$presentation_kind == "workshop", ]
events <- presentations[presentations$presentation_kind == "event", ]
talks <- presentations[
  presentations$presentation_kind %in% c("talk", "keynote"),
]

talk_sessions <- presentations[
  presentations$presentation_kind == "talk_session",
]

talk_counts <- table(
  presentations$parent_session_id[presentations$presentation_kind == "talk"]
)
talk_sessions$talk_count <- as.integer(
  talk_counts[match(talk_sessions$session_id, names(talk_counts))]
)
talk_sessions$talk_count[is.na(talk_sessions$talk_count)] <- 0L

write.csv(
  talks[talks_columns],
  file.path(output_dir, "derived", "talks.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  talk_sessions[talk_sessions_columns],
  file.path(output_dir, "derived", "talk_sessions.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  workshops[program_columns],
  file.path(output_dir, "derived", "workshops.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  events[program_columns],
  file.path(output_dir, "derived", "events.csv"),
  row.names = FALSE,
  na = ""
)
