query_schedule_fn <- function(
  `_intent` = NULL,
  date = NULL,
  from = NULL,
  to = NULL,
  track = NULL,
  room = NULL,
  kind = NULL,
  speaker = NULL
) {
  d <- schedule_data()
  combine <- function(df, kind_name, track_col = NULL) {
    out <- data.frame(
      id = df$record_id,
      kind = kind_name,
      title = df$title,
      date = sched_date(df$start_time_event_local),
      start = sched_clock(df$start_time_event_local),
      end = sched_clock(df$end_time_event_local),
      location = df$effective_location_name,
      track = if (is.null(track_col)) NA_character_ else df[[track_col]],
      stringsAsFactors = FALSE
    )
    out
  }
  combined <- rbind(
    combine(d$talks, "talk", "track_title"),
    combine(d$sessions, "session"),
    combine(d$workshops, "workshop"),
    combine(d$events, "event")
  )

  filters <- c()

  if (!is.null(date)) {
    resolved <- sched_resolve_date(date)
    combined <- combined[combined$date == resolved, , drop = FALSE]
    filters <- c(filters, resolved)
  }

  if (!is.null(from) && !is.null(to) && from == to) {
    combined <- combined[
      combined$start <= from & combined$end > from,
      ,
      drop = FALSE
    ]
    filters <- c(filters, paste0("at ", from))
  } else {
    if (!is.null(from)) {
      combined <- combined[combined$end > from, , drop = FALSE]
    }
    if (!is.null(to)) {
      combined <- combined[combined$start < to, , drop = FALSE]
    }
    if (!is.null(from) || !is.null(to)) {
      filters <- c(
        filters,
        paste0(
          from %||% "00:00",
          "-",
          to %||% "23:59"
        )
      )
    }
  }

  if (!is.null(track)) {
    matched <- sched_match(track, combined$track)
    combined <- combined[
      !is.na(combined$track) & combined$track == matched,
      ,
      drop = FALSE
    ]
    filters <- c(filters, paste0("track: ", matched))
  }

  if (!is.null(room)) {
    matched <- sched_match(room, combined$location)
    combined <- combined[
      !is.na(combined$location) & combined$location == matched,
      ,
      drop = FALSE
    ]
    filters <- c(filters, paste0("room: ", matched))
  }

  if (!is.null(kind)) {
    combined <- combined[combined$kind == kind, , drop = FALSE]
    filters <- c(filters, paste0("kind: ", kind))
  }

  if (!is.null(speaker)) {
    matched <- sched_match(speaker, d$speakers$full_name)
    speaker_ids <- unique(d$speakers$speaker_id[
      d$speakers$full_name == matched
    ])
    record_ids <- unique(d$speakers$record_id[
      d$speakers$speaker_id %in% speaker_ids
    ])
    combined <- combined[combined$id %in% record_ids, , drop = FALSE]
    filters <- c(filters, paste0("speaker: ", matched))
  }

  combined <- combined[
    order(combined$date, combined$start, combined$location, combined$title),
    ,
    drop = FALSE
  ]

  speakers <- vapply(
    combined$id,
    function(id) {
      sp <- sched_speakers_for(id)
      if (nrow(sp)) paste(sp$full_name, collapse = ", ") else NA_character_
    },
    character(1)
  )
  combined$speakers <- speakers

  combined$track[is.na(combined$track) | combined$track == ""] <- NA
  combined$speakers[is.na(combined$speakers) | combined$speakers == ""] <- NA
  combined$location[is.na(combined$location) | combined$location == ""] <- NA

  final <- combined[,
    c(
      "id",
      "kind",
      "title",
      "date",
      "start",
      "end",
      "location",
      "track",
      "speakers"
    ),
    drop = FALSE
  ]

  ellmer::ContentToolResult(
    value = jsonlite::toJSON(final, auto_unbox = TRUE),
    extra = list(
      display = shinychat::tool_result_display(
        title = "Searched the schedule",
        value_preview = paste(nrow(final), "matching items"),
        label = if (length(filters)) {
          paste(filters, collapse = "; ")
        } else {
          "all schedule items"
        }
      )
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

query_schedule <- ellmer::tool(
  query_schedule_fn,
  name = "query_schedule",
  description = paste(
    "Deterministically filter the posit::conf(2026) schedule by date, time window,",
    "track, room, kind, or speaker. Use this for questions like 'what's happening",
    "Tuesday at 2pm', 'what's in Ballroom H & K', or 'which talks are in the Agents,",
    "context, MCP track' -- anything where you need exact times, rooms, or tracks",
    "rather than a full-text search. All arguments are optional; combine them to",
    "narrow results."
  ),
  arguments = list(
    `_intent` = ellmer::type_string(
      "A short snippet used for display purposes to explain the call to the user.",
      required = FALSE
    ),
    date = ellmer::type_string(
      "Conference day as an ISO date (YYYY-MM-DD) or weekday name, e.g. 'Tuesday'. Pass null to skip.",
      required = FALSE
    ),
    from = ellmer::type_string(
      "Start of a time window in 24-hour conference-local time, 'HH:MM'. Pass null to skip.",
      required = FALSE
    ),
    to = ellmer::type_string(
      paste(
        "End of a time window in 24-hour conference-local time, 'HH:MM'. A record",
        "matches if it overlaps the window. Pass the same value for from and to to",
        "find what's happening at that exact time. Pass null to skip."
      ),
      required = FALSE
    ),
    track = ellmer::type_string(
      "Track (session block) title, e.g. 'Agents, context, MCP'. Use list_schedule_options(type = 'tracks') for exact titles. Pass null to skip.",
      required = FALSE
    ),
    room = ellmer::type_string(
      "Room name. Use list_schedule_options(type = 'rooms') for exact names. Pass null to skip.",
      required = FALSE
    ),
    kind = ellmer::type_enum(
      values = c("talk", "session", "workshop", "event"),
      description = "Item kind. 'talk' includes keynotes. Pass null to skip.",
      required = FALSE
    ),
    speaker = ellmer::type_string(
      "Speaker full name. Use list_schedule_options(type = 'speakers') for exact names. Pass null to skip.",
      required = FALSE
    )
  ),
  annotations = ellmer::tool_annotations(
    title = "Searching the schedule",
    icon = bsicons::bs_icon("calendar-range")
  )
)
