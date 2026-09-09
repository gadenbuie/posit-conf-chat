fmt_when <- function(date, start, end) {
  d <- ifelse(
    is.na(date) | !nzchar(date),
    "",
    format(as.Date(date), "%a %b %e")
  )
  s <- clock12(start)
  e <- clock12(end)
  t <- ifelse(s == "", "", ifelse(e == "", s, paste0(s, "\u2013", e)))
  trimws(paste(d, t))
}

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
  combined <- dplyr::bind_rows(
    sched_items("talks", track = TRUE, data = d),
    sched_items("sessions", data = d),
    sched_items("workshops", data = d),
    sched_items("events", data = d)
  )

  filters <- c()

  if (!is.null(date)) {
    resolved <- sched_resolve_date(date)
    combined <- dplyr::filter(combined, date == .env$resolved)
    filters <- c(filters, resolved)
  }

  if (!is.null(from) && !is.null(to) && from == to) {
    combined <- dplyr::filter(
      combined,
      start <= .env$from,
      end > .env$from
    )
    filters <- c(filters, paste0("at ", from))
  } else {
    if (!is.null(from)) {
      combined <- dplyr::filter(combined, end > .env$from)
    }
    if (!is.null(to)) {
      combined <- dplyr::filter(combined, start < .env$to)
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
    combined <- dplyr::filter(combined, !is.na(track), track == matched)
    filters <- c(filters, paste0("track: ", matched))
  }

  if (!is.null(room)) {
    matched <- sched_match(room, combined$location)
    combined <- dplyr::filter(combined, !is.na(location), location == matched)
    filters <- c(filters, paste0("room: ", matched))
  }

  if (!is.null(kind)) {
    combined <- dplyr::filter(combined, kind == .env$kind)
    filters <- c(filters, paste0("kind: ", kind))
  }

  if (!is.null(speaker)) {
    matched <- sched_match(speaker, d$speakers$full_name)
    speaker_ids <- d$speakers |>
      dplyr::filter(full_name == matched) |>
      dplyr::pull(speaker_id) |>
      unique()
    record_ids <- d$speakers |>
      dplyr::filter(speaker_id %in% speaker_ids) |>
      dplyr::pull(record_id) |>
      unique()
    combined <- dplyr::filter(combined, id %in% record_ids)
    filters <- c(filters, paste0("speaker: ", matched))
  }

  final <- combined |>
    dplyr::arrange(date, start, location, title) |>
    dplyr::left_join(sched_speaker_names(d), by = c("id" = "record_id")) |>
    dplyr::mutate(
      dplyr::across(c(track, location, speakers), \(x) dplyr::na_if(x, ""))
    ) |>
    dplyr::select(
      id,
      kind,
      title,
      date,
      start,
      end,
      location,
      track,
      speakers
    )

  results_table <- final |>
    dplyr::transmute(
      Kind = tools::toTitleCase(kind),
      Title = title,
      When = fmt_when(date, start, end),
      Where = location,
      Track = track,
      Speakers = speakers
    )

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
        },
        markdown = df_to_markdown_table(results_table),
        show_request = FALSE,
        open = TRUE,
        full_screen = TRUE,
        open_style = "framed"
      )
    )
  )
}

query_schedule <- ellmer::tool(
  query_schedule_fn,
  name = "query_schedule",
  description = paste(
    "Deterministically filter the posit::conf(2026) schedule by date, time window,",
    "track, room, kind, or speaker. Use this for questions like 'what's happening",
    "Tuesday at 2pm', 'what's in Ballroom H & K', or 'which talks are in the Agents,",
    "context, MCP track' -- anything where you need exact times, rooms, or tracks",
    "rather than a full-text search. Only `_intent` is required; all other",
    # Fixed in tidyverse/ellmer#1135 but might still exist for OpenRouter/others
    "arguments are optional. IMPORTANT: the schema may list arguments as",
    "required, but only `_intent` is -- OMIT any other argument you don't need",
    "(do not pass null or empty strings) and combine the rest to narrow results."
  ),
  arguments = list(
    `_intent` = ellmer::type_string(
      "A short snippet used for display purposes to explain the call to the user."
    ),
    date = ellmer::type_string(
      "Conference day as an ISO date (YYYY-MM-DD) or weekday name, e.g. 'Tuesday'.",
      required = FALSE
    ),
    from = ellmer::type_string(
      "Start of a time window in 24-hour conference-local time, 'HH:MM'.",
      required = FALSE
    ),
    to = ellmer::type_string(
      paste(
        "End of a time window in 24-hour conference-local time, 'HH:MM'. A record",
        "matches if it overlaps the window. Pass the same value for from and to to",
        "find what's happening at that exact time."
      ),
      required = FALSE
    ),
    track = ellmer::type_string(
      "Track (session block) title, e.g. 'Agents, context, MCP'. Use list_schedule_options(type = 'tracks') for exact titles.",
      required = FALSE
    ),
    room = ellmer::type_string(
      "Room name. Use list_schedule_options(type = 'rooms') for exact names.",
      required = FALSE
    ),
    kind = ellmer::type_enum(
      values = c("talk", "session", "workshop", "event"),
      description = "Item kind. 'talk' includes keynotes.",
      required = FALSE
    ),
    speaker = ellmer::type_string(
      "Speaker full name. Use list_schedule_options(type = 'speakers') for exact names.",
      required = FALSE
    )
  ),
  annotations = ellmer::tool_annotations(
    title = "Searching the schedule",
    icon = bsicons::bs_icon("calendar-range")
  )
)
