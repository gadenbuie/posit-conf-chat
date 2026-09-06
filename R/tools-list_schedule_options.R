list_schedule_options <- function(type) {
  d <- schedule_data()

  options_df <- switch(
    type,
    days = sched_items(
      kinds = c("talks", "sessions", "workshops", "events"),
      data = d
    ) |>
      dplyr::filter(!is.na(date), date != "") |>
      dplyr::count(date, kind) |>
      tidyr::complete(
        date,
        kind = c("talk", "session", "workshop", "event"),
        fill = list(n = 0L)
      ) |>
      tidyr::pivot_wider(names_from = kind, values_from = n) |>
      dplyr::transmute(
        date,
        weekday = format(as.Date(date), "%A"),
        talks = as.integer(talk),
        sessions = as.integer(session),
        workshops = as.integer(workshop),
        events = as.integer(event)
      ),
    tracks = d$sessions |>
      dplyr::arrange(
        sched_date(start_time_event_local),
        start_time_event_local
      ) |>
      dplyr::transmute(
        title,
        date = sched_date(start_time_event_local),
        start = sched_clock(start_time_event_local),
        end = sched_clock(end_time_event_local),
        room = effective_location_name,
        talk_count
      ),
    rooms = sched_items(data = d) |>
      dplyr::filter(!is.na(location), location != "") |>
      dplyr::count(location, name = "items") |>
      dplyr::arrange(dplyr::desc(items)) |>
      dplyr::rename(room = location),
    speakers = d$speakers |>
      dplyr::distinct(speaker_id, full_name) |>
      dplyr::arrange(full_name) |>
      dplyr::rename(name = full_name) |>
      dplyr::left_join(
        d$speakers |> dplyr::count(speaker_id, name = "sessions"),
        by = "speaker_id"
      ),
    kinds = dplyr::tibble(
      kind = c("talk", "session", "workshop", "event"),
      description = c(
        "Individual talks, including keynotes",
        "Track blocks that group talks",
        "Hands-on workshops",
        "Conference events outside the talk tracks"
      ),
      count = c(
        nrow(d$talks),
        nrow(d$sessions),
        nrow(d$workshops),
        nrow(d$events)
      )
    )
  )

  plural <- switch(
    type,
    days = paste0("conference day", if (nrow(options_df) == 1) "" else "s"),
    tracks = paste0("track", if (nrow(options_df) == 1) "" else "s"),
    rooms = paste0("room", if (nrow(options_df) == 1) "" else "s"),
    speakers = paste0("speaker", if (nrow(options_df) == 1) "" else "s"),
    kinds = paste0("item kind", if (nrow(options_df) == 1) "" else "s")
  )

  ellmer::ContentToolResult(
    value = jsonlite::toJSON(options_df, auto_unbox = TRUE),
    extra = list(
      display = shinychat::tool_result_display(
        title = paste0("Listed ", nrow(options_df), " ", plural),
        value_preview = paste(nrow(options_df), type, "options"),
        label = type
      )
    )
  )
}

list_schedule_options_tool <- ellmer::tool(
  list_schedule_options,
  name = "list_schedule_options",
  description = "List the valid values for schedule filters. Call this before query_schedule() to learn the exact conference day dates, track titles, room names, speaker names and ids, or item kinds.",
  arguments = list(
    type = ellmer::type_enum(
      values = c("days", "tracks", "rooms", "speakers", "kinds"),
      description = "Which set of options to list"
    )
  ),
  annotations = ellmer::tool_annotations(
    title = "Listing schedule options",
    icon = bsicons::bs_icon("list-ul")
  )
)
