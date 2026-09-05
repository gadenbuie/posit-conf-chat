list_schedule_options <- function(type) {
  d <- schedule_data()

  count_by_date <- function(df, date) {
    sum(sched_date(df$start_time_event_local) == date)
  }

  options_df <- switch(
    type,
    days = {
      dates <- sched_days()
      data.frame(
        date = dates,
        weekday = format(as.Date(dates), "%A"),
        talks = vapply(
          dates,
          function(x) count_by_date(d$talks, x),
          integer(1)
        ),
        sessions = vapply(
          dates,
          function(x) count_by_date(d$sessions, x),
          integer(1)
        ),
        workshops = vapply(
          dates,
          function(x) count_by_date(d$workshops, x),
          integer(1)
        ),
        events = vapply(
          dates,
          function(x) count_by_date(d$events, x),
          integer(1)
        ),
        row.names = NULL
      )
    },
    tracks = {
      s <- d$sessions[
        order(
          sched_date(d$sessions$start_time_event_local),
          d$sessions$start_time_event_local
        ),
      ]
      data.frame(
        title = s$title,
        date = sched_date(s$start_time_event_local),
        start = sched_clock(s$start_time_event_local),
        end = sched_clock(s$end_time_event_local),
        room = s$effective_location_name,
        talk_count = s$talk_count,
        row.names = NULL
      )
    },
    rooms = {
      locs <- c(
        d$talks$effective_location_name,
        d$sessions$effective_location_name,
        d$workshops$effective_location_name,
        d$events$effective_location_name
      )
      locs <- locs[!is.na(locs) & locs != ""]
      counts <- sort(table(locs), decreasing = TRUE)
      data.frame(
        room = names(counts),
        items = as.integer(counts),
        row.names = NULL
      )
    },
    speakers = {
      sp <- unique(d$speakers[c("speaker_id", "full_name")])
      sp <- sp[order(sp$full_name), ]
      data.frame(
        speaker_id = sp$speaker_id,
        name = sp$full_name,
        sessions = as.integer(table(d$speakers$speaker_id)[sp$speaker_id]),
        row.names = NULL
      )
    },
    kinds = data.frame(
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
      ),
      stringsAsFactors = FALSE
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
