conf_tz <- "America/Chicago"

# For testing the "on now" features, override the current time with a query
# string, e.g. ?now=2026-09-15+13:05, or with options(positconf.now = ...).
conf_now <- function() {
  override <- getOption("positconf.now")
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$now)) {
      override <- query$now
    }
  }
  if (!is.null(override) && nzchar(override)) {
    now <- as.POSIXct(
      gsub("[T+]", " ", override),
      format = "%Y-%m-%d %H:%M",
      tz = conf_tz
    )
    if (!is.na(now)) {
      return(now)
    }
    warning("Ignoring unparseable time override: '", override, "'")
  }
  now <- Sys.time()
  attr(now, "tzone") <- conf_tz
  now
}

conf_bounds <- function() {
  d <- schedule_data()
  times <- c(
    d$talks$start_time_event_local,
    d$sessions$start_time_event_local,
    d$workshops$start_time_event_local,
    d$events$start_time_event_local
  )
  ends <- c(
    d$talks$end_time_event_local,
    d$sessions$end_time_event_local,
    d$workshops$end_time_event_local,
    d$events$end_time_event_local
  )
  parse <- function(x) {
    as.POSIXct(substr(x, 1, 16), format = "%Y-%m-%d %H:%M", tz = conf_tz)
  }
  list(start = min(parse(times)), end = max(parse(ends)))
}

# Talks, workshops, and events (not track sessions) happening at `now`,
# plus everything still to come today.
schedule_status <- function(now = conf_now()) {
  d <- schedule_data()
  date <- format(now, "%Y-%m-%d")
  time <- format(now, "%H:%M")

  items <- function(df, kind) {
    data.frame(
      id = df$record_id,
      kind = kind,
      title = df$title,
      date = sched_date(df$start_time_event_local),
      start = sched_clock(df$start_time_event_local),
      end = sched_clock(df$end_time_event_local),
      location = df$effective_location_name,
      stringsAsFactors = FALSE
    )
  }
  all <- rbind(
    items(d$talks, "talk"),
    items(d$workshops, "workshop"),
    items(d$events, "event")
  )
  all$speakers <- vapply(
    all$id,
    function(id) {
      sp <- sched_speakers_for(id)
      if (nrow(sp)) paste(sp$full_name, collapse = ", ") else NA_character_
    },
    character(1)
  )

  today <- all[all$date == date, , drop = FALSE]
  on_now <- today[today$start <= time & today$end > time, , drop = FALSE]
  on_now <- on_now[order(on_now$start, on_now$title), , drop = FALSE]
  up_next <- today[today$start > time, , drop = FALSE]
  up_next <- up_next[order(up_next$start, up_next$title), , drop = FALSE]

  list(now = now, on_now = on_now, up_next = up_next)
}

on_now <- function() {
  status <- schedule_status()
  bounds <- conf_bounds()

  note <- NULL
  if (status$now < bounds$start) {
    note <- paste(
      "The conference has not started yet; it begins",
      format(bounds$start, "%A, %B %e at %H:%M %Z.")
    )
  } else if (status$now > bounds$end) {
    note <- "The conference has ended."
  } else if (!nrow(status$on_now)) {
    note <- "Nothing is scheduled at this exact moment."
  }

  ellmer::ContentToolResult(
    value = jsonlite::toJSON(
      list(
        now = format(status$now, "%Y-%m-%d %H:%M %Z"),
        note = note,
        on_now = status$on_now
      ),
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ),
    extra = list(
      display = shinychat::tool_result_display(
        title = "Checked what's on now",
        value_preview = if (nrow(status$on_now)) {
          paste(nrow(status$on_now), "items on now")
        } else {
          "nothing on right now"
        }
      )
    )
  )
}

on_now_tool <- ellmer::tool(
  on_now,
  name = "on_now",
  description = paste(
    "Get the current conference-local date and time and the talks, workshops,",
    "and events happening right now, at the lowest granularity (talks, not",
    "track sessions). Use this for 'what's on now' questions during the",
    "conference; use query_schedule() to find what's coming up later."
  ),
  arguments = list(),
  annotations = ellmer::tool_annotations(
    title = "Checking what's on now",
    icon = bsicons::bs_icon("clock")
  )
)

on_now_item <- function(item) {
  meta <- paste(
    c(
      paste0(clock12(item$start), "\u2013", clock12(item$end)),
      card_value(item$location),
      card_value(item$speakers)
    ),
    collapse = " \u00b7 "
  )
  htmltools::tags$div(
    class = "mb-3",
    htmltools::tags$div(class = "fw-semibold", card_value(item$title)),
    htmltools::tags$small(class = "text-muted", meta)
  )
}

on_now_section <- function(title, items, empty) {
  htmltools::tags$section(
    class = "mb-4",
    htmltools::tags$h5(title),
    if (nrow(items)) {
      lapply(seq_len(nrow(items)), function(i) on_now_item(items[i, ]))
    } else {
      htmltools::tags$div(class = "text-muted fst-italic", empty)
    }
  )
}

on_now_ui <- function(status = schedule_status(), bounds = conf_bounds()) {
  if (status$now < bounds$start) {
    days <- as.numeric(difftime(bounds$start, status$now, units = "days"))
    return(htmltools::tags$div(
      class = "text-center py-5",
      htmltools::tags$h2(class = "display-4 mb-2", ceiling(days)),
      htmltools::tags$div(
        class = "text-muted",
        "days until posit::conf(2026) starts on ",
        format(bounds$start, "%A, %B %e")
      )
    ))
  }
  if (status$now > bounds$end) {
    return(htmltools::tags$div(
      class = "text-center py-5 text-muted",
      htmltools::tags$h5("That's a wrap!"),
      htmltools::tags$p("posit::conf(2026) has ended. Thanks for attending!")
    ))
  }
  htmltools::tagList(
    on_now_section(
      "On Now",
      status$on_now,
      "Nothing scheduled at this exact moment."
    ),
    on_now_section(
      "Up Next",
      status$up_next,
      "Nothing else scheduled today."
    )
  )
}
