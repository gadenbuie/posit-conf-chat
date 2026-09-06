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
# plus the next round: everything sharing the next start time today. If
# that round is tiny (e.g. staggered lightning talks), merge following
# start times within 30 minutes (up to 3) until the round fills out.
# Unless a track talk is currently on, the round is shown at track
# session granularity rather than as individual talks.
schedule_status <- function(now = conf_now()) {
  d <- schedule_data()
  date <- format(now, "%Y-%m-%d")
  time <- format(now, "%H:%M")

  items <- function(df, kind) {
    out <- data.frame(
      id = df$record_id,
      kind = kind,
      title = df$title,
      date = sched_date(df$start_time_event_local),
      start = sched_clock(df$start_time_event_local),
      end = sched_clock(df$end_time_event_local),
      location = df$effective_location_name,
      stringsAsFactors = FALSE
    )
    out$speakers <- vapply(
      out$id,
      function(id) {
        sp <- sched_speakers_for(id)
        if (nrow(sp)) paste(sp$full_name, collapse = ", ") else NA_character_
      },
      character(1)
    )
    out
  }
  granular <- rbind(
    items(d$talks, "talk"),
    items(d$workshops, "workshop"),
    items(d$events, "event")
  )
  tracks <- rbind(
    items(d$sessions, "session"),
    items(d$workshops, "workshop"),
    items(d$events, "event")
  )

  today <- granular[granular$date == date, , drop = FALSE]
  on_now <- today[today$start <= time & today$end > time, , drop = FALSE]
  on_now <- on_now[order(on_now$start, on_now$title), , drop = FALSE]

  keynotes <- d$talks$record_id[d$talks$is_keynote %in% TRUE]
  mid_track <- any(on_now$kind == "talk" & !on_now$id %in% keynotes)
  pool <- if (mid_track) {
    granular
  } else {
    rbind(tracks, granular[granular$id %in% keynotes, , drop = FALSE])
  }

  up_next <- pool[pool$date == date & pool$start > time, , drop = FALSE]
  if (nrow(up_next)) {
    starts <- utils::head(sort(unique(up_next$start)), 3)
    to_min <- function(x) {
      as.integer(substr(x, 1, 2)) * 60L + as.integer(substr(x, 4, 5))
    }
    keep <- starts[1]
    for (s in starts[-1]) {
      if (sum(up_next$start %in% keep) >= 4) {
        break
      }
      if (to_min(s) - to_min(starts[1]) > 30) {
        break
      }
      keep <- c(keep, s)
    }
    up_next <- up_next[up_next$start %in% keep, , drop = FALSE]
    up_next <- up_next[
      order(up_next$start, up_next$location, up_next$title),
      ,
      drop = FALSE
    ]
  }

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

on_now_section <- function(title, items, empty) {
  htmltools::tags$section(
    class = "mb-4",
    htmltools::tags$h2(title),
    if (nrow(items)) {
      locations <- vapply(items$location, card_value, character(1))
      cards <- lapply(items$id, function(id) contents_shinychat(show_item(id)))
      lapply(unique(locations), function(location) {
        htmltools::tagList(
          if (nzchar(location)) htmltools::tags$h3(location),
          cards[locations == location]
        )
      })
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
