ItemCardResult <- S7::new_class(
  "ItemCardResult",
  parent = ellmer::ContentToolResult,
  properties = list(
    kind = S7::class_character,
    item = S7::class_list,
    speakers = S7::class_any,
    sessions = S7::class_any
  )
)

contents_shinychat <- shinychat::contents_shinychat

S7::method(contents_shinychat, ItemCardResult) <- function(content) {
  switch(
    content@kind,
    talk = card_talk(content),
    session = card_session(content),
    workshop = card_workshop(content),
    event = card_event(content),
    speaker = card_speaker(content),
    stop("Unknown card kind: ", content@kind)
  )
}

card_value <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.atomic(x)) {
    return("")
  }
  if (isTRUE(x)) {
    return("TRUE")
  }
  if (isFALSE(x)) {
    return("")
  }
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) "" else x
}

clock12 <- function(x) {
  x <- card_value(x)
  if (!nzchar(x)) {
    return("")
  }
  parts <- strsplit(x, ":", fixed = TRUE)[[1]]
  hour <- as.integer(parts[1])
  minute <- if (length(parts) > 1) parts[2] else "00"
  meridiem <- if (hour < 12) "AM" else "PM"
  hour12 <- hour %% 12
  if (hour12 == 0) {
    hour12 <- 12
  }
  sprintf("%d:%s %s", hour12, minute, meridiem)
}

card_date <- function(date) {
  date <- card_value(date)
  if (!nzchar(date)) {
    return("")
  }
  trimws(format(as.Date(date), "%A, %B %e"))
}

conf_icon <- function(paths) {
  htmltools::HTML(paste0(
    '<svg class="conf-icon" viewBox="0 0 24 24" fill="none" ',
    'stroke="currentColor" stroke-width="2" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">',
    paths,
    "</svg>"
  ))
}

icon_calendar <- conf_icon(
  '<rect x="3" y="4" width="18" height="18" rx="2"/><path d="M16 2v4M8 2v4M3 10h18"/>'
)
icon_clock <- conf_icon(
  '<circle cx="12" cy="12" r="10"/><path d="M12 6v6l4 2"/>'
)
icon_pin <- conf_icon(
  '<path d="M20 10c0 6-8 12-8 12S4 16 4 10a8 8 0 0 1 16 0Z"/><circle cx="12" cy="10" r="3"/>'
)

card_chip <- function(label, modifier) {
  label <- card_value(label)
  if (!nzchar(label)) {
    return(NULL)
  }
  htmltools::tags$span(
    class = paste("badge rounded-pill text-uppercase conf-chip", modifier),
    label
  )
}

card_meta <- function(date, start, end, location) {
  date <- card_value(date)
  start <- clock12(start)
  end <- clock12(end)
  location <- card_value(location)
  items <- list()
  if (nzchar(date)) {
    items <- c(
      items,
      list(htmltools::tags$span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        icon_calendar,
        date
      ))
    )
  }
  if (nzchar(start)) {
    items <- c(
      items,
      list(htmltools::tags$span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        icon_clock,
        if (nzchar(end)) paste0(start, "\u2013", end) else start
      ))
    )
  }
  if (nzchar(location)) {
    items <- c(
      items,
      list(htmltools::tags$span(
        class = "d-inline-flex align-items-center gap-1 text-nowrap",
        icon_pin,
        location
      ))
    )
  }
  if (!length(items)) {
    return(NULL)
  }
  htmltools::tags$div(
    class = "conf-muted d-flex flex-wrap column-gap-3 row-gap-1 small mb-2",
    items
  )
}

card_md <- function(md) {
  md <- card_value(md)
  if (!nzchar(md)) {
    return(NULL)
  }
  htmltools::HTML(commonmark::markdown_html(md))
}

card_speaker_row <- function(sp) {
  name <- card_value(sp$full_name)
  image <- card_value(sp$image_url)
  linkedin <- card_value(sp$linkedin_url)
  body <- htmltools::tags$div(
    htmltools::tags$div(
      htmltools::tags$strong(name),
      if (nzchar(card_value(sp$title_affiliation))) {
        htmltools::tags$div(htmltools::tags$small(
          class = "text-muted",
          card_value(sp$title_affiliation)
        ))
      },
      if (nzchar(linkedin)) {
        htmltools::tags$div(
          htmltools::tags$a(
            class = "conf-speaker-link",
            href = linkedin,
            target = "_blank",
            rel = "noopener",
            "LinkedIn"
          )
        )
      }
    )
  )
  if (nzchar(image)) {
    htmltools::tags$div(
      class = "d-flex align-items-center mb-2 gap-2",
      htmltools::tags$img(
        src = image,
        class = "rounded-circle flex-shrink-0",
        width = 48,
        height = 48,
        alt = name,
        loading = "lazy"
      ),
      body
    )
  } else {
    htmltools::tags$div(class = "d-flex align-items-center mb-2 gap-2", body)
  }
}

card_speaker_list <- function(sp) {
  if (is.null(sp) || !nrow(sp)) {
    return(NULL)
  }
  rows <- if (inherits(sp, "data.frame")) {
    lapply(seq_len(nrow(sp)), function(i) as.list(sp[i, , drop = FALSE]))
  } else {
    list(sp)
  }
  htmltools::tagList(lapply(rows, card_speaker_row))
}

card_talk <- function(content) {
  item <- content@item
  is_keynote <- isTRUE(item$is_keynote)
  htmltools::tags$div(
    class = paste(
      "card conf-card border-0 shadow-sm rounded-3 mb-2",
      if (is_keynote) "conf-card-keynote"
    ),
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        if (is_keynote) {
          card_chip("Keynote", "conf-chip-keynote")
        } else {
          card_chip("Talk", "conf-chip-talk")
        },
        card_chip(item$track_title, "conf-chip-track")
      ),
      htmltools::tags$h5(class = "card-title text-balance", card_value(item$title)),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      card_md(item$abstract),
      card_speaker_list(content@speakers)
    )
  )
}

card_session <- function(content) {
  item <- content@item
  talks <- schedule_data()$talks
  children <- talks[
    !is.na(talks$parent_session_id) &
      talks$parent_session_id == item$session_id,
    ,
    drop = FALSE
  ]
  children <- children[order(children$start_time_event_local), , drop = FALSE]
  talk_rows <- lapply(seq_len(nrow(children)), function(i) {
    talk <- children[i, , drop = FALSE]
    speaker_names <- unique(sched_speakers_for(talk$record_id)$full_name)
    speaker_names <- speaker_names[
      !is.na(speaker_names) & nzchar(speaker_names)
    ]
    htmltools::tags$div(
      class = "conf-session-talk d-flex flex-column flex-sm-row gap-1 gap-sm-3 py-2",
      htmltools::tags$div(
        class = "conf-session-talk-time conf-muted small",
        paste0(
          clock12(sched_clock(talk$start_time_event_local)),
          "\u2013",
          clock12(sched_clock(talk$end_time_event_local))
        )
      ),
      htmltools::tags$div(
        htmltools::tags$strong(card_value(talk$title)),
        if (length(speaker_names)) {
          htmltools::tags$div(
            class = "conf-muted small",
            paste(speaker_names, collapse = " \u00b7 ")
          )
        }
      )
    )
  })
  htmltools::tags$div(
    class = "card conf-card border-0 shadow-sm rounded-3 mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        card_chip("Session", "conf-chip-session"),
        card_chip(item$track_title, "conf-chip-track")
      ),
      htmltools::tags$h5(class = "card-title text-balance", card_value(item$title)),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      if (length(talk_rows)) {
        htmltools::tags$div(class = "conf-session-talks", talk_rows)
      } else {
        htmltools::tags$div(
          class = "text-muted fst-italic",
          "Talks to be announced"
        )
      }
    )
  )
}

card_workshop <- function(content) {
  item <- content@item
  format <- card_value(item$session_format)
  format_chip <- if (identical(format, "VIRTUAL")) {
    card_chip("Virtual", "conf-chip-virtual")
  } else {
    card_chip("In-person", "conf-chip-inperson")
  }
  htmltools::tags$div(
    class = "card conf-card border-0 shadow-sm rounded-3 mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        card_chip("Workshop", "conf-chip-workshop"),
        format_chip
      ),
      htmltools::tags$h5(class = "card-title text-balance", card_value(item$title)),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      card_md(item$abstract),
      card_speaker_list(content@speakers)
    )
  )
}

card_event <- function(content) {
  item <- content@item
  htmltools::tags$div(
    class = "card conf-card border-0 shadow-sm rounded-3 mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        card_chip("Event", "conf-chip-event")
      ),
      htmltools::tags$h5(class = "card-title text-balance", card_value(item$title)),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      card_md(item$abstract),
      card_speaker_list(content@speakers)
    )
  )
}

card_speaker <- function(content) {
  item <- content@item
  name <- card_value(item$full_name)
  image <- card_value(item$image_url)
  sessions <- content@sessions
  session_items <- if (length(sessions)) {
    lapply(sessions, function(s) {
      htmltools::tags$div(
        class = "mb-2",
        htmltools::tags$strong(card_value(s$title)),
        htmltools::tags$div(
          class = "text-muted small",
          paste(
            c(
              paste0(
                toupper(substr(card_value(s$kind), 1, 1)),
                substr(card_value(s$kind), 2, nchar(card_value(s$kind)))
              ),
              card_date(s$date),
              paste0(clock12(s$start), "\u2013", clock12(s$end)),
              card_value(s$location)
            ),
            collapse = " \u00b7 "
          )
        )
      )
    })
  }
  htmltools::tags$div(
    class = "card conf-card border-0 shadow-sm rounded-3 mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      if (nzchar(image)) {
        htmltools::tags$img(
          src = image,
          class = "rounded-circle mb-2",
          width = 96,
          height = 96,
          alt = name,
          loading = "lazy"
        )
      },
      htmltools::tags$h5(class = "card-title", name),
      if (nzchar(card_value(item$title_affiliation))) {
        htmltools::tags$div(
          class = "text-muted mb-2",
          card_value(item$title_affiliation)
        )
      },
      card_md(item$biography),
      if (length(session_items)) {
        htmltools::tagList(
          htmltools::tags$h6(class = "mt-3", "Sessions"),
          session_items
        )
      }
    )
  )
}
