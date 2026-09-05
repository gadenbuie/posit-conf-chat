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

card_meta <- function(date, start, end, location) {
  date <- card_value(date)
  start <- clock12(start)
  end <- clock12(end)
  location <- card_value(location)
  parts <- character()
  if (nzchar(date)) {
    parts <- c(parts, date)
  }
  if (nzchar(start)) {
    parts <- c(parts, if (nzchar(end)) paste0(start, "\u2013", end) else start)
  }
  if (nzchar(location)) {
    parts <- c(parts, location)
  }
  if (!length(parts)) {
    return(NULL)
  }
  htmltools::tags$div(
    class = "text-muted small mb-2",
    htmltools::HTML(paste(parts, collapse = " &middot; "))
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
  body <- htmltools::tagList(
    htmltools::tags$div(
      htmltools::tags$strong(name),
      if (nzchar(card_value(sp$title_affiliation))) {
        htmltools::tags$div(htmltools::tags$small(
          class = "text-muted",
          card_value(sp$title_affiliation)
        ))
      }
    ),
    if (nzchar(linkedin)) {
      htmltools::tags$small(
        htmltools::tags$a(
          href = linkedin,
          target = "_blank",
          rel = "noopener",
          "LinkedIn"
        )
      )
    }
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
  htmltools::tags$div(
    class = "card shadow-sm mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$h5(
        class = "card-title",
        card_value(item$title),
        if (isTRUE(item$is_keynote)) {
          htmltools::tags$span(class = "badge text-bg-warning ms-2", "Keynote")
        }
      ),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      if (nzchar(card_value(item$track_title))) {
        htmltools::tags$div(
          class = "text-muted small mb-2",
          paste("Track:", item$track_title)
        )
      },
      card_md(item$abstract),
      card_speaker_list(content@speakers)
    )
  )
}

card_session <- function(content) {
  item <- content@item
  talks <- schedule_data()$talks
  children <- talks[talks$parent_session_id == item$session_id, , drop = FALSE]
  children <- children[order(children$start_time_event_local), , drop = FALSE]
  talk_rows <- lapply(seq_len(nrow(children)), function(i) {
    talk <- children[i, , drop = FALSE]
    speaker_names <- unique(sched_speakers_for(talk$record_id)$full_name)
    speaker_names <- speaker_names[
      !is.na(speaker_names) & nzchar(speaker_names)
    ]
    htmltools::tags$div(
      class = "mb-2",
      htmltools::tags$strong(card_value(talk$title)),
      htmltools::tags$div(
        class = "text-muted small",
        paste(
          c(
            paste0(
              clock12(sched_clock(talk$start_time_event_local)),
              "\u2013",
              clock12(sched_clock(talk$end_time_event_local))
            ),
            speaker_names
          ),
          collapse = " \u00b7 "
        )
      )
    )
  })
  htmltools::tags$div(
    class = "card shadow-sm mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$h5(class = "card-title", card_value(item$title)),
      card_meta(
        sched_date(item$start_time_event_local),
        sched_clock(item$start_time_event_local),
        sched_clock(item$end_time_event_local),
        item$effective_location_name
      ),
      if (length(talk_rows)) {
        htmltools::tagList(talk_rows)
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
  badge <- if (identical(format, "VIRTUAL")) {
    htmltools::tags$span(class = "badge text-bg-info mb-2", "Virtual")
  } else {
    htmltools::tags$span(class = "badge text-bg-secondary mb-2", "In-person")
  }
  htmltools::tags$div(
    class = "card shadow-sm mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$h5(class = "card-title", card_value(item$title)),
      badge,
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
    class = "card shadow-sm mb-2",
    style = "max-width: 640px",
    htmltools::tags$div(
      class = "card-body",
      htmltools::tags$h5(class = "card-title", card_value(item$title)),
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
    class = "card shadow-sm mb-2",
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
