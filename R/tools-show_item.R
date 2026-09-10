show_item <- function(id, in_agenda = FALSE) {
  res <- resolve_item(id)
  ItemCardResult(
    value = sched_summary_md(res),
    kind = res$kind,
    item = card_item(res$kind, res$item),
    speakers = card_speakers(res$speakers),
    sessions = if (identical(res$kind, "speaker")) res$sessions else NULL,
    in_agenda = isTRUE(in_agenda)
  )
}

sched_summary_md <- function(res) {
  item <- res$item
  if (identical(res$kind, "speaker")) {
    lines <- c(
      paste0("**", card_value(item$full_name), "**"),
      card_value(item$title_affiliation)
    )
    sessions <- res$sessions
    if (length(sessions)) {
      lines <- c(
        lines,
        "",
        "**Sessions**",
        vapply(
          sessions,
          function(s) paste0("- ", sched_summary_line_md(s)),
          character(1)
        )
      )
    }
    return(paste(lines[nzchar(lines)], collapse = "\n\n"))
  }
  lines <- c(
    paste0("**", card_value(item$title), "**"),
    sched_summary_line_md(list(
      kind = res$kind,
      date = sched_date(item$start_time_event_local),
      start = sched_clock(item$start_time_event_local),
      end = sched_clock(item$end_time_event_local),
      location = item$effective_location_name,
      speakers = dplyr::pull(res$speakers, full_name),
      track = item$track_title
    ))
  )
  if (identical(res$kind, "keynote")) {
    lines <- paste0("Keynote\n\n", paste(lines, collapse = "\n\n"))
  }
  abstract <- card_value(item$abstract)
  if (nzchar(abstract)) {
    lines <- c(lines, abstract)
  }
  paste(lines, collapse = "\n\n")
}

sched_summary_line_md <- function(s) {
  when <- paste(
    c(
      card_date(s$date),
      if (nzchar(clock12(s$end))) {
        paste0(clock12(s$start), "\u2013", clock12(s$end))
      } else {
        clock12(s$start)
      }
    ),
    collapse = " "
  )
  parts <- c(
    card_value(s$kind),
    when,
    card_value(s$location)
  )
  line <- paste(parts[nzchar(parts)], collapse = " \u00b7 ")
  speakers <- s$speakers
  if (length(speakers)) {
    speakers <- speakers[!is.na(speakers) & nzchar(speakers)]
    if (length(speakers)) {
      line <- paste0(line, " \u00b7 ", paste(speakers, collapse = ", "))
    }
  }
  track <- card_value(s$track)
  if (nzchar(track)) {
    line <- paste0(line, " \u00b7 ", track)
  }
  line
}

show_item_tool <- ellmer::tool(
  function(id) show_item(id),
  name = "show_item",
  description = "Display a rich detail card for a schedule item to the user: a talk, keynote, track session, workshop, event, or speaker. The id is a record_id (talk/session/workshop/event), a speaker_id (speaker), or the item's title or the speaker's full name; a unique partial match is accepted and ambiguous matches return an error listing the options. The tool also returns the item's details (title, date, time, location, speakers, abstract) in plain markdown, so you can answer follow-up questions about the item without calling another tool.",
  arguments = list(
    id = ellmer::type_string(
      "Schedule item id: record_id for a talk/session/workshop/event, speaker_id for a speaker, or a title or speaker name."
    )
  ),
  annotations = ellmer::tool_annotations(
    title = "Showing schedule item",
    icon = bsicons::bs_icon("card-heading")
  )
)
