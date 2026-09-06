agenda_conflict_error <- function(message) {
  structure(
    list(message = message),
    class = c("agenda_conflict", "error", "condition")
  )
}

manage_agenda <- function(action, id = NULL, agenda_ids, force = FALSE) {
  action <- match.arg(action, c("add", "remove", "clear", "show"))
  # isolate() avoids taking a reactive dependency in chat_server's observer
  agenda_get <- function() shiny::isolate(agenda_ids())
  agenda_set <- function(value) agenda_ids(value)

  if (action %in% c("add", "remove") && (is.null(id) || !nzchar(id))) {
    stop("Provide the schedule item id for action '", action, "'.")
  }

  if (action == "add") {
    res <- resolve_item(id)
    if (identical(res$kind, "speaker")) {
      stop(
        "Speakers can't be added to the agenda. Add a talk, session, workshop, or event by its record_id."
      )
    }
    summary <- sched_summary(res)
    desc <- agenda_item_desc(summary)
    current <- agenda_get()
    if (id %in% current) {
      title <- "Already in the agenda"
      text <- paste0(
        desc,
        " is already in the agenda. ",
        length(current),
        " items total."
      )
    } else {
      conflicts <- Filter(
        function(other) agenda_overlap(summary, other),
        lapply(current, function(i) sched_summary(resolve_item(i)))
      )
      if (length(conflicts) && !force) {
        stop(agenda_conflict_error(paste0(
          "Can't add ",
          desc,
          ": it overlaps with ",
          paste(
            vapply(conflicts, agenda_item_desc, character(1)),
            collapse = ", "
          ),
          ", which ",
          if (length(conflicts) == 1) "is" else "are",
          " already in the agenda. ",
          agenda_conflict_advice(summary, conflicts)
        )))
      }
      agenda_set(c(current, id))
      title <- "Added to the agenda"
      text <- paste0(
        "Added ",
        desc,
        " to the agenda. ",
        length(current) + 1,
        " items total.",
        if (length(conflicts)) {
          paste0(
            " Note: it overlaps with ",
            paste(
              vapply(conflicts, agenda_item_desc, character(1)),
              collapse = ", "
            ),
            ", which is already in your agenda."
          )
        }
      )
    }
  } else if (action == "remove") {
    current <- agenda_get()
    if (!id %in% current) {
      title <- "Not in the agenda"
      text <- paste0(
        "That item is not in the agenda. ",
        length(current),
        " items total."
      )
    } else {
      summary <- sched_summary(resolve_item(id))
      agenda_set(setdiff(current, id))
      title <- "Removed from the agenda"
      text <- paste0(
        "Removed ",
        agenda_item_desc(summary),
        " from the agenda. ",
        length(current) - 1,
        " items total."
      )
    }
  } else if (action == "clear") {
    n <- length(agenda_get())
    agenda_set(character())
    title <- "Cleared the agenda"
    text <- paste0("Cleared the agenda (", n, " items removed).")
  } else {
    items <- lapply(agenda_get(), function(i) sched_summary(resolve_item(i)))
    title <- "Your agenda"
    text <- if (!length(items)) {
      "Your agenda is empty."
    } else {
      paste0(
        "Your agenda (",
        length(items),
        " items):\n",
        paste(
          vapply(
            items,
            function(s) paste0("- ", agenda_item_desc(s)),
            character(1)
          ),
          collapse = "\n"
        )
      )
    }
  }

  ellmer::ContentToolResult(
    value = text,
    extra = list(
      display = shinychat::tool_result_display(
        title = title,
        value_preview = paste(length(agenda_get()), "items in agenda")
      )
    )
  )
}

agenda_conflicts_for <- function(id, agenda_ids) {
  summary <- sched_summary(resolve_item(id))
  Filter(
    function(other) agenda_overlap(summary, other),
    lapply(agenda_ids(), function(i) sched_summary(resolve_item(i)))
  )
}

agenda_conflict_phrase <- function(s) {
  htmltools::tags$span(
    "You already have ",
    htmltools::tags$b(s$title),
    " on your agenda for ",
    paste0(s$date, " ", s$start, "\u2013", s$end),
    if (!is.na(s$location) && nzchar(s$location)) {
      paste0(" in ", s$location)
    } else {
      ""
    },
    "."
  )
}

agenda_item_desc <- function(s) {
  location <- s$location
  paste0(
    "'",
    s$title,
    "' (",
    s$date,
    " ",
    s$start,
    "-",
    s$end,
    if (!is.null(location) && !is.na(location) && nzchar(location)) {
      paste0(", ", location)
    } else {
      ""
    },
    ")"
  )
}

agenda_overlap <- function(a, b) {
  identical(a$date, b$date) &&
    nzchar(a$start) &&
    nzchar(a$end) &&
    nzchar(b$start) &&
    nzchar(b$end) &&
    a$start < b$end &&
    b$start < a$end
}

agenda_conflict_advice <- function(new, conflicts) {
  new_is_track <- identical(new$kind, "session")
  has_track <- any(vapply(
    conflicts,
    function(s) identical(s$kind, "session"),
    logical(1)
  ))
  if (new_is_track && has_track) {
    paste(
      "These are both track sessions: ask the attendee which one they'd like to",
      "commit to; if they prefer the new one, remove the conflicting track and add this one again."
    )
  } else if (new_is_track != has_track) {
    if (new_is_track) {
      paste(
        "This is a conflict between the full track and an individual talk already in the agenda.",
        "Ask the attendee whether they'd prefer the full track commitment",
        "(remove the individual talk and add the track)",
        "or keep their granular picks and skip the track."
      )
    } else {
      paste(
        "This is a conflict between an individual talk and a track session already in the agenda.",
        "Ask the attendee whether they'd like to make the track commitment more granular",
        "(remove the track and add only the specific talks they want, including this one)",
        "or keep the full track and skip this talk."
      )
    }
  } else {
    paste(
      "These occupy the same time slot: ask the attendee which one they'd like to",
      "commit to; if they prefer the new one, remove the conflicting item and add this one again."
    )
  }
}

show_agenda_tool <- function(agenda_ids) {
  ellmer::tool(
    function() manage_agenda("show", agenda_ids = agenda_ids),
    name = "show_agenda",
    description = paste(
      "List the schedule items the attendee has saved in their agenda for",
      "posit::conf(2026), with titles, dates, times, and locations.",
      "Read-only: use manage_agenda() to change the agenda."
    ),
    arguments = list(),
    annotations = ellmer::tool_annotations(
      title = "Checking your agenda",
      icon = bsicons::bs_icon("bookmark-star")
    )
  )
}

agenda_tool <- function(agenda_ids) {
  ellmer::tool(
    function(action, id) manage_agenda(action, id, agenda_ids = agenda_ids),
    name = "manage_agenda",
    description = paste(
      "Manage the attendee's saved agenda for posit::conf(2026).",
      "The agenda is shown to the attendee in the My Agenda drawer alongside the chat,",
      "so changes made here are immediately visible to them.",
      "Use 'add' to save a talk, session, workshop, or event (by its record_id) that the attendee wants to attend.",
      "Use 'remove' to take a saved item out of the agenda.",
      "Use 'clear' to empty the agenda entirely.",
      "Use 'show' to list the currently saved items;",
      "this is mainly needed when you need the agenda's details to answer a question,",
      "since the attendee can already see the drawer themselves.",
      "Speakers cannot be added to the agenda directly; add their talk, session, workshop, or event instead."
    ),
    arguments = list(
      action = ellmer::type_enum(
        values = c("add", "remove", "clear", "show"),
        description = "Agenda action"
      ),
      id = ellmer::type_string(
        "Schedule item id (record_id) to add or remove. Required for add and remove; ignored for clear and show.",
        required = FALSE
      )
    ),
    annotations = ellmer::tool_annotations(
      title = "Updating your agenda",
      icon = bsicons::bs_icon("bookmark-plus")
    )
  )
}
