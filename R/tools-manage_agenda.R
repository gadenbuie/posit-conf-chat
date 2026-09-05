manage_agenda <- function(action, id = NULL, agenda_ids) {
  action <- match.arg(action, c("add", "remove", "clear", "show"))
  agenda_get <- function() shiny::isolate(agenda_ids())
  agenda_set <- function(value) shiny::isolate(agenda_ids(value))

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
      paste0(
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
      if (length(conflicts)) {
        stop(
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
        )
      }
      agenda_set(c(current, id))
      paste0(
        "Added ",
        desc,
        " to the agenda. ",
        length(current) + 1,
        " items total."
      )
    }
  } else if (action == "remove") {
    current <- agenda_get()
    if (!id %in% current) {
      paste0(
        "That item is not in the agenda. ",
        length(current),
        " items total."
      )
    } else {
      summary <- sched_summary(resolve_item(id))
      agenda_set(setdiff(current, id))
      paste0(
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
    paste0("Cleared the agenda (", n, " items removed).")
  } else {
    items <- lapply(agenda_get(), function(i) sched_summary(resolve_item(i)))
    if (!length(items)) {
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
