show_item <- function(id, in_agenda = FALSE) {
  res <- resolve_item(id)
  ItemCardResult(
    value = jsonlite::toJSON(sched_summary(res), auto_unbox = TRUE),
    kind = res$kind,
    item = res$item,
    speakers = res$speakers,
    sessions = if (identical(res$kind, "speaker")) res$sessions else NULL,
    in_agenda = isTRUE(in_agenda)
  )
}

show_item_tool <- ellmer::tool(
  function(id) show_item(id),
  name = "show_item",
  description = "Show the user a rich detail card for a schedule item: a talk, keynote, track session, workshop, event, or speaker. The id must come from query_schedule(), list_schedule_options(), or the full-text search tool. Talks, sessions, workshops, and events are identified by record_id; speakers are identified by speaker_id. The card is displayed directly to the user; the value you receive is a compact summary.",
  arguments = list(
    id = ellmer::type_string(
      "Schedule item id: record_id for a talk/session/workshop/event, speaker_id for a speaker."
    )
  ),
  annotations = ellmer::tool_annotations(
    title = "Showing schedule item",
    icon = bsicons::bs_icon("card-heading")
  )
)
