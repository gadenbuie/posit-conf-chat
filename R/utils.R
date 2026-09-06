update_query_string <- function(session, updates) {
  query <- shiny::parseQueryString(session$clientData$url_search)
  is_na <- map_lgl(updates, function(x) length(x) == 1 && is.na(x))
  query[names(updates)[is_na]] <- NULL
  updates <- updates[!is_na]
  query[names(updates)] <- updates
  qs <- paste(
    map_chr(
      names(query),
      function(nm) {
        paste0(
          URLencode(nm, reserved = TRUE),
          "=",
          URLencode(query[[nm]], reserved = TRUE)
        )
      }
    ),
    collapse = "&"
  )
  updateQueryString(paste0("?", qs), mode = "push", session = session)
}

filter_query_values <- function(values, defaults) {
  is_default <- map2_lgl(values, defaults[names(values)], identical)
  values[is_default] <- NA_character_
  values
}

# Keep only ids that still exist in the schedule.
valid_agenda_ids <- function(ids) {
  intersect(as.character(ids), schedule_ids())
}

# Re-register the ragnar search tool with an added `_intent` argument so
# shinychat shows why the model called it (see the shinychat tool-ui vignette).
search_tool_with_intent <- function(chat) {
  search_tool <- chat$get_tools()[["search_schedule"]]
  ellmer::tool(
    function(text, `_intent` = NULL) search_tool(text = text),
    name = search_tool@name,
    description = search_tool@description,
    arguments = list(
      text = search_tool@arguments@properties$text,
      `_intent` = ellmer::type_string(
        "A short snippet used for display purposes to explain the call to the user.",
        required = FALSE
      )
    ),
    annotations = search_tool@annotations
  )
}
