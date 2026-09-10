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
# shinychat shows why the model called it (see the shinychat tool-ui vignette)
# and with ready-made aside citations for the retrieved items.
search_tool_with_intent <- function(chat) {
  search_tool <- chat$get_tools()[["search_schedule"]]
  ellmer::tool(
    function(text, `_intent` = NULL) {
      attach_search_citations(search_tool(text = text))
    },
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

attach_search_citations <- function(result) {
  chunks <- tryCatch(
    jsonlite::fromJSON(as.character(result)),
    error = function(e) NULL
  )
  if (is.null(chunks) || is.null(chunks$origin)) {
    return(result)
  }
  origins <- unique(chunks$origin)
  origins <- origins[!is.na(origins) & nzchar(origins)]
  asides <- purrr::compact(lapply(origins, item_citation_aside))
  if (!length(asides)) {
    return(result)
  }
  paste(
    as.character(result),
    paste0(
      "## Citations\n\n",
      "Attach each item's aside tag inline at the end of the sentence or ",
      "bullet that discusses that item. Keep each tag on a single line and ",
      "copy it exactly as written, use each one at most once per reply, and ",
      "don't mention the tags to the attendee.\n\n",
      paste(asides, collapse = "\n\n")
    ),
    sep = "\n\n"
  )
}

item_citation_aside <- function(id) {
  res <- tryCatch(resolve_item(id), error = function(e) NULL)
  if (is.null(res) || identical(res$kind, "speaker")) {
    return(NULL)
  }
  item <- res$item
  label <- card_value(item$title)
  if (!nzchar(label)) {
    return(NULL)
  }
  speakers <- dplyr::pull(res$speakers, full_name)
  speakers <- speakers[!is.na(speakers) & nzchar(speakers)]
  when <- paste(
    c(
      format(as.Date(sched_date(item$start_time_event_local)), "%a"),
      if (nzchar(clock12(sched_clock(item$end_time_event_local)))) {
        paste0(
          clock12(sched_clock(item$start_time_event_local)),
          "\u2013",
          clock12(sched_clock(item$end_time_event_local))
        )
      } else {
        clock12(sched_clock(item$start_time_event_local))
      }
    ),
    collapse = " "
  )
  meta <- paste(
    c(when, card_value(item$effective_location_name), speakers),
    collapse = " \u00b7 "
  )
  body <- collapse_spaces(paste0("**", label, "** \u00b7 ", meta))
  sprintf(
    '<shiny-aside label="%s">%s</shiny-aside>',
    htmltools::htmlEscape(label, attribute = TRUE),
    body
  )
}

collapse_spaces <- function(x) {
  gsub("\\s+", " ", x)
}

df_to_markdown_table <- function(df) {
  if (nrow(df) == 0) {
    return("_No results._")
  }
  esc <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    gsub("|", "\\|", x, fixed = TRUE)
  }
  cols <- lapply(df, esc)
  row <- function(i) {
    paste0(
      "| ",
      paste(
        vapply(cols, function(col) col[[i]], character(1)),
        collapse = " | "
      ),
      " |"
    )
  }
  header <- paste0(
    "| ",
    paste(vapply(names(df), esc, character(1)), collapse = " | "),
    " |"
  )
  separator <- paste0(
    "| ",
    paste(rep("---", length(cols)), collapse = " | "),
    " |"
  )
  paste(
    c(header, separator, vapply(seq_len(nrow(df)), row, character(1))),
    collapse = "\n"
  )
}
