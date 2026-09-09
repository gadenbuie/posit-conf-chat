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
      "Attach each item's aside tag to your reply right after the sentence or ",
      "bullet that discusses that item. Copy the tags exactly as written, use ",
      "each one at most once per reply, and don't mention the tags to the attendee.\n\n",
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
  body <- paste(
    c(
      paste0("**", label, "**"),
      sched_summary_line_md(list(
        kind = res$kind,
        date = sched_date(item$start_time_event_local),
        start = sched_clock(item$start_time_event_local),
        end = sched_clock(item$end_time_event_local),
        location = item$effective_location_name,
        speakers = dplyr::pull(res$speakers, full_name),
        track = item$track_title
      )),
      excerpt_text(card_value(item$abstract), 280)
    ),
    collapse = "\n\n"
  )
  sprintf(
    '<shiny-aside label="%s">\n%s\n</shiny-aside>',
    htmltools::htmlEscape(label, attribute = TRUE),
    body
  )
}

excerpt_text <- function(x, max_chars) {
  if (!nzchar(x)) {
    return("")
  }
  if (nchar(x) <= max_chars) {
    return(x)
  }
  cut <- substr(x, 1, max_chars)
  spaces <- gregexpr(" ", cut, fixed = TRUE)[[1]]
  spaces <- spaces[spaces > 0]
  if (length(spaces) && spaces[length(spaces)] > max_chars / 2) {
    cut <- substr(cut, 1, spaces[length(spaces)] - 1)
  }
  paste0(cut, "\u2026")
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
