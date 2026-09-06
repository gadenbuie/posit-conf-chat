home_tab <- "__home__"
tab_values <- c("on_now", "full_schedule")

# Validated app state from a parsed URL query string. Fields that are missing
# or invalid are NULL (except tab, which falls back to the home tab).
query_state <- function(query, choices) {
  list(
    tab = if (!is.null(query$tab) && query$tab %in% tab_values) {
      query$tab
    } else {
      home_tab
    },
    format = if (!is.null(query$format) && query$format %in% format_choices) {
      query$format
    },
    location = if (
      !is.null(query$location) && query$location %in% c("", choices$locations)
    ) {
      query$location
    },
    speaker = if (
      !is.null(query$speaker) && query$speaker %in% c("", choices$speakers)
    ) {
      query$speaker
    }
  )
}

# Query string updates for the current app state, dropping default values so
# the URL stays clean.
state_query_values <- function(tab, format, location, speaker) {
  c(filter_query_values(
    list(tab = tab, format = format, location = location, speaker = speaker),
    list(tab = home_tab, format = "all", location = "", speaker = "")
  ))
}
