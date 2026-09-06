ContentPromptStyle <- S7::new_class(
  "ContentPromptStyle",
  parent = ellmer::ContentText,
  properties = list(
    slug = S7::new_property(S7::class_character, default = "friendly"),
    style = S7::new_property(
      S7::class_character,
      default = "full",
      validator = function(value) {
        if (!value %in% c("full", "reminder")) {
          "must be \"full\" or \"reminder\""
        }
      }
    )
  )
)

# shiny sources R/ into a child of globalenv(), but ellmer::contents_replay()
# resolves non-package S7 classes by name from shinychat's namespace, whose
# parent chain reaches globalenv() but never descends into the app env. The
# class must be visible there for saved history to replay.
assign("ContentPromptStyle", ContentPromptStyle, envir = globalenv())

output_styles <- c(
  "Friendly" = "friendly",
  "Pirate" = "pirate",
  "Cowboy" = "cowboy",
  "Grumpy Old Man" = "grumpy-old-man",
  "Concise" = "concise",
  "Hype Squad" = "hype-squad"
)

default_output_style <- unname(output_styles[1])

content_prompt_style <- function(slug, style = c("full", "reminder")) {
  style <- rlang::arg_match(style)
  slug <- rlang::arg_match(slug, unname(output_styles))
  name <- names(output_styles)[output_styles == slug]

  text <- if (style == "full") {
    path <- file.path("prompts", paste0("style-", slug, ".md"))
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    sprintf("You're now in %s mode, please reply in that style.", name)
  }

  ContentPromptStyle(
    text = sprintf("<system-reminder>\n%s\n</system-reminder>", text),
    slug = slug,
    style = style
  )
}

is_prompt_style_content <- function(content) {
  S7::S7_inherits(content, ContentPromptStyle)
}

turn_has_prompt_style <- function(turn) {
  any(purrr::map_lgl(turn@contents, is_prompt_style_content))
}

# Applies the output style to the client's chat history without touching the
# system prompt, so the prompt cache stays intact. The style content is
# appended to the most recent user turn — or, if there are no user turns yet,
# to a new first user turn. If that turn already carries a ContentPromptStyle
# it is replaced, so a style change is only "locked in" once the user submits
# a new message. The first style content in a conversation embeds the full
# style prompt; later changes embed a short reminder.
apply_prompt_style <- function(client, slug) {
  turns <- client$get_turns()

  last_idx <- purrr::detect_index(
    turns,
    function(turn) turn@role == "user",
    .dir = "backward"
  )
  if (last_idx == 0) {
    turns <- c(turns, list(ellmer::Turn("user", list())))
    last_idx <- length(turns)
  }
  last_turn <- turns[[last_idx]]

  existing <- purrr::keep(last_turn@contents, is_prompt_style_content)
  if (length(existing) && identical(existing[[length(existing)]]@slug, slug)) {
    return(invisible(FALSE))
  }

  last_turn@contents <- purrr::discard(
    last_turn@contents,
    is_prompt_style_content
  )
  turns[[last_idx]] <- last_turn

  # The default style is the persona's baseline, so returning to it only
  # ever needs a reminder; other styles embed the full prompt the first
  # time they appear in a conversation.
  has_prior_style <- any(purrr::map_lgl(turns, turn_has_prompt_style))
  style <- if (has_prior_style || slug == default_output_style) {
    "reminder"
  } else {
    "full"
  }

  last_turn@contents <- c(
    last_turn@contents,
    list(content_prompt_style(slug, style))
  )
  turns[[last_idx]] <- last_turn
  client$set_turns(turns)
  invisible(TRUE)
}

# Hide prompt-style reminders when restored history is rendered in the UI.
contents_shinychat_generic <- shinychat::contents_shinychat
S7::method(contents_shinychat_generic, ContentPromptStyle) <- function(
  content
) {
  NULL
}
