register_slash_commands <- function(chat, current_style) {
  session <- shiny::getDefaultReactiveDomain()

  chat$slash_command(
    "now",
    "What's happening right now?",
    function(content) {
      content@text <- paste(
        "What's happening at the conference right now?",
        "Use the on_now tool to find out, then use show_item()",
        "to display a card for each item currently in progress,",
        "and mention what's coming up next."
      )
      respond_to_command(chat, content)
    }
  )

  chat$slash_command(
    "speaker",
    "Show a speaker's card",
    function(content) {
      content@text <- sprintf(
        paste(
          "Find the speaker named \u201c%s\u201d using search_schedule or",
          "list_schedule_options, then use show_item() with their speaker_id",
          "to display their card. If the name is ambiguous, ask which speaker",
          "was meant before showing a card."
        ),
        content@user_text
      )
      respond_to_command(chat, content)
    }
  )

  chat$slash_command(
    "add",
    "Find an item and add it to your agenda",
    function(content) {
      content@text <- sprintf(
        paste(
          "Find the schedule item matching \u201c%s\u201d using search_schedule,",
          "show its card with show_item(), and add it to the attendee's agenda",
          "with manage_agenda(). If the search is ambiguous, ask which item",
          "was meant and add it only after they confirm."
        ),
        content@user_text
      )
      respond_to_command(chat, content)
    }
  )

  chat$slash_command(
    "style",
    sprintf(
      "Switch the reply style: %s",
      paste(names(output_styles), collapse = ", ")
    ),
    function(content) {
      if (chat$status() != "idle") {
        stop("Wait for the current reply to finish before switching styles.")
      }
      slug <- style_slug_from_text(content@user_text, current_style())
      current_style(slug)
      bslib::update_toolbar_input_select(
        "output_style",
        choices = output_styles,
        selected = slug,
        session = session
      )
      apply_prompt_style(chat$client, slug)
      content@text <- sprintf(
        paste(
          "The user switched the reply style to %s with the /style command.",
          "Reply with a single sentence acknowledging the new style,",
          "written in the new style."
        ),
        names(output_styles)[output_styles == slug]
      )
      respond_to_command(chat, content)
    }
  )

  chat$slash_command(
    "agenda",
    "Open your agenda drawer",
    function() {
      chat_drawer_show("chat", title = "My Agenda")
    },
    echo = FALSE
  )

  chat$slash_command(
    "new",
    "Start a new conversation",
    function() {
      chat$new_chat(greeting = TRUE)
    },
    echo = FALSE
  )
}

style_slug_from_text <- function(text, current_slug) {
  slugs <- unname(output_styles)
  if (!nzchar(text)) {
    return(slugs[(which(slugs == current_slug) %% length(slugs)) + 1])
  }
  norm <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  want <- norm(text)
  hit <- slugs[norm(slugs) == want]
  if (!length(hit)) {
    hit <- slugs[startsWith(norm(names(output_styles)), want)]
  }
  if (length(hit) != 1) {
    stop(
      "Unknown style '",
      text,
      "'. Valid styles: ",
      paste(names(output_styles), collapse = ", ")
    )
  }
  hit
}

respond_to_command <- function(chat, content) {
  chat$append(chat$client$stream(content))
}
