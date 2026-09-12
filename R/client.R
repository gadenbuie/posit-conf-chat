greeting_header <- paste0(
  '[![posit::conf(2026)](assets/posit-conf-header.png)]',
  '(https://conf.posit.co/2026/)'
)

new_chat_client <- function(spec, system_prompt, api_args = NULL) {
  api_args <- api_args %||% env_json("POSIT_CONF_API_ARGS") %||% list()
  order <- api_args$provider$order
  if (!is.null(order)) {
    # as.list() keeps a single-element order as a JSON array; auto-unboxing
    # would collapse it to a string, which OpenRouter rejects.
    api_args$provider$order <- as.list(order)
  }
  ellmer::chat(spec$name, system_prompt = system_prompt, api_args = api_args)
}

new_agent_client <- function(spec, system_prompt, store_location, agenda_ids) {
  client <- new_chat_client(spec, system_prompt)

  store <- ragnar::ragnar_store_connect(store_location)
  ragnar::ragnar_register_tool_retrieve(
    client,
    store,
    paste(
      "posit::conf(2026) Schedule.",
      "",
      "Results include each item's record_id in the origin column.",
      "Each result ends with a Citations section containing a ready-made",
      "<shiny-aside> tag per item; attach these tags to your answer as",
      "described in the system prompt.",
      "Use show_item() to present an item in detail,",
      "and query_schedule() for exact times, rooms, and tracks.",
      sep = "\n"
    ),
    name = "search_schedule",
    title = "Searching the conf schedule"
  )
  client$register_tool(search_tool_with_intent(client))

  client$register_tool(skills_tool())
  client$register_tool(query_schedule)
  client$register_tool(show_item_tool)
  client$register_tool(on_now_tool)
  client$register_tool(agenda_tool(agenda_ids))

  client
}

# `context` is injected as a user turn ahead of the generate request, so the
# system prompt stays stable and cacheable across sessions.
new_greeting_client <- function(spec, system_prompt, context, agenda_ids) {
  client <- new_chat_client(
    spec,
    system_prompt,
    api_args = env_json(
      "POSIT_CONF_GREETING_API_ARGS",
      "POSIT_CONF_API_ARGS"
    )
  )
  client$set_turns(list(ellmer::Turn(
    "user",
    list(ellmer::ContentText(context))
  )))
  client$register_tool(show_agenda_tool(agenda_ids))
  client
}

static_greeting <- function() {
  greeting_md <- paste(
    readLines(file.path("prompts", "greeting-static.md"), warn = FALSE),
    collapse = "\n"
  )
  chat_greeting(paste0(greeting_header, greeting_md))
}

# Returns a function that generates the greeting by streaming from the
# greeting model, then caches it until the schedule changes (the next session
# start or end), at which point the next call regenerates it.
greeting_generator <- function(spec, system_prompt, agenda_ids) {
  greeting_cache <- NULL
  cache_expires <- NULL

  function() {
    if (
      !is.null(greeting_cache) &&
        (is.null(cache_expires) || conf_now() < cache_expires)
    ) {
      return(chat_greeting(greeting_cache))
    }
    status <- schedule_status()
    cache_expires <<- schedule_next_change(status$now)
    context <- paste(
      "Here is the current conference schedule status as JSON:",
      as.character(on_now_json(status)),
      sep = "\n\n"
    )
    greeting_client <- new_greeting_client(
      spec,
      system_prompt,
      context,
      agenda_ids
    )
    greeting_stream <- coro::async_generator(function() {
      collected <- greeting_header
      yield(greeting_header)
      stream <- greeting_client$stream_async(
        paste(
          "Generate the greeting now. Start directly with the greeting text. ",
          "Do not restate, summarize, or refer to these instructions."
        ),
        stream = "content"
      )
      for (chunk in await_each(stream)) {
        if (S7::S7_inherits(chunk, ellmer::ContentThinking)) {
          next
        }
        if (S7::S7_inherits(chunk, ellmer::ContentText)) {
          collected <- paste0(collected, chunk@text)
          yield(chunk@text)
        }
      }
      greeting_cache <<- collected
      invisible()
    })()

    chat_greeting(greeting_stream)
  }
}
