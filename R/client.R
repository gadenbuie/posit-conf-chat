greeting_header <- paste0(
  '<p class="greeting-header mb-3">',
  '<a href="https://conf.posit.co/2026/" target="_blank" rel="noopener">',
  '<img src="assets/posit-conf-header.png" alt="posit::conf(2026)" ',
  'style="max-width:100%; max-height:150px; border-radius:8px">',
  '</a></p>\n\n'
)

new_chat_client <- function(spec, system_prompt) {
  ellmer::chat(spec$name, system_prompt = system_prompt)
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
      "Use show_item() to present an item in detail,",
      "and query_schedule() for exact times, rooms, and tracks.",
      sep = "\n"
    ),
    name = "search_schedule",
    title = "Searching the conf schedule"
  )
  client$register_tool(search_tool_with_intent(client))

  client$register_tool(list_schedule_options_tool)
  client$register_tool(skills_tool())
  client$register_tool(query_schedule)
  client$register_tool(show_item_tool)
  client$register_tool(on_now_tool)
  client$register_tool(agenda_tool(agenda_ids))

  client
}

new_greeting_client <- function(spec, system_prompt, agenda_ids) {
  client <- new_chat_client(spec, system_prompt)
  client$register_tool(on_now_tool)
  client$register_tool(show_agenda_tool(agenda_ids))
  client
}

static_greeting <- function() {
  greeting_md <- paste(readLines("greeting.md", warn = FALSE), collapse = "\n")
  chat_greeting(paste0(greeting_header, greeting_md))
}

# Returns a function that generates the greeting once per session, streaming
# from the greeting model, and serves the cached result on later calls.
greeting_generator <- function(spec, system_prompt, agenda_ids) {
  greeting_cache <- NULL

  function() {
    if (!is.null(greeting_cache)) {
      return(chat_greeting(greeting_cache))
    }
    greeting_client <- new_greeting_client(spec, system_prompt, agenda_ids)
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
