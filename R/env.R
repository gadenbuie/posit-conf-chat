env_var <- function(..., default = NULL) {
  for (nm in c(...)) {
    value <- Sys.getenv(nm, unset = NA_character_)
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  default
}

env_var_first <- function(names, default = NULL) {
  do.call(env_var, c(as.list(names), list(default = default)))
}

env_json <- function(..., default = NULL) {
  json <- env_var(..., default = default)
  if (is.null(json)) {
    return(NULL)
  }
  jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
}

env_chat_spec <- function(
  provider,
  model,
  default_provider = "posit",
  default_model = "zai-org/GLM-5.3-Flash"
) {
  name <- paste0(
    env_var_first(provider, default = default_provider),
    "/",
    env_var_first(model, default = default_model)
  )
  list(name = name)
}
