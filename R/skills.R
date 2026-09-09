skills_dir <- function() {
  dir <- "skills"
  if (!dir.exists(dir)) {
    stop("Skills directory not found: ", file.path(getwd(), dir), call. = FALSE)
  }
  dir
}

skills_path <- function(skill, reference = NULL) {
  if (identical(reference, "")) {
    reference <- NULL
  }
  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", skill)) {
    stop(
      "Invalid skill name: '",
      skill,
      "'. Use lowercase letters, numbers, and hyphens.",
      call. = FALSE
    )
  }
  path <- file.path(skills_dir(), skill)
  if (!is.null(reference)) {
    reference <- sub("^references/", "", reference)
    if (!grepl("^[A-Za-z0-9_-]+(\\.md)?$", reference)) {
      stop("Invalid reference file name: '", reference, "'.", call. = FALSE)
    }
    if (!grepl("\\.md$", reference)) {
      reference <- paste0(reference, ".md")
    }
    path <- file.path(path, "references", reference)
  } else {
    path <- file.path(path, "SKILL.md")
  }
  if (!file.exists(path)) {
    msg <- paste0(
      "Skill file not found: ",
      path,
      if (!is.null(reference)) {
        paste0(
          ". That reference file name may be wrong. Load the skill's own ",
          "instructions first with skill(\"",
          skill,
          "\") — no reference ",
          "argument. They list the exact reference file names."
        )
      }
    )
    stop(msg, call. = FALSE)
  }
  path
}

skills_read <- function(skill, reference = NULL) {
  sk <- frontmatter::read_front_matter(skills_path(skill, reference))
  ellmer::ContentToolResult(
    value = sk$body,
    extra = list(
      display = tool_result_display(
        title = if (!is.null(sk$data[["learned-about"]])) {
          sprintf("Learned about %s", sk$data[["learned-about"]])
        } else {
          "Learned from skill"
        },
        show_request = FALSE,
        markdown = sk$body
      )
    )
  )
}

skills_list <- function() {
  skill_dirs <- list.dirs(skills_dir(), recursive = FALSE)
  purrr::map(skill_dirs, function(dir) {
    path <- file.path(dir, "SKILL.md")
    if (!file.exists(path)) {
      return(NULL)
    }
    fm <- frontmatter::read_front_matter(path)
    references <- list.files(
      file.path(dir, "references"),
      pattern = "\\.md$",
      full.names = FALSE
    )
    list(
      name = fm$data$name %||% basename(dir),
      description = fm$data$description %||% "",
      references = references
    )
  }) |>
    purrr::compact()
}

skills_prompt <- function() {
  skills <- skills_list()
  if (length(skills) == 0) {
    return("")
  }
  lines <- c()
  for (skill in skills) {
    lines <- c(
      lines,
      paste0("### ", skill$name),
      "",
      skill$description,
      ""
    )
  }
  paste(lines, collapse = "\n")
}

skills_tool <- function() {
  ellmer::tool(
    skills_read,
    name = "skill",
    description = paste0(
      "Read the full instructions of an available skill, or one of its ",
      "reference files. ALWAYS call it WITHOUT `reference` first to load ",
      "the skill's instructions; those instructions list the reference ",
      "files and when each is relevant. Only after reading them may you ",
      "load a reference by its exact name. Never guess a reference name."
    ),
    arguments = list(
      skill = ellmer::type_string(
        "The name of the skill to read, e.g. 'conf-general-info'."
      ),
      reference = ellmer::type_string(
        paste(
          "Optional: a reference document within the skill, using the",
          "name given in the skill's instructions. Omit to read the",
          "skill's own instructions; you must load those first."
        ),
        required = FALSE
      )
    ),
    annotations = ellmer::tool_annotations(
      title = "Learning...",
      icon = bsicons::bs_icon("book")
    )
  )
}
