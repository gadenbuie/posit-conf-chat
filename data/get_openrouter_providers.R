# Fetches OpenRouter provider pricing, speed, and privacy data for a model
# to inform provider `order` choices in POSIT_CONF_API_ARGS.
# Run: Rscript data/get_openrouter_providers.R <model-slug>
# Falls back to OPENROUTER_MODEL if no argument is passed.

library(httr2)
library(jsonlite)

model_slug <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(model_slug) || !nzchar(model_slug)) {
  model_slug <- Sys.getenv("OPENROUTER_MODEL", "z-ai/glm-5.3-flash")
}
if (!nzchar(model_slug)) {
  stop("Pass a model slug as an argument or set OPENROUTER_MODEL")
}

page_url <- paste0("https://openrouter.ai/", model_slug)
endpoints_url <- paste0(
  "https://openrouter.ai/api/v1/models/",
  model_slug,
  "/endpoints"
)

html <- request(page_url) |>
  req_perform() |>
  resp_body_string()

rows <- strsplit(html, '<tr class="or-table__row', fixed = TRUE)[[1]][-1]

first_match <- function(x, pattern) {
  m <- regmatches(x, regexec(pattern, x))[[1]]
  if (length(m) < 2) NA_character_ else m[[2]]
}

cell_price <- function(cell) {
  nums <- as.numeric(regmatches(cell, gregexpr("[0-9]+\\.[0-9]+", cell))[[1]])
  if (length(nums) == 0) NA_real_ else tail(nums, 1)
}

providers <- do.call(
  rbind,
  lapply(rows, function(row) {
    cells <- strsplit(row, "<td", fixed = TRUE)[[1]]
    if (length(cells) < 5) {
      return(NULL)
    }
    data.frame(
      provider = first_match(row, "text-foreground\">([^<]+)</span>"),
      privacy = first_match(row, 'aria-label="Privacy: ([^"]+)"'),
      price_in = cell_price(cells[3]),
      price_out = cell_price(cells[4]),
      latency_s = as.numeric(first_match(
        row,
        "tabular-nums\">([0-9.]+)<span class=\"text-muted-foreground\">s<"
      )),
      tps = as.numeric(first_match(
        row,
        ">([0-9.]+)<span class=\"text-muted-foreground\"> tps"
      )),
      uptime_pct = as.numeric(first_match(
        row,
        "text-foreground\">([0-9.]+)%</span>"
      )),
      stringsAsFactors = FALSE
    )
  })
)

providers <- providers[!is.na(providers$privacy) & !is.na(providers$provider), ]

endpoints_resp <- request(endpoints_url) |>
  req_perform() |>
  resp_body_json()
endpoints <- endpoints_resp$data$endpoints

quant <- vapply(
  providers$provider,
  function(nm) {
    hit <- endpoints[vapply(
      endpoints,
      function(e) {
        grepl(
          gsub("[^a-z0-9]", "", tolower(e$provider_name)),
          gsub("[^a-z0-9]", "", tolower(nm)),
          fixed = TRUE
        )
      },
      logical(1)
    )]
    if (length(hit) == 0) NA_character_ else hit[[1]]$quantization
  },
  character(1)
)

providers$quantization <- quant

providers <- providers[
  order(
    providers$privacy != "Private",
    providers$price_out,
    -replace(providers$tps, is.na(providers$tps), 0)
  ),
]

cat(sprintf(
  "%-15s %-9s %8s %8s %7s %6s %7s  %-6s\n",
  "provider",
  "privacy",
  "in$/M",
  "out$/M",
  "lat_s",
  "tps",
  "uptime",
  "quant"
))
for (i in seq_len(nrow(providers))) {
  p <- providers[i, ]
  cat(sprintf(
    "%-15s %-9s %8.4f %8.4f %7.2f %6.0f %7.1f%%  %-6s\n",
    p$provider,
    p$privacy,
    p$price_in,
    p$price_out,
    p$latency_s,
    p$tps,
    p$uptime_pct,
    p$quantization
  ))
}

private <- providers[
  providers$privacy == "Private" & !is.na(providers$price_out),
]
cheapest <- private[private$price_out == min(private$price_out), ]
cheapest <- cheapest[order(-replace(cheapest$tps, is.na(cheapest$tps), 0)), ]

slug <- function(x) gsub("[^a-z0-9]", "", tolower(x))
order_slugs <- slug(cheapest$provider)

cat("\nCheapest private providers, throughput-sorted:\n")
cat('  "order": [', paste0('"', order_slugs, '"', collapse = ", "), "]\n")
cat(
  '  POSIT_CONF_API_ARGS=\'{"provider":{"order":[',
  paste0('"', order_slugs, '"', collapse = ", "),
  '],"data_collection":"deny"}}\'\n',
  sep = ""
)
