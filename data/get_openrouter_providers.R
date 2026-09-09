# Fetches OpenRouter provider pricing, speed, and privacy data for a model
# to inform provider `order` choices in POSIT_CONF_API_ARGS.
# Run: Rscript data/get_openrouter_providers.R <model-slug>
# Falls back to OPENROUTER_MODEL if no argument is passed.

library(httr2)
library(jsonlite, warn.conflicts = FALSE)

model_slug <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(model_slug) || !nzchar(model_slug)) {
  model_slug <- Sys.getenv("OPENROUTER_MODEL", "z-ai/glm-5.3-flash")
}
if (!nzchar(model_slug)) {
  stop("Pass a model slug as an argument or set OPENROUTER_MODEL")
}

# Providers at least this fast all feel the same in a chat UI, so past this
# point extra speed isn't worth paying for and price decides.
SPEED_GOOD_ENOUGH <- 1.5

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

# Composite score: latency hurts UX more than low TPS, so it gets 2x weight.
# Each is normalized against the median so units don't matter; missing
# latency is treated as worst-case, missing TPS as slowest.
lat_med <- median(providers$latency_s, na.rm = TRUE)
tps_med <- median(providers$tps, na.rm = TRUE)
latency_penalty <- providers$latency_s / lat_med
latency_penalty[is.na(latency_penalty)] <-
  max(providers$latency_s, na.rm = TRUE) / lat_med
tps_penalty <- tps_med / providers$tps
tps_penalty[is.na(tps_penalty)] <- tps_med / min(providers$tps, na.rm = TRUE)
providers$speed <- 2 * latency_penalty + tps_penalty

providers <- providers[
  order(
    providers$privacy != "Private",
    providers$speed,
    providers$price_out
  ),
]

cat(sprintf(
  "%-15s %-9s %8s %8s %7s %6s %7s %6s  %-6s\n",
  "provider",
  "privacy",
  "in$/M",
  "out$/M",
  "lat_s",
  "tps",
  "uptime",
  "speed",
  "quant"
))
for (i in seq_len(nrow(providers))) {
  p <- providers[i, ]
  cat(sprintf(
    "%-15s %-9s %8.4f %8.4f %7.2f %6.0f %7.1f%% %6.2f  %-6s\n",
    p$provider,
    p$privacy,
    p$price_in,
    p$price_out,
    p$latency_s,
    p$tps,
    p$uptime_pct,
    p$speed,
    p$quantization
  ))
}

private <- providers[
  providers$privacy == "Private" & !is.na(providers$price_out),
]
adequate <- private[private$speed <= SPEED_GOOD_ENOUGH, ]
if (nrow(adequate) == 0) {
  adequate <- private[private$speed == min(private$speed), ]
}
recommend <- adequate[order(adequate$price_out, adequate$speed), ]

slug <- function(x) gsub("[^a-z0-9]", "", tolower(x))
order_slugs <- slug(recommend$provider)

cat("\nRecommended private providers (good-enough speed, then cheapest):\n")
cat('  "order": [', paste0('"', order_slugs, '"', collapse = ", "), "]\n")
cat(
  '  POSIT_CONF_API_ARGS=\'{"provider":{"order":[',
  paste0('"', order_slugs, '"', collapse = ", "),
  '],"data_collection":"deny"}}\'\n',
  sep = ""
)
