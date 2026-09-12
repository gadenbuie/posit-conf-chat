# posit::conf(2026) Schedule Assistant

A Shiny chat app for exploring the posit::conf(2026) schedule. Ask about
sessions, workshops, and speakers; search the schedule with full-text
retrieval; and build a personal agenda. The app also includes live "On Now"
and "Full Schedule" views alongside the chat.

## Features

- **Schedule chat** — an [ellmer](https://ellmer.tidyverse.org/) chat agent
  with tools to search the schedule (ragnar BM25 full-text search over a
  DuckDB store), look up exact times, rooms, and tracks, see what's on now,
  and show session details.
- **My Agenda** — save sessions to a personal agenda, with conflict
  detection between overlapping sessions. The agenda is stored in the
  browser; conversation history is not persisted.
- **On Now / Full Schedule** — browsable schedule views that stay in sync
  with the agenda and share state through the URL query string.
- **Reply styles** — switch the assistant's tone (concise, friendly, pirate,
  ...), also available as chat slash commands.

## Setup

The app is an R package (`positConfChat`) plus a Shiny app at the root.
Install dependencies from the [DESCRIPTION](DESCRIPTION) with
[pak](https://pak.r-lib.org/) — including the GitHub remotes
([shinychat](https://github.com/posit-dev/shinychat/pkg-r) and
[ellmer](https://github.com/tidyverse/ellmer)):

```r
pak::local_install_dev_deps()
```

For deployment, the app uses `renv` ([renv.lock](renv.lock)).

### Environment variables

The chat client resolves its provider and model from environment variables,
falling back to Posit's provider with `zai-org/GLM-5.3-Flash`:

| Variable | Purpose |
|---|---|
| `POSIT_CONF_PROVIDER`, `POSIT_CONF_MODEL` | Provider and model for the main chat client |
| `POSIT_CONF_GREETING_PROVIDER`, `POSIT_CONF_GREETING_MODEL` | Override for the dynamic greeting client |
| `POSIT_CONF_API_ARGS`, `POSIT_CONF_GREETING_API_ARGS` | JSON of extra API args passed to the chat client |
| `APP_THEME` | Default theme, `conf` (default) or `basic`; also settable via `?theme=` |
| `GREETING_DYNAMIC` | Set to `yes` to generate the greeting from the model instead of the static greeting |

See [R/env.R](R/env.R) for the resolution order.

#### OpenRouter

```
GREETING_DYNAMIC=yes
POSIT_CONF_PROVIDER=openrouter
POSIT_CONF_MODEL=z-ai/glm-5.3-flash
POSIT_CONF_API_ARGS='{"provider":{"order":["relace","baseten","makora","coreweave","fireworks"],"data_collection":"deny"},"reasoning":{"effort":"high"}}'
POSIT_CONF_GREETING_API_ARGS='{"provider":{"order":["baseten","makora"],"data_collection":"deny"},"reasoning":{"effort":"low"}}'
OPENROUTER_API_KEY=...
```

#### OpenTelemetry

```
OTEL_TRACES_EXPORTER="http/protobuf"
OTEL_EXPORTER_OTLP_ENDPOINT="https://logfire-us.pydantic.dev"
OTEL_EXPORTER_OTLP_HEADERS="Authorization=<YOUR-WRITE-TOKEN>"
OTEL_SERVICE_NAME="posit-conf-chat"
```

### Data

The app reads from `data/ragnar.duckdb`, a prebuilt search store over the
conference schedule. The data pipeline that downloads the schedule from the
Zuddl widget on the [Sessions page](https://conf.posit.co/2026/sessions/) and
derives the CSV tables and the search store lives in [data/](data/) — see
[data/README.md](data/README.md) for the schema.

To update the data, run `make preflight` from the project root — it fetches
the latest schedule, rebuilds the search store, and regenerates the deployment
manifest. Each step is also a standalone target (`make schedule`,
`make ragnar`, `make manifest`), or can be run from R:

```r
source("data/get_schedule_zuddl.R")
source("data/posit-ragnar.R")
```

## Running

```r
shiny::runApp()
```

## Structure

- [app.R](app.R) — Shiny UI and server
- [R/](R/) — chat client, tools (`tools-*.R`), schedule views, agenda, slash commands
- [prompts/](prompts/) — system prompt, greeting, and reply styles
- [skills/](skills/) — agent skills exposed to the chat client
- [data/](data/) — schedule pipeline, derived CSVs, and the ragnar store
