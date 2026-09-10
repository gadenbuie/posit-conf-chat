# posit::conf(2026) schedule data

## Source

The data is sourced from the public schedule embedded on the [posit::conf(2026) Sessions page](https://conf.posit.co/2026/sessions/).

The page uses a Zuddl schedule widget. The downloader:

1. Uses a previously discovered schedule API URL when it still returns a valid schedule.
2. Falls back to loading the Sessions page with Chromote and observing its schedule API request when the saved URL fails or becomes stale.
3. Saves the refreshed source URL for future runs.

The raw Zuddl response is preserved in `data/raw/`. It is the source of truth for all derived CSV files. Rich-text fields arrive from the API as HTML; the downloader converts them to Markdown with `pandoc`, so the original HTML is only preserved in the raw snapshots.

## Outputs

| File | Grain | Purpose |
|---|---|---|
| `data/raw/posit-conf-2026-schedule-<timestamp>.json` | One API response | Immutable raw source snapshot |
| `data/raw/posit-conf-2026-schedule-latest.json` | One API response | Most recently downloaded raw response |
| `data/raw/posit-conf-2026-schedule-source.json` | One downloader configuration | Last known working schedule endpoint and discovery metadata |
| `data/raw/posit-conf-2026-manifest-<timestamp>.json` | One download run | Provenance, counts, timestamps, source URLs, and hashes |
| `data/raw/posit-conf-2026-manifest-latest.json` | One download run | Manifest for the most recent successful run |
| `data/derived/schedule_records.csv` | One returned Zuddl schedule record | Faithful tabular form of every record in the API response |
| `data/derived/speakers.csv` | One session-speaker assignment | Speaker information and the session to which each speaker is assigned |
| `data/derived/locations.csv` | One distinct location | Deduplicated physical-location lookup |
| `data/derived/talks.csv` | One talk or keynote, enriched with parent-session data | Individual conference presentations and keynotes |
| `data/derived/talk_sessions.csv` | One parent session block | Conference sessions that group the talks in `talks.csv` |
| `data/derived/workshops.csv` | One workshop | Pre-conference workshops, both in-person and virtual |
| `data/derived/events.csv` | One non-presentation event | Meals, receptions, and other event programming |
| `data/ragnar.duckdb` | One talk, keynote, or workshop | Ragnar search store over `talks.csv` and `workshops.csv`; BM25 full-text search, no embeddings. Rebuilt by `data/posit-ragnar.R`. Query with `ragnar::ragnar_retrieve_bm25()`. |

## `schedule_records.csv`

`schedule_records.csv` is the closest CSV representation of the raw schedule API response. It includes every returned record:

* Individual talks.
* Workshops.
* Parent session blocks / tracks.
* Keynotes or other scheduled programming.
* Meals, receptions, and other non-presentation event blocks.

Important columns:

| Column | Meaning |
|---|---|
| `record_id` | Unique ID for this schedule record |
| `session_id` | Stable Zuddl ID for the session represented by the record |
| `parent_session_id` | `session_id` of the parent session block; populated for constituent talks |
| `title` | Session, talk, workshop, or event title |
| `abstract` | Description or abstract in Markdown, converted from source HTML by pandoc, when supplied |
| `start_time_utc`, `end_time_utc` | Raw schedule times parsed in UTC |
| `start_time_event_local`, `end_time_event_local` | The same times rendered in the conference time zone |
| `session_type` | Broad display type, such as `Virtual` or `In-person` |
| `session_format` | Delivery format, such as `HYBRID`, `VIRTUAL`, or `IN_PERSON` |
| `venue`, `sub_venue` | Zuddl venue fields, especially useful for parent session blocks |
| `location_name` | Physical room name, when supplied |
| `location_id` | Stable Zuddl ID for the room/location |
| `status` | Zuddl schedule status |

## Classification

Every record in the API response is classified into one of five `presentation_kind` values, and the four topic tables partition the schedule:

| `presentation_kind` | Table | Records |
|---|---|---|
| `talk` | `talks.csv` | Child records: individual talks inside a parent session block |
| `keynote` | `talks.csv` | Records whose title starts with `Keynote` |
| `talk_session` | `talk_sessions.csv` | Stage session blocks on conference days, including blocks with no child talks yet |
| `workshop` | `workshops.csv` | Pre-conference-day records that are virtual or run three hours or longer |
| `event` | `events.csv` | Everything else: meals, receptions, and other non-presentation programming |

## `talks.csv`

`talks.csv` starts with the `schedule_records.csv` columns, then joins each talk to its parent session block.

A parent session block typically provides the conference-level context for multiple individual talks: its track name, shared room, and session time. Child talks typically provide the title, abstract, speaker list, and their own shorter time interval.

For example:

| Record type | Title | Parent session | Time |
|---|---|---|---|
| Parent session block | `Agents, context, MCP` | — | 1:00–2:20 PM |
| Individual talk | `What context do your AI agents actually need?` | `Agents, context, MCP` | 1:00–1:20 PM |
| Individual talk | `AI Agents Deserve R` | `Agents, context, MCP` | 1:20–1:40 PM |

Additional parent-derived columns include:

| Column | Meaning |
|---|---|
| `track_title` | Title of the parent session block |
| `track_start_time_utc`, `track_end_time_utc` | Parent session block timing in UTC |
| `track_start_time_event_local`, `track_end_time_event_local` | Parent session block timing in conference-local time |
| `track_session_format` | Parent session block format |
| `track_venue`, `track_sub_venue` | Parent session block venue fields |
| `track_location_id`, `track_location_name` | Parent session block room/location |
| `effective_location_name` | Record-level room when available; otherwise inherited from the parent session block |
| `effective_session_format` | Record-level format when available; otherwise inherited from the parent session block |
| `kind` | `talk` for individual track talks, `keynote` for keynotes (which have no parent session block) |

Use `talks.csv` for most talk-oriented analysis. Join `speakers.csv` on `record_id` to attach speakers, and `talk_sessions.csv` on `parent_session_id` = `session_id` to attach the full session row.

## `talk_sessions.csv`

`talk_sessions.csv` contains one row per conference session block, with the columns needed to place it on the schedule: title, abstract (usually empty; most blocks leave descriptions to their child talks), timing, format, stage venue, and room.

| Column | Meaning |
|---|---|
| `talk_count` | Number of child talks currently in `talks.csv`; `0` for sessions whose talks have not been added yet |

## `workshops.csv`

`workshops.csv` contains the pre-conference workshops. Both in-person all-day trainings and shorter virtual workshops appear here, distinguished by `session_format` (`IN_PERSON` or `VIRTUAL`). Speakers attach via `speakers.csv` on `record_id`.

## `events.csv`

`events.csv` contains non-presentation programming: lunches, receptions, and evening events. Speakers attach via `speakers.csv` on `record_id` where supplied.

Use `schedule_records.csv` when an exact representation of the source schedule matters, including the raw `presentation_kind` classification for every record.

## `speakers.csv`

`speakers.csv` is normalized so that each row connects one speaker to one session.

| Column | Meaning |
|---|---|
| `record_id`, `session_id` | Join keys to `schedule_records.csv` or `presentations.csv` |
| `speaker_id` | Stable Zuddl speaker ID |
| `account_id` | Zuddl account ID, when supplied |
| `speaker_order` | Display order within the session |
| `first_name`, `last_name`, `full_name` | Speaker name |
| `title_affiliation` | Role, organization, or affiliation supplied by Zuddl |
| `biography` | Speaker biography in Markdown, converted from source HTML by pandoc, when supplied |
| `image_url` | Speaker profile-image URL, when supplied |
| `linkedin_url` | LinkedIn profile URL, when supplied |

A presentation with multiple speakers appears once in `presentations.csv` but has multiple matching rows in `speakers.csv`.

## `locations.csv`

`locations.csv` deduplicates the physical location fields found in `schedule_records.csv`.

| Column | Meaning |
|---|---|
| `location_id` | Stable Zuddl location ID |
| `location_name` | Room name |
| `location_city`, `location_state`, `location_country` | Location address fields, when supplied |
| `location_map_link` | Map URL, when supplied |

## Time zones

The raw source returns ISO 8601 UTC timestamps. The downloader also writes event-local display times using `America/Indiana/Knox`, the time zone identified in the event metadata. In September, this corresponds to Central Daylight Time.

Use UTC columns for computation and joins across time zones. Use event-local columns for schedule display and human-facing analysis.

## Data limitations

* The source is a public schedule-widget API, not a documented public export API.
* The program may change between downloads. Retain the timestamped raw snapshots when reproducibility matters.
* Some parent session blocks intentionally have no abstract or speakers; their individual child talks contain that information.
* Some virtual sessions do not have a physical location.
* `sponsors` and `files` are present in the raw API response but were empty when this dataset structure was created.
* The derived `presentation_kind` field is heuristic. It is suitable for practical filtering but does not replace preserving or inspecting the raw Zuddl fields.
