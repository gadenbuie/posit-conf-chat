---
name: interview
learned-about: "interviewing the attendee"
description: Interview the attendee to learn what they work on, what they're interested in, and what they want to learn, then build a shared understanding that maps to posit::conf's talks, tracks, and workshops. Use when the attendee asks to be interviewed or "get to know me", has an empty agenda, says they don't know what to attend, gives vague or broad interests, or wants personalized recommendations. Do not use when the attendee already has a clear, specific request (a known talk, track, speaker, or time).
---

# Attendee interview

Learn who the attendee is and what they want from posit::conf(2026) by asking a few questions, then turn that shared understanding into concrete schedule recommendations.

## How the interview works

1. Open by telling the attendee you'll ask a few short questions to get to know them, and that the clickable suggestions are just shortcuts — they can always ignore them and answer in their own words. Say this once, at the start of the interview only.
2. Ask **one question per turn** (never more than two). Build each next question on their previous answers instead of running through a fixed list.
3. After 3–5 questions (sooner if the picture is already clear), stop asking and summarize what you've learned in 2–3 sentences, e.g. "So you're a data scientist who mostly works in R, you're getting into AI tooling, and you want hands-on practice." Let them correct anything, then immediately propose real sessions from the schedule that match, using the scheduling tools and offering to add picks to their agenda with `manage_agenda()`.
4. The attendee can jump ahead at any time — if they name topics or sessions themselves, skip the questions and go straight to recommendations.

## What to learn

Not all of this is essential; follow the thread of their answers:

- What they work on day to day, and their role
- Their tools: R, Python, both, or something else — and how experienced they are
- What they want to learn about or get better at
- Whether they prefer hands-on workshops, talks, or keynotes — and which days they're attending
- Any constraints: time blocks they must keep free, accessibility needs, topics they already know well

## Ground questions in the schedule

Don't ask abstract questions like "what are you interested in?" Make the options real by checking the catalog first with `search_schedule()` or `list_schedule_options()`, then anchor answers in actual content:

> "Are you more interested in AI agents — there's a whole track on agents, context, and MCP — or in building Shiny apps?"

This shows the attendee what the conference actually offers and makes it easy for them to react to concrete options.

## Suggestions

Use suggestions as multiple-choice answers to your questions. Write them as a markdown list wrapped in spans with the class "suggestion", phrased as the attendee's answer:

* <span class="suggestion">Mostly R, a little Python</span>
* <span class="suggestion">Hands-on workshops over talks</span>
* <span class="suggestion">I want to learn about AI tooling</span>

Keep each under ~90 characters and never repeat the same wording twice. Two or three options per question is enough. Never make a question feel closed: the attendee can always type their own answer, so don't add filler options like "other" or "none of these".
