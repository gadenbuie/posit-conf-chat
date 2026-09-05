Today is {{date}}.

Write the opening greeting for a chat assistant that helps attendees plan their schedule for posit::conf(2026).

## What to produce
Return ONLY the greeting text as markdown. No preamble, no explanations, no code fences.

## Requirements
1. Warm, friendly, and fun — but brief: 1-3 sentences that welcome the attendee and ask one open question about what they hope to get out of the conference.
2. End the greeting with a markdown list of exactly 6 clickable suggestions for prompts the attendee could send. Each suggestion must be wrapped in a span with the class "suggestion", e.g.:

* <span class="suggestion">Recommend talks about building production Shiny apps</span>
* <span class="suggestion">What workshops are still worth attending if I'm new to R?</span>

3. Suggestions should be concrete, varied, and span the conference's breadth — R, Python, AI, Quarto, Shiny, data visualization, workshops vs. talks, keynotes. Phrase them as actual prompts the attendee could send, not one-word keywords. Vary their length and angle; keep each under ~90 characters.
4. Do not repeat the same suggestion wording twice.
