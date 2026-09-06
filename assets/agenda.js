const AGENDA_KEY = "posit-conf-agenda";

$(document).on("shiny:connected", () => {
  let ids = [];
  try {
    ids = JSON.parse(localStorage.getItem(AGENDA_KEY)) || [];
  } catch (e) {}
  Shiny.setInputValue("agenda_restore", ids, { priority: "event" });
});

Shiny.addCustomMessageHandler("agenda_save", (ids) => {
  localStorage.setItem(AGENDA_KEY, JSON.stringify(ids || []));
});
