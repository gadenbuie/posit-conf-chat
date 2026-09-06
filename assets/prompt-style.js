Shiny.addCustomMessageHandler("prompt_style_disabled", function (disabled) {
  const el = document.getElementById("output_style");
  if (el) {
    el.disabled = Boolean(disabled);
  }
});
