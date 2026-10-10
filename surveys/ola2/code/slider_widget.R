# Helper function to render the allocation slider widget.
# id: the surveydown question id (e.g. "cbc_q1", "cbc_practice")
# The boxes are labelled "Postulante A" (left) and "Postulante B" (right), like
# the columns of the conjoint table above the slider.
allocation_slider <- function(id, total_budget = 2000000) {
  step_size   <- 100000
  default_val <- total_budget / 2  # 1.000.000 — punto medio exacto con $2.000.000
  fmt_clp <- function(x) paste0("$", format(x, big.mark = ".", scientific = FALSE))

  htmltools::browsable(htmltools::tagList(
    # Slider
    shiny::div(
      style = "margin: -30px 0 16px 0;",
      sd_question(
        type    = "slider_numeric",
        id      = id,
        label   = "",
        option  = seq(0, total_budget, by = step_size),
        grid    = FALSE,
        default = default_val,
        width   = "100%"
      )
    ),

    # Allocation display boxes
    shiny::div(
      style = "display: flex; gap: 8px; justify-content: center; margin-top: 14px;",
      shiny::div(
        style = "border: 2px solid #28a745; padding: 8px 4px; border-radius: 5px; text-align: left; flex: 1; min-width: 0; background-color: #f0fff4; position: relative; overflow: hidden;",
        shiny::div(id = paste0(id, "_shade1"), style = "position: absolute; left: 0; right: 0; bottom: 0; height: 50%; background: rgba(40, 167, 69, 0.18); pointer-events: none; transition: height 120ms linear; z-index: 0;"),
        shiny::div(
          style = "position: relative; z-index: 1;",
          shiny::h4(shiny::span(id = paste0(id, "_name1"), "Postulante A", style = "color: #28a745; font-weight: 800;"), style = "margin: 0 0 4px 0; font-size: 0.9rem;"),
          shiny::h3(shiny::span(id = paste0(id, "_opt1"), fmt_clp(default_val), style = ""), style = "color: #28a745; margin: 4px 0; font-weight: 900; font-size: 1.1rem;"),
          shiny::p(shiny::span(id = paste0(id, "_opt1_pct"), "50%", style = ""), style = "color: #28a745; font-size: 14px; margin: 2px 0; font-weight: 800;")
        )
      ),
      shiny::div(
        style = "border: 2px solid #007bff; padding: 8px 4px; border-radius: 5px; text-align: right; flex: 1; min-width: 0; background-color: #f0f8ff; position: relative; overflow: hidden;",
        shiny::div(id = paste0(id, "_shade2"), style = "position: absolute; left: 0; right: 0; bottom: 0; height: 50%; background: rgba(0, 123, 255, 0.18); pointer-events: none; transition: height 120ms linear; z-index: 0;"),
        shiny::div(
          style = "position: relative; z-index: 1;",
          shiny::h4(shiny::span(id = paste0(id, "_name2"), "Postulante B", style = "color: #007bff; font-weight: 800;"), style = "margin: 0 0 4px 0; font-size: 0.9rem;"),
          shiny::h3(shiny::span(id = paste0(id, "_opt2"), fmt_clp(default_val), style = ""), style = "color: #007bff; margin: 4px 0; font-weight: 900; font-size: 1.1rem;"),
          shiny::p(shiny::span(id = paste0(id, "_opt2_pct"), "50%", style = ""), style = "color: #007bff; font-size: 14px; margin: 2px 0; font-weight: 800;")
        )
      )
    ),

    # "Prefiero no responder" (saved as <id>_nr: "99" if checked, empty
    # otherwise). Checking it greys out the slider; moving the slider unchecks
    # it. "Siguiente" stays disabled until the slider is touched or this is
    # checked (see updateCbcNextButton() in app.R).
    shiny::div(
      class = "cbc-nr",
      style = "display: flex; justify-content: center; margin-top: 10px;",
      sd_question(
        type   = "mc_multiple",
        id     = paste0(id, "_nr"),
        label  = "",
        option = c("Prefiero no responder" = "99")
      )
    ),
    shiny::p(
      id = paste0(id, "_hint"), class = "cbc-next-hint",
      "Mueva el control deslizante o marque «Prefiero no responder» para continuar."
    ),

    # Slider CSS
    shiny::tags$style(HTML(paste0(
      "#container-", id, " { margin-bottom: 2px !important; }",
      "#container-", id, " .form-group { margin-bottom: 2px !important; }",
      "#container-", id, " .irs { margin-top: 0 !important; margin-bottom: 10px !important; height: 26px !important; }",
      "#container-", id, " .irs-grid, #container-", id, " .irs-grid-text, #container-", id, " .irs-min, #container-", id, " .irs-max, #container-", id, " .irs-single { display: none !important; }",
      "#", id, " .irs-bar { background: #adb5bd !important; height: 10px !important; }",
      "#", id, " .irs-bar-edge { background: #adb5bd !important; }",
      "#", id, " .irs-line { background: #adb5bd !important; background-color: #adb5bd !important; background-image: none !important; border-color: #adb5bd !important; }",
      "#", id, "_name1 { color: #28a745 !important; font-weight: 800 !important; }",
      "#", id, "_name2 { color: #007bff !important; font-weight: 800 !important; }",
      "@media (max-width: 576px) {",
      "#container-", id, " { margin-bottom: 0 !important; }",
      "#container-", id, " .irs { margin-top: 0 !important; margin-bottom: 8px !important; height: 22px !important; }",
      "#", id, "_opt1, #", id, "_opt2 { font-size: 1.1rem !important; }",
      "#", id, "_opt1_pct, #", id, "_opt2_pct { font-size: 0.95rem !important; }",
      "}"
    ))),

    # JavaScript to update allocation display
    shiny::tags$script(htmltools::HTML(paste0(
      "(function() {",
      "  const sliderId = '", id, "';",
      "  const totalBudget = ", total_budget, ";",
      "  ",
      "  function getRawValue() {",
      "    const sliderEl = document.getElementById(sliderId);",
      "    if (!sliderEl) return totalBudget / 2;",
      "    const parse = (x) => { const n = parseInt(String(x).replace(/[^0-9]/g, ''), 10); return Number.isFinite(n) ? n : null; };",
      "    const v = parse(sliderEl.value);",
      "    if (v !== null) return Math.max(0, Math.min(totalBudget, v));",
      "    const d = parse(sliderEl.getAttribute('data-from'));",
      "    if (d !== null) return Math.max(0, Math.min(totalBudget, d));",
      "    return totalBudget / 2;",
      "  }",
      "  ",
      "  function updateAllocation() {",
      "    const rawVal = getRawValue();",
      "    const opt2 = rawVal;",
      "    const opt1 = totalBudget - rawVal;",
      "    const pct1 = Math.round(opt1 / totalBudget * 100);",
      "    const pct2 = 100 - pct1;",
      "    const fmt = (x) => '$' + Math.round(x).toLocaleString('es-ES');",
      "    const opt1El = document.getElementById('", id, "_opt1');",
      "    const opt2El = document.getElementById('", id, "_opt2');",
      "    const pct1El = document.getElementById('", id, "_opt1_pct');",
      "    const pct2El = document.getElementById('", id, "_opt2_pct');",
      "    const shade1El = document.getElementById('", id, "_shade1');",
      "    const shade2El = document.getElementById('", id, "_shade2');",
      "    if (opt1El) opt1El.textContent = fmt(opt1);",
      "    if (opt2El) opt2El.textContent = fmt(opt2);",
      "    if (pct1El) pct1El.textContent = pct1 + '%';",
      "    if (pct2El) pct2El.textContent = pct2 + '%';",
      "    if (shade1El) shade1El.style.height = pct1 + '%';",
      "    if (shade2El) shade2El.style.height = pct2 + '%';",
      "  }",
      "  ",
      "  function init() {",
      "    updateAllocation();",
      "  }",
      "  ",
      "  init();",
      "  setTimeout(init, 50);",
      "  setTimeout(init, 300);",
      "  setTimeout(init, 900);",
      "  if (document.readyState === 'loading') {",
      "    document.addEventListener('DOMContentLoaded', init);",
      "  }",
      "  window.addEventListener('load', init);",
      "  ",
      "  setInterval(init, 500);",
      "  ",
      "  function bindListeners() {",
      "    const sliderEl = document.getElementById(sliderId);",
      "    if (sliderEl && !sliderEl.dataset.allocBound) {",
      "      sliderEl.addEventListener('change', updateAllocation);",
      "      sliderEl.addEventListener('input', updateAllocation);",
      "      sliderEl.dataset.allocBound = '1';",
      "    }",
      "  }",
      "  bindListeners();",
      "  setInterval(bindListeners, 1000);",
      "})();",
      ""
    )))
  ))
}

