Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(surveydown)

# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak(c(
#   'surveydown-dev/surveydown', # Development version from GitHub
#   'here',
#   'glue',
#   'readr',
#   'dplyr',
#   'kableExtra',
#   'tidyr',
#   'waiter'
# ))

# Load packages
library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(here)
library(kableExtra)
library(tidyr)
library(waiter)

# Read in the full survey design file
design <- read_csv("data/choice_questions.csv")

# Database setup --------------------------------------------------------------
#

# Configure credentials once with:
# surveydown::sd_db_config()
#
# In production (default), responses are written to the configured database.
# For local UI tests without writes, set SD_IGNORE_DB=true.
ignore_db <- tolower(Sys.getenv("SD_IGNORE_DB", "false")) %in% c("1", "true", "yes")
db <- sd_db_connect(ignore = ignore_db, gssencmode = "disable")

# UI setup --------------------------------------------------------------------

# surveydown's Lua filter always embeds the full Font Awesome library (CSS
# with its icon fonts + JS: ~5.4 MB of the ~7.6 MB page head), even though
# this survey uses no icons. Strip both blocks from the head that sd_ui()
# returns, cutting the page every participant downloads from ~3.5 MB to
# ~0.85 MB gzipped. shiny::icon() would still work: it brings its own copy.
sd_ui_no_font_awesome <- function() {
  ui <- sd_ui()
  head_html <- ui[[1]]$children[[1]]
  # Leave the UI untouched if surveydown ever changes this structure
  if (!inherits(head_html, "html")) return(ui)
  head_html <- gsub(
    "(?s)<style[^>]*>[[:space:]]*[.]fa[{]font-family:var[(]--fa-style-family.*?</style>",
    "", head_html, perl = TRUE
  )
  head_html <- gsub(
    "(?s)<script[^>]*>[[:space:]]*/[*]![[:space:]]*[*] Font Awesome Free.*?</script>",
    "", head_html, perl = TRUE
  )
  ui[[1]]$children[[1]] <- HTML(head_html)
  ui
}

ui <- tagList(
  useShinyjs(),
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      spin_fading_circles(),
      tags$p("Cargando la encuesta", style = "color: #ffffff; margin-top: 20px;")
    ),
    color = "#333e48"
  ),
  sd_ui_no_font_awesome(),
  tags$head(
    tags$script(HTML("
      function parsePctValue(x) {
        if (x === null || x === undefined) return null;
        var n = parseInt(String(x).replace('%', '').trim(), 10);
        if (!Number.isFinite(n)) return null;
        return Math.max(0, Math.min(100, n));
      }

      function parseMoneyValue(txt) {
        if (!txt) return null;
        var digits = String(txt).replace(/[^0-9]/g, '');
        if (!digits) return null;
        var n = parseInt(digits, 10);
        return Number.isFinite(n) ? n : null;
      }

      function getSliderPct(sliderId) {
        var sliderEl = document.getElementById(sliderId);
        if (!sliderEl) return null;

        var rawVal = parseInt(String(sliderEl.value).replace(/[^0-9]/g, ''), 10);
        if (!Number.isFinite(rawVal)) {
          var dataFrom = parseInt(String(sliderEl.getAttribute('data-from') || '').replace(/[^0-9]/g, ''), 10);
          if (!Number.isFinite(dataFrom)) return null;
          rawVal = dataFrom;
        }

        // Slider en pesos directos (rango > 100): convertir a porcentaje
        var sliderMax = parseInt(sliderEl.getAttribute('data-max') || '100', 10);
        if (sliderMax > 100) {
          var total = getTotalBudget(sliderId);
          return Math.round(rawVal / total * 100);
        }
        return Math.max(0, Math.min(100, rawVal));
      }

      function getTotalBudget(sliderId) {
        var o1 = document.getElementById(sliderId + '_opt1');
        var o2 = document.getElementById(sliderId + '_opt2');
        var v1 = parseMoneyValue(o1 ? o1.textContent : null);
        var v2 = parseMoneyValue(o2 ? o2.textContent : null);
        if (v1 !== null && v2 !== null && (v1 + v2) > 0) return v1 + v2;
        return 2000000;
      }

      function formatMoneyCLP(n) {
        return '$' + Math.round(n).toLocaleString('es-ES');
      }

      function applyCBCAllocationToDom() {
        var sliders = document.querySelectorAll('input[id^=cbc_]');
        sliders.forEach(function(sliderEl) {
          var sliderId = sliderEl.id;
          var sliderPct = getSliderPct(sliderId);
          if (sliderPct === null) return;

          // Slider invertido: mover derecha = más para opt2 (derecha/azul)
          var pct2 = sliderPct;
          var pct1 = 100 - pct2;
          var totalBudget = getTotalBudget(sliderId);
          var opt1 = totalBudget * (pct1 / 100);
          var opt2 = totalBudget * (pct2 / 100);

          var opt1El = document.getElementById(sliderId + '_opt1');
          var opt2El = document.getElementById(sliderId + '_opt2');
          var pct1El = document.getElementById(sliderId + '_opt1_pct');
          var pct2El = document.getElementById(sliderId + '_opt2_pct');
          var shade1El = document.getElementById(sliderId + '_shade1');
          var shade2El = document.getElementById(sliderId + '_shade2');

          if (opt1El) opt1El.textContent = formatMoneyCLP(opt1);
          if (opt2El) opt2El.textContent = formatMoneyCLP(opt2);
          if (pct1El) pct1El.textContent = pct1 + '%';
          if (pct2El) pct2El.textContent = pct2 + '%';
          if (shade1El) shade1El.style.height = pct1 + '%';
          if (shade2El) shade2El.style.height = pct2 + '%';
        });
      }

      // Keep allocations synced regardless of widget script timing
      setInterval(applyCBCAllocationToDom, 200);
      document.addEventListener('input', applyCBCAllocationToDom);
      document.addEventListener('change', applyCBCAllocationToDom);
      document.addEventListener('click', function() {
        setTimeout(applyCBCAllocationToDom, 0);
        setTimeout(applyCBCAllocationToDom, 120);
      });

      // Sliders with a 'Prefiero no responder' box (<id>_nr: the conjoint's
      // cbc_* sliders, dec_1..dec_3, estatus_subjetivo, posicion_politica):
      // 'Siguiente' stays disabled until the respondent touches every visible
      // one (a click on the handle counts, so the initial value can be kept)
      // or checks its 'Prefiero no responder'. Checking it greys out the
      // slider; touching the slider unchecks it.
      window.sliderTouched = window.sliderTouched || {};

      function sliderNrBox(id) {
        return document.querySelector('input[name=\"' + id + '_nr\"]');
      }

      function sliderIsAnswered(id) {
        var nr = sliderNrBox(id);
        // A non-empty _hist means the slider was moved before (restored when
        // going back or resuming the survey)
        var hist = document.getElementById(id + '_hist');
        return !!window.sliderTouched[id] || (nr && nr.checked) ||
          (hist && hist.value !== '');
      }

      function sliderTouch(id) {
        window.sliderTouched[id] = true;
        var nr = sliderNrBox(id);
        if (nr && nr.checked) {
          nr.checked = false;
          $(nr).trigger('change');
        }
        updateSliderNextButton();
      }

      // Capture phase: ion.rangeSlider calls stopPropagation() on mousedown
      // over the handle/track, so a bubbling listener would never see it
      function sliderFromEvent(e) {
        if (!e.target.closest) return null;
        var irs = e.target.closest('.irs');
        var container = irs && irs.closest('.question-container');
        var id = container && container.getAttribute('data-question-id');
        return id && sliderNrBox(id) ? id : null;
      }
      ['mousedown', 'touchstart'].forEach(function(type) {
        document.addEventListener(type, function(e) {
          var id = sliderFromEvent(e);
          if (id) sliderTouch(id);
        }, true);
      });
      document.addEventListener('keydown', function(e) {
        var id = sliderFromEvent(e);
        if (id && e.keyCode >= 37 && e.keyCode <= 40) sliderTouch(id);
      }, true);
      $(document).on('change', 'input[type=\"checkbox\"][name$=\"_nr\"]', function() {
        updateSliderNextButton();
      });

      function updateSliderNextButton() {
        var sliders = Array.from(document.querySelectorAll('input.js-range-slider'))
          .filter(function(s) { return !!sliderNrBox(s.id); });
        if (sliders.length === 0) return;  // no slider with 'Prefiero no responder'
        var allAnswered = true;
        sliders.forEach(function(s) {
          var answered = sliderIsAnswered(s.id);
          var nr = sliderNrBox(s.id);
          var container = document.getElementById('container-' + s.id);
          if (container) container.classList.toggle('slider-nr-active', !!(nr && nr.checked));
          var hint = document.getElementById(s.id + '_hint');
          if (hint) hint.style.display = answered ? 'none' : '';
          // Hidden sliders (dec_2 while it does not apply) don't block
          var visible = container && container.offsetParent !== null;
          if (visible && !answered) allAnswered = false;
        });
        document.querySelectorAll('.sd-nav-next').forEach(function(btn) {
          btn.disabled = !allAnswered;
        });
      }

      setInterval(updateSliderNextButton, 200);

      // Transition page: 'transicion_ancla' is referenced in sd_skip_if(),
      // which makes surveydown treat it as required, so fill it in
      // automatically ('visto') as soon as the page shows up.
      setInterval(function() {
        var anchor = document.getElementById('transicion_ancla');
        if (anchor && anchor.value === '') $(anchor).val('visto').trigger('change');
      }, 200);
    ")),
    tags$style(HTML("
      /* Flatly theme renders body text at 17px (root font-size); bump it +3px.
         Headers stay rem-based off the root font-size, so they're unaffected. */
      body {
        font-size: 20px;
      }
      /* surveydown/Bootstrap set these in em/rem relative to the *root*
         font-size (still 17px), not body, so they drift from the 20px
         body text above. Pin them to 20px so question text, answer
         options, free-text inputs and the ranking matrix all match. */
      .control-label, .radio label, .checkbox label,
      .form-control,
      .matrix-question, .matrix-question th {
        font-size: 20px;
      }
      /* kableExtra's conjoint profile table ships its own font-size;
         override it so it matches the rest of the question text too. */
      .cell-output-display table.table-condensed td,
      .cell-output-display table.table-condensed th {
        font-size: 20px !important;
      }
      /* Quarto callouts (e.g. cbc_practice-page) render their body at a
         smaller, em-based font-size; pin it to match body-font (20px). */
      .callout-body-container,
      .callout-body-container p,
      .callout-body-container li {
        font-size: 20px !important;
      }
      /* Slider track: neutral grey, sin colores que confundan */
      .irs-bar, .irs-bar--single, .irs-bar-edge,
      .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge {
        background-color: #adb5bd !important;
        background: #adb5bd !important;
        border-top-color: #adb5bd !important;
        border-bottom-color: #adb5bd !important;
        height: 8px !important;
        border: none !important;
      }
      .irs-line,
      .irs--shiny .irs-line {
        background-color: #adb5bd !important;
        background: #adb5bd !important;
        border-color: #adb5bd !important;
        background-image: none !important;
        height: 8px !important;
      }
      .irs--shiny .irs-line::before {
        background: transparent !important;
      }
      /* Highlight slider thumb in yellow */
      .irs--shiny .irs-handle,
      .irs--shiny .irs-handle:hover,
      .irs--shiny .irs-handle.state_hover,
      .irs--shiny .irs-handle.from,
      .irs--shiny .irs-handle.to {
        border: 2px solid #e0a800 !important;
        background: #ffd43b !important;
        box-shadow: 0 0 0 2px rgba(255, 212, 59, 0.28) !important;
      }
      .irs--shiny .irs-handle > i:first-child {
        background: #ffd43b !important;
      }
      /* Compact slider vertical footprint */
      .irs,
      .irs--shiny {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      .question-container,
      .question-container .form-group,
      .form-group.shiny-input-container {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      /* Force compact layout for conjoint sliders (cbc_*) */
      .question-container[data-question-id^='cbc_'] .form-group,
      .question-container[id^='container-cbc_'] .form-group {
        padding: 0 !important;
        margin: 0 !important;
        border: 0 !important;
        background: transparent !important;
        box-shadow: none !important;
      }
      .question-container[data-question-id^='cbc_'] .irs,
      .question-container[id^='container-cbc_'] .irs {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        height: 22px !important;
      }
      .question-container[data-question-id^='cbc_'] .irs-grid,
      .question-container[data-question-id^='cbc_'] .irs-grid-text,
      .question-container[data-question-id^='cbc_'] .irs-min,
      .question-container[data-question-id^='cbc_'] .irs-max,
      .question-container[data-question-id^='cbc_'] .irs-single,
      .question-container[id^='container-cbc_'] .irs-grid,
      .question-container[id^='container-cbc_'] .irs-grid-text,
      .question-container[id^='container-cbc_'] .irs-min,
      .question-container[id^='container-cbc_'] .irs-max,
      .question-container[id^='container-cbc_'] .irs-single {
        display: none !important;
      }
      /* Hide current-value tooltip and numeric boundary labels on all sliders */
      .irs-single, .irs-min, .irs-max {
        display: none !important;
      }
      /* dec_2 (and its 'Prefiero no responder' box) hidden until dec_1 is
         answered with a value other than 0 */
      .question-container[data-question-id='dec_2'],
      #dec_2_nr_block {
        display: none;
      }
      /* *_hist (dec_1_hist, cbc_q1_hist..cbc_q6_hist) only store their
         slider's trajectory (filled from the server) */
      .question-container[data-question-id$='_hist'] {
        display: none;
      }
      /* Transition slide between the conjoint and the rest of the survey
         (page 'transicion'): centered text and 'Continuar' button on the
         usual page background. Its 'transicion_ancla' question is only a
         hidden anchor for the form A/B routing in sd_skip_if(). */
      .question-container[data-question-id='transicion_ancla'] {
        display: none;
      }
      .transicion-slide {
        min-height: 60vh;
        padding: 48px 32px;
        margin-top: 24px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        text-align: center;
      }
      .transicion-slide h2 {
        margin-bottom: 24px;
      }
      .transicion-slide p {
        max-width: 640px;
        margin-left: auto;
        margin-right: auto;
      }
      .transicion-slide .sd-nav-container {
        display: flex;
        justify-content: center;
        margin-top: 32px;
      }
      .transicion-slide .sd-nav-next {
        float: none !important;
      }
      /* Sliders' 'Prefiero no responder' (see updateSliderNextButton()):
         greyed-out slider while checked, grey 'Siguiente' until answered */
      .slider-nr-active .irs {
        opacity: 0.35;
        filter: grayscale(1);
      }
      .slider-nr .question-container,
      .slider-nr .form-group {
        width: 100% !important;
      }
      /* Plain checkbox under its slider, not a separate question card (no
         card background/border or unanswered highlight, no empty label) */
      .slider-nr .form-group.shiny-input-container {
        padding: 0 !important;
        margin: 0 !important;
        border: 0 !important;
        background: transparent !important;
        box-shadow: none !important;
      }
      .slider-nr .control-label:empty {
        display: none;
      }
      .slider-nr .shiny-options-group {
        display: flex;
        justify-content: center;
      }
      .slider-nr .checkbox label {
        font-size: 17px;
        color: #6c757d;
      }
      .slider-next-hint {
        text-align: center;
        font-size: 15px;
        color: #6c757d;
        margin: 6px 0 0 0;
      }
      .sd-nav-next:disabled {
        background-color: #adb5bd !important;
        border-color: #adb5bd !important;
        color: #ffffff !important;
        cursor: not-allowed;
        opacity: 1;
      }
      /* No sé/Prefiero no responder are always coded as value=98/99
         across the survey's mc questions - set them apart from the
         substantive scale above them with extra spacing. */
      .shiny-options-group .radio:has(input[value='98']) {
        margin-top: 18px !important;
      }
      /* dec_1/dec_2/dec_3 sliders: bump the grid labels (numbers/percentages)
         +3px over the ionRangeSlider default (9px) so they're easier to read. */
      .question-container[data-question-id='dec_1'] .irs-grid-text,
      .question-container[data-question-id='dec_2'] .irs-grid-text,
      .question-container[data-question-id='dec_3'] .irs-grid-text {
        font-size: 12px !important;
        line-height: 12px !important;
      }
      /* Match conjoint table option columns with option box colors */
      .cell-output-display table.table.table-condensed > tbody > tr > td:nth-child(2),
      .cell-output-display table.table.table-condensed > thead > tr > th:nth-child(2),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:nth-of-type(odd) > td:nth-child(2),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:nth-of-type(even) > td:nth-child(2),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:hover > td:nth-child(2) {
        background-color: #f0fff4 !important;
        box-shadow: inset 0 0 0 9999px #f0fff4 !important;
        color: #0b3d1a !important;
      }

      .cell-output-display table.table.table-condensed > tbody > tr > td:nth-child(3),
      .cell-output-display table.table.table-condensed > thead > tr > th:nth-child(3),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:nth-of-type(odd) > td:nth-child(3),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:nth-of-type(even) > td:nth-child(3),
      .cell-output-display table.table-striped.table-hover.table-condensed > tbody > tr:hover > td:nth-child(3) {
        background-color: #d9ecff !important;
        box-shadow: inset 0 0 0 9999px #d9ecff !important;
        color: #001f4d !important;
      }

      /* Vertical lines separating the conjoint table columns (attribute
         labels | Postulante A | Postulante B) */
      .cell-output-display table.table.table-condensed > tbody > tr > td:nth-child(2),
      .cell-output-display table.table.table-condensed > tbody > tr > td:nth-child(3),
      .cell-output-display table.table.table-condensed > thead > tr > th:nth-child(2),
      .cell-output-display table.table.table-condensed > thead > tr > th:nth-child(3) {
        border-left: 2px solid #868e96 !important;
      }
    ")),
    tags$script(HTML("
      function localizeNavButtons() {
        document.querySelectorAll('.sd-nav-prev').forEach(function(btn) {
          if (btn.textContent.trim() === 'Previous' || btn.textContent.trim() === 'Anterior') {
            btn.textContent = 'Anterior';
          }
        });
        document.querySelectorAll('.sd-nav-next').forEach(function(btn) {
          if (btn.textContent.trim() === 'Next' || btn.textContent.trim() === 'Siguiente') {
            btn.textContent = 'Siguiente';
          }
        });
      }

      function paintSliderFill() {
        document.querySelectorAll('input.js-range-slider').forEach(function(inputEl) {
          var min = parseFloat(inputEl.getAttribute('data-min'));
          var max = parseFloat(inputEl.getAttribute('data-max'));
          var val = parseFloat(inputEl.value);
          if (isNaN(min) || isNaN(max) || isNaN(val) || max <= min) return;

          var pct = ((val - min) / (max - min)) * 100;
          pct = Math.max(0, Math.min(100, pct));

          var sliderWrap = inputEl.previousElementSibling;
          if (!sliderWrap) return;
          var bar = sliderWrap.querySelector('.irs-bar');
          if (!bar) return;

          bar.style.left = '0%';
          bar.style.width = pct + '%';
          bar.style.background = '#adb5bd';
          bar.style.backgroundColor = '#adb5bd';
        });
      }

      function applyUiPolish() {
        localizeNavButtons();
        paintSliderFill();
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', applyUiPolish);
      } else {
        applyUiPolish();
      }

      if (window.$) {
        $(document).on('shiny:inputBinding:bound', function() {
          setTimeout(applyUiPolish, 80);
        });
      }

      document.addEventListener('input', applyUiPolish);
      document.addEventListener('change', applyUiPolish);
      document.addEventListener('click', function() {
        setTimeout(applyUiPolish, 0);
        setTimeout(applyUiPolish, 100);
      });
    "))
  )
)

# Helper functions ------------------------------------------------------------

# Column headers of the conjoint tables, in the colors of the slider boxes
# (altID 1 = left/green, altID 2 = right/blue)
alt_headers <- c(
  "<span style='color: #28a745 !important; font-weight: 800;'>Postulante A</span>",
  "<span style='color: #007bff !important; font-weight: 800;'>Postulante B</span>"
)

make_cbc_table <- function(df, attr_order = NULL) {
  alt_ids <- sort(unique(df$altID))

  has_custom_cols <- all(c("sex", "need", "identity", "control", "effort", "reciprocity", "attitude") %in% names(df))

  attrs_by_alt <- NULL

  if (has_custom_cols) {
    # Snapshot of what's actually shown to the respondent for this task,
    # keyed by altID. Needed to store the presented profiles alongside the
    # slider answer, since the design is randomized per-session and can't be
    # reconstructed afterwards.
    attr_cols <- c("sex", "need", "identity", "control", "effort", "reciprocity", "attitude")
    df_by_alt <- df[match(alt_ids, df$altID), ]
    attrs_by_alt <- stats::setNames(
      lapply(seq_along(alt_ids), function(i) as.list(df_by_alt[i, attr_cols])),
      as.character(alt_ids)
    )

    attr_labels <- c(
      sex         = "Sexo:",
      need        = "Su hogar llega a fin de mes con:",
      identity    = "País de nacimiento:",
      control     = "Requiere la beca porque:",
      effort      = "Estudia:",
      reciprocity = "Fuera de sus estudios:",
      attitude    = "Ve la beca como:"
    )
    ordered <- if (!is.null(attr_order)) attr_order else names(attr_labels)

    alts <- df |>
      arrange(altID) |>
      select(!!!setNames(rlang::syms(ordered), attr_labels[ordered]))
  } else {
    # Backward-compatible rendering for the original apple template
    alts <- df |>
      arrange(altID) |>
      mutate(
        price = paste(scales::dollar(price), "/ lb"),
        image = paste0('<img src="', image, '" width=100>')
      ) |>
      select(
        ` ` = image,
        `Price:` = price,
        `Type:` = type,
        `Freshness:` = freshness
      )
  }

  row.names(alts) <- NULL # Drop row names

  # One column per applicant, headed "Postulante A" / "Postulante B"
  profiles <- t(alts)
  colnames(profiles) <- alt_headers[seq_len(ncol(profiles))]

  table <- kbl(profiles, escape = FALSE) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center",
      font_size = 15
    ) |>
    column_spec(1, bold = TRUE)

  list(
    render = function() { shiny::HTML(as.character(table)) },
    attrs = attrs_by_alt
  )
}

# Levels of each conjoint attribute. A level's position in its vector is its
# number in cbc_attr_order_levels (e.g. identity: 1 = Chile, 2 = Venezuela,
# 3 = Perú), so don't reorder them without updating the documentation.
cbc_levels <- list(
  sex = c(
    "Hombre",
    "Mujer"
  ),
  need = c(
    "Dificultad",
    "Holgura"
  ),
  identity = c(
    "Chile",
    "Venezuela",
    "Perú"
  ),
  control = c(
    "Postuló a otras becas pero no obtuvo financiamiento",
    "No alcanzó a postular a tiempo a otras becas"
  ),
  effort = c(
    "Más que sus compañeros",
    "Igual que sus compañeros",
    "Menos que sus compañeros"
  ),
  reciprocity = c(
    "Ha hecho voluntariado",
    "No ha hecho voluntariado"
  ),
  attitude = c(
    "Una ayuda que agradece",
    "Algo que se merece"
  )
)

build_default_conjoint_design <- function(resp_id, n_questions = 6, min_diff = 2) {
  niveles <- cbc_levels

  # Generate a pair of profiles that differ in at least min_diff of the six
  # substantive attributes. Sex is drawn independently (p = 0.5) like the
  # rest but does not count toward min_diff.
  substantive <- setdiff(names(niveles), "sex")
  generate_pair <- function() {
    repeat {
      alt1 <- sapply(niveles, function(lvls) sample(lvls, 1))
      alt2 <- sapply(niveles, function(lvls) sample(lvls, 1))
      if (sum(alt1[substantive] != alt2[substantive]) >= min_diff) return(list(alt1 = alt1, alt2 = alt2))
    }
  }

  rows <- lapply(seq_len(n_questions), function(q) {
    pair <- generate_pair()
    data.frame(
      profileID   = NA_integer_,
      respID      = resp_id,
      qID         = q,
      altID       = 1:2,
      obsID       = q,
      sex         = c(pair$alt1[["sex"]],         pair$alt2[["sex"]]),
      need        = c(pair$alt1[["need"]],        pair$alt2[["need"]]),
      identity    = c(pair$alt1[["identity"]],    pair$alt2[["identity"]]),
      control     = c(pair$alt1[["control"]],     pair$alt2[["control"]]),
      effort      = c(pair$alt1[["effort"]],      pair$alt2[["effort"]]),
      reciprocity = c(pair$alt1[["reciprocity"]], pair$alt2[["reciprocity"]]),
      attitude    = c(pair$alt1[["attitude"]],    pair$alt2[["attitude"]]),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  result$profileID <- seq_len(nrow(result))
  result[, c("profileID", "respID", "qID", "altID", "obsID",
             "sex", "need", "identity", "control", "effort", "reciprocity", "attitude")]
}

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)
  sd_store_value(completion_code)

  # Identificador del participante que viene en el enlace de la encuesta
  # (ej. https://.../?id_user=1). sd_get_url_pars() es un reactive, asi que
  # solo puede leerse dentro de un contexto reactivo: observe() corre una vez
  # que el clientData de la sesion (la URL) esta disponible.
  observe({
    pars <- sd_get_url_pars("id_user")
    id_user <- if (is.null(pars$id_user)) "" else pars$id_user
    sd_store_value(id_user, "id_user")
  })

  # Sample a random respondentID and store it in your data
  respondentID <- sample(design$respID, 1)
  sd_store_value(respondentID, "respID")

  # Filter for the rows for the chosen respondentID
  df <- design |>
    filter(respID == respondentID)

  # match the new survey content.
  if (!all(c("rendimiento", "situacion_hogar", "educ_padres") %in% names(df))) {
    df <- build_default_conjoint_design(respondentID, n_questions = 6)
  }

  # Repeated task for measuring intra-respondent reliability (Clayton,
  # Horiuchi, Kaufman, King & Komisarchik 2026): the practice task shows this
  # respondent's task-6 profiles (randomized like every other task, so the
  # practice task differs across respondents), and task 6 shows the same two
  # profiles again with their sides swapped (altID 1 <-> 2), so a consistent
  # answer reflects the profiles rather than the side of the screen.
  practice_df <- df |>
    filter(qID == 6) |>
    mutate(qID = 0L, obsID = 0L)
  df <- df |>
    mutate(altID = ifelse(qID == 6, 3L - altID, altID)) |>
    arrange(qID, altID)

  # Random attribute order fixed for this respondent across the practice task
  # and all 6 questions. Sex is shuffled together with the other six
  # attributes, so it can land in any of the 7 rows.
  attr_order <- sample(c("sex", "need", "identity", "control", "effort", "reciprocity", "attitude"))

  # Persist the display order so attribute-order effects (Hainmueller et al.
  # 2014, sec. 5.3.4) can be diagnosed later. Stored as a 7-character code
  # (Sex, Need, Identity, Control, Effort, Reciprocity, Attitude) - the
  # letters are unique across the seven rows, so e.g. "EANSIRC" means the
  # respondent saw effort first, sex fourth and control last. The order is
  # not otherwise recoverable: cbc_profiles always serializes attributes in
  # a fixed canonical order regardless of what was displayed.
  attr_code <- c(sex = "S", need = "N", identity = "I", control = "C",
                 effort = "E", reciprocity = "R", attitude = "A")
  sd_store_value(paste(attr_code[attr_order], collapse = ""), "cbc_attr_order")

  # Asignación aleatoria a una de dos formas de la encuesta. Ambas parten
  # igual (welcome -> demographics -> conjoint -> transicion) y terminan igual
  # (id-final -> end_normal); entre medio difieren en el orden de los módulos:
  #   A: Meritocracia -> Actitudes + Percepción -> Merecimiento
  #   B: Merecimiento -> Actitudes + Percepción -> Meritocracia
  # donde Meritocracia = merit-1..merit-2, Actitudes + Percepción =
  # clasicos..lucro y Merecimiento = deserve-1..deserve-7. El ruteo está en
  # sd_skip_if() más abajo.
  version_encuesta <- sample(c("A", "B"), 1)
  sd_store_value(version_encuesta)
  # Si la persona retoma la encuesta (cookies), sd_store_value() conserva la
  # forma ya guardada en la base y descarta el nuevo sorteo; se usa esa forma
  # para rutear, si no quien retoma podría cambiar de forma a mitad de camino
  # y saltarse o repetir módulos.
  version_guardada <- session$userData$deferred_values$version_encuesta
  if (!is.null(version_guardada)) version_encuesta <- version_guardada

  # Practice task: rendered here (not in survey.qmd) because its profiles are
  # drawn per respondent (see above).
  practice_table <- make_cbc_table(practice_df, attr_order = attr_order)
  output$cbc_practice_table <- practice_table$render

  # Create the options for each choice question (using the helper function above)
  tables <- lapply(1:6, function(q) make_cbc_table(df |> filter(qID == q), attr_order = attr_order))
  for (q in 1:6) {
    local({
      tbl <- tables[[q]]
      output[[paste0("cbc", q, "_table")]] <- tbl$render
    })
  }

  # Persist the attributes shown for each alternative in every cbc
  # task, so the slider allocation can later be analyzed against what was
  # actually presented. The design is randomized per-session (see
  # build_default_conjoint_design() above), so without this the profiles
  # behind each answer are lost once the session ends.
  #
  # A single sd_store_value() call for all 6 tasks: every call does at least
  # one DB/CSV lookup internally (to support cookie-resume), so calling it
  # once per attribute (84 calls) added ~3s per session locally and, against
  # a real remote DB, was slow enough to make the app appear to hang. One
  # per task (6 calls) was still measurably slower than before this feature
  # existed, so all 6 tasks are bundled into one JSON value instead. The
  # practice task is included too (key "practice"): its profiles are drawn per
  # respondent, and task 6 repeats it.
  all_tables <- c(list(practice = practice_table),
                  stats::setNames(tables, paste0("q", 1:6)))
  all_profiles <- lapply(all_tables, function(tbl) {
    if (is.null(tbl$attrs)) return(NULL)
    stats::setNames(tbl$attrs, paste0("a", names(tbl$attrs)))
  })
  all_profiles <- Filter(Negate(is.null), all_profiles)
  if (length(all_profiles) > 0) {
    profiles_json <- jsonlite::toJSON(all_profiles, auto_unbox = TRUE)
    profiles_str <- as.character(profiles_json)
    sd_store_value(profiles_str, "cbc_profiles", auto_assign = FALSE)
  }

  # Compact code of every profile shown (practice task + 6 tasks, profiles A
  # and B = 14 codes), stored in one string as cbc_attr_order_levels, e.g.
  # "practice_A:I2E3N1S2C1R2A1|practice_B:...|q1_A:...|...|q6_B:...". Each
  # code lists the attributes in the order they were displayed (same letters
  # as cbc_attr_order), each followed by its level number in cbc_levels.
  profile_codes <- unlist(lapply(names(all_tables), function(task) {
    attrs <- all_tables[[task]]$attrs
    vapply(names(attrs), function(alt) {
      levels_shown <- vapply(attr_order, function(a) {
        match(attrs[[alt]][[a]], cbc_levels[[a]])
      }, integer(1))
      paste0(task, "_", LETTERS[as.integer(alt)], ":",
             paste0(attr_code[attr_order], levels_shown, collapse = ""))
    }, character(1))
  }))
  sd_store_value(paste(profile_codes, collapse = "|"), "cbc_attr_order_levels")

  # Ancla una regla de sd_skip_if() a una página: surveydown solo evalúa cada
  # regla en las páginas que contienen alguna pregunta citada como
  # input$<id>, así que en_pagina(input$<id>) limita la regla a la página de
  # esa pregunta. Siempre es TRUE (el argumento ni se evalúa), así que la
  # regla se cumple aunque la pregunta quede sin responder. Ojo: surveydown
  # vuelve obligatoria toda pregunta citada en sd_skip_if(); por eso se anclan
  # en preguntas que ya son obligatorias (y transicion_ancla se llena sola).
  en_pagina <- function(pregunta) TRUE

  # Define any conditional skip logic here (skip to page if a condition is true)
  #
  # Ruteo de las dos formas (ver version_encuesta más arriba). Ojo:
  # sd_skip_if() solo salta hacia páginas que están MÁS ADELANTE en
  # survey.qmd (las reglas que apuntan hacia atrás se ignoran en silencio).
  # Los saltos hacia atrás quedan entonces en el page_next de sd_nav() en
  # survey.qmd, y aquí solo están los saltos hacia adelante:
  #
  #   fin de                      | forma A               | forma B
  #   transicion                  | merit-1    (page_next)| deserve-1  (regla)
  #   merit-2 (Meritocracia)      | clasicos   (page_next)| id-final   (regla)
  #   lucro (Actitudes+Percepción)| deserve-1  (page_next)| merit-1    (regla)
  #   deserve-7 (Merecimiento)    | id-final   (regla)    | clasicos   (page_next)
  sd_skip_if(
    input$consent_understand == "no" ~ "end_consent",

    version_encuesta == "B" & en_pagina(input$transicion_ancla) ~ "deserve-1",
    version_encuesta == "B" & en_pagina(input$merit_8)          ~ "id-final",
    version_encuesta == "B" & en_pagina(input$lucro_3)          ~ "merit-1",
    version_encuesta == "A" & en_pagina(input$adc_7)            ~ "id-final"
  )

  # Block advancing past the ranking page until at least 3 of the 5 rows
  # have a priority assigned. This is done as a JS click intercept (capture
  # phase, so it runs before Shiny's own next-button handler) instead of
  # sd_stop_if(): sd_stop_if() marks every input it references as required,
  # which would force all 5 rows to be answered instead of just 3.
  runjs("
    document.addEventListener('click', function(e) {
      var btn = e.target.closest('#ranking_next');
      if (!btn) return;
      var rows = ['educacion', 'salud', 'pensiones', 'cuidado_ninos', 'carreteras'];
      var filled = rows.filter(function(r) {
        return document.querySelector('input[name=\"ranking_politicas_' + r + '\"]:checked') !== null;
      }).length;
      if (filled < 3) {
        e.preventDefault();
        e.stopPropagation();
        Shiny.setInputValue('ranking_politicas_block_warning', Math.random(), {priority: 'event'});
      }
    }, true);
  ")
  observeEvent(input$ranking_politicas_block_warning, {
    # Same title/text as surveydown's default required-question warning, so
    # this reads identically to the warning shown for any other unanswered
    # required question.
    shinyWidgets::sendSweetAlert(
      session = session,
      title   = "Advertencia",
      text    = "Por favor, responda todas las preguntas obligatorias antes de continuar.",
      type    = "warning"
    )
  }, ignoreInit = TRUE)

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Allow deselecting a ranking radio by clicking it again.
  # mousedown captures the pre-click checked state; click uses that snapshot
  # to decide whether to uncheck and clear the Shiny value.
  runjs("
    $(document).on('mousedown', 'input[name^=\"ranking_politicas_\"]', function() {
      this.dataset.wasChecked = this.checked ? 'true' : 'false';
    });
    $(document).on('click', 'input[name^=\"ranking_politicas_\"]', function() {
      if (this.dataset.wasChecked === 'true') {
        this.checked = false;
        Shiny.setInputValue(this.name, null, {priority: 'event'});
      }
    });
  ")

  # Enforce unique priority selection across the ranking matrix rows.
  # When a priority is chosen in one row, that same value is disabled
  # (greyed out) in the other two rows so it cannot be assigned twice.
  ranking_filled_count <- reactiveVal(0)
  observe({
    rows <- c("educacion", "salud", "pensiones", "cuidado_ninos", "carreteras")
    selected <- setNames(
      lapply(rows, function(r) input[[paste0("ranking_politicas_", r)]]),
      rows
    )

    for (row in rows) {
      other_vals <- unlist(selected[setdiff(rows, row)])
      other_vals <- other_vals[!is.null(other_vals) & !is.na(other_vals)]
      input_name <- paste0("ranking_politicas_", row)

      # Re-enable all options for this row before applying new constraints
      runjs(sprintf(
        "document.querySelectorAll('input[name=\"%s\"]').forEach(function(el) {
           el.disabled = false;
           var p = el.parentElement; if (p) p.style.opacity = '1';
         });",
        input_name
      ))

      # Disable options already chosen in the other rows
      for (val in other_vals) {
        runjs(sprintf(
          "var el = document.querySelector('input[name=\"%s\"][value=\"%s\"]');
           if (el) {
             el.disabled = true;
             var p = el.parentElement; if (p) p.style.opacity = '0.4';
           }",
          input_name, val
        ))
      }
    }

    # Once exactly 4 of the 5 rows have a priority, the remaining row and
    # remaining priority value are both forced (only one combination is
    # left), so auto-assign it instead of waiting for the user to do it.
    # Only do this when the 4th row was just *added* (count rising to 4),
    # not when it's the result of clearing the 5th row to make a change -
    # otherwise deselecting a row while all 5 are filled would instantly
    # get re-filled, making it look like deselection is broken.
    filled_rows <- rows[!sapply(selected, is.null)]
    current_count <- length(filled_rows)
    previous_count <- isolate(ranking_filled_count())
    if (current_count == 4 && previous_count < current_count) {
      missing_row <- setdiff(rows, filled_rows)
      missing_priority <- setdiff(as.character(1:5), unlist(selected[filled_rows]))
      if (length(missing_row) == 1 && length(missing_priority) == 1) {
        updateRadioButtons(
          session,
          paste0("ranking_politicas_", missing_row),
          selected = missing_priority
        )
      }
    }
    ranking_filled_count(current_count)
  })

  # dec_2 only applies if dec_1's answer is not 0 (i.e. some students would
  # be admitted for free, so paying extra to support them is a real choice)
  # and dec_1 was answered (not 'Prefiero no responder'). Its 'Prefiero no
  # responder' box (dec_2_nr_block) is shown and hidden together with it.
  observe({
    show_dec2 <- !is.null(input$dec_1) && input$dec_1 != 0 &&
      !("99" %in% input$dec_1_nr)
    runjs(sprintf(
      "var display = '%s';
       var el = document.querySelector(\".question-container[data-question-id='dec_2']\");
       if (el) el.style.display = display;
       var nr = document.getElementById('dec_2_nr_block');
       if (nr) nr.style.display = display;",
      if (show_dec2) "block" else "none"
    ))
  })

  # Keep a slider's full trajectory, not just the final answer, to see how the
  # answer changes: every position the slider rested on for >= 0.5 s is
  # appended as "value@UTC time" (entries joined by "|") to the hidden text
  # question <id>_hist, which surveydown then saves as its own column. The
  # hidden question must live on the same page as the slider (surveydown only
  # renders the current page, so updateTextInput() can't reach it otherwise).
  track_slider_history <- function(id) {
    hist_id <- paste0(id, "_hist")
    hist_val <- reactiveVal(NULL)
    settled <- debounce(reactive(input[[id]]), 500)
    observeEvent(settled(), {
      # Skip the default value and page restorations - only log once touched
      req(isTRUE(input[[paste0(id, "_interacted")]]))
      hist <- hist_val()
      # First entry this session: continue the history surveydown restored
      # into <id>_hist (cookies / going back) instead of overwriting it
      if (is.null(hist)) {
        restored <- input[[hist_id]]
        hist <- if (is.null(restored) || restored == "") character(0) else
          strsplit(restored, "|", fixed = TRUE)[[1]]
      }
      value <- as.character(settled())
      last_value <- if (length(hist) > 0) sub("@.*", "", hist[length(hist)]) else NA
      if (identical(value, last_value)) return()
      hist <- c(hist, paste0(value, "@", format(Sys.time(), "%Y-%m-%d %H:%M:%OS1", tz = "UTC")))
      hist_val(hist)
      updateTextInput(session, hist_id, value = paste(hist, collapse = "|"))
    })
  }

  # dec_1 and the 6 conjoint allocation sliders (values for cbc_q* are the
  # CLP amount given to the right-hand applicant, same as cbc_q* itself)
  for (id in c("dec_1", paste0("cbc_q", 1:6))) track_slider_history(id)

  # Hide the load-time spinner once the first render flush completes, i.e.
  # once the survey page is actually visible/interactive, not just once the
  # server code above has finished executing. Registered before sd_server()
  # only because sd_server() must be the last call in this function - the
  # callback itself still fires after render, same as before.
  session$onFlushed(function() {
    waiter_hide()
  }, once = TRUE)

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
