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
      function applyCBCNamesToDom() {
        if (!window.cbc_names) return;
        Object.keys(window.cbc_names).forEach(function(id) {
          var names = window.cbc_names[id];
          if (!Array.isArray(names) || names.length < 2) return;
          var n1 = document.getElementById(id + '_name1');
          var n2 = document.getElementById(id + '_name2');
          if (n1) n1.textContent = names[0];
          if (n2) n2.textContent = names[1];
        });
      }

      Shiny.addCustomMessageHandler('setCBCNames', function(msg) {
        window.cbc_names = window.cbc_names || {};
        window.cbc_names[msg.id] = msg.names;
        applyCBCNamesToDom();
      });

      // Keep names synced when pages are rendered lazily or after navigation
      setInterval(applyCBCNamesToDom, 300);

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', applyCBCNamesToDom);
      } else {
        applyCBCNamesToDom();
      }

      document.addEventListener('click', function() {
        setTimeout(applyCBCNamesToDom, 0);
        setTimeout(applyCBCNamesToDom, 120);
      });

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
      /* dec_2 hidden until dec_1 is interacted with */
      .question-container[data-question-id='dec_2'] {
        display: none;
      }
      /* *_hist (dec_1_hist, cbc_q1_hist..cbc_q6_hist) only store their
         slider's trajectory (filled from the server) */
      .question-container[data-question-id$='_hist'] {
        display: none;
      }
      /* No sé/Prefiero no responder are always coded as value=98/99
         across the survey's mc questions - set them apart from the
         substantive scale above them with extra spacing. */
      .shiny-options-group .radio:has(input[value='98']) {
        margin-top: 18px !important;
      }
      /* Estado-vs-privados sliders (cargo_/efic_/part_): show a label only
         at both extremes and the midpoint. The 4 unlabeled stops in
         between (js-grid-text-1/2/4/5) stay fully selectable as
         intermediate points - they don't get a label, but their tick
         mark is kept visible as an orientation point for respondents.
         Sub-ticks (.small) and the tick at the 3 labeled stops are
         hidden, so only the 4 unlabeled stops show a mark. */
      .question-container[data-question-id^='cargo_'] .irs-grid-pol.small,
      .question-container[data-question-id^='efic_'] .irs-grid-pol.small,
      .question-container[data-question-id^='part_'] .irs-grid-pol.small,
      .question-container[data-question-id^='cargo_'] .irs-grid-pol:has(+ .js-grid-text-0),
      .question-container[data-question-id^='cargo_'] .irs-grid-pol:has(+ .js-grid-text-3),
      .question-container[data-question-id^='cargo_'] .irs-grid-pol:has(+ .js-grid-text-6),
      .question-container[data-question-id^='efic_'] .irs-grid-pol:has(+ .js-grid-text-0),
      .question-container[data-question-id^='efic_'] .irs-grid-pol:has(+ .js-grid-text-3),
      .question-container[data-question-id^='efic_'] .irs-grid-pol:has(+ .js-grid-text-6),
      .question-container[data-question-id^='part_'] .irs-grid-pol:has(+ .js-grid-text-0),
      .question-container[data-question-id^='part_'] .irs-grid-pol:has(+ .js-grid-text-3),
      .question-container[data-question-id^='part_'] .irs-grid-pol:has(+ .js-grid-text-6),
      .question-container[data-question-id^='cargo_'] .js-grid-text-1,
      .question-container[data-question-id^='cargo_'] .js-grid-text-2,
      .question-container[data-question-id^='cargo_'] .js-grid-text-4,
      .question-container[data-question-id^='cargo_'] .js-grid-text-5,
      .question-container[data-question-id^='efic_'] .js-grid-text-1,
      .question-container[data-question-id^='efic_'] .js-grid-text-2,
      .question-container[data-question-id^='efic_'] .js-grid-text-4,
      .question-container[data-question-id^='efic_'] .js-grid-text-5,
      .question-container[data-question-id^='part_'] .js-grid-text-1,
      .question-container[data-question-id^='part_'] .js-grid-text-2,
      .question-container[data-question-id^='part_'] .js-grid-text-4,
      .question-container[data-question-id^='part_'] .js-grid-text-5 {
        display: none !important;
      }
      .question-container[data-question-id^='cargo_'] .irs-grid-text,
      .question-container[data-question-id^='efic_'] .irs-grid-text,
      .question-container[data-question-id^='part_'] .irs-grid-text {
        font-size: 11px !important;
        line-height: 11px !important;
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

male_names <- c("Mateo", "Lucas", "Benjamin", "Nicolas", "Daniel", "Santiago", "Tomas", "Joaquin")
female_names <- c("Sofia", "Valentina", "Isidora", "Martina", "Camila", "Florencia", "Catalina", "Antonia")

# Fixed profiles of the practice task (cbc_practice-page), the same for every
# respondent. Task 6 shows them again with their sides swapped (see server).
practice_profiles <- data.frame(
  altID       = 1:2,
  need        = c("Dificultad", "Holgura"),
  identity    = c("Chile", "Chile"),
  control     = c("Postuló a otras becas pero no obtuvo financiamiento", "No alcanzó a postular a tiempo a otras becas"),
  effort      = c("Más que sus compañeros", "Igual que sus compañeros"),
  reciprocity = c("Ha hecho voluntariado", "No ha hecho voluntariado"),
  attitude    = c("Una ayuda que agradece", "Una ayuda que agradece"),
  stringsAsFactors = FALSE
)

make_cbc_table <- function(df, attr_order = NULL, fixed_names = NULL, slider_id = NULL) {
  # Each profile independently draws a male or female name with p = 0.5,
  # unless the caller fixes them (practice task and its repetition in task 6).
  alt_ids <- sort(unique(df$altID))
  assigned <- if (!is.null(fixed_names)) fixed_names else sapply(alt_ids, function(i) {
    pool <- if (runif(1) < 0.5) male_names else female_names
    sample(pool, 1)
  })
  name_map <- stats::setNames(assigned, as.character(alt_ids))

  has_custom_cols <- all(c("need", "identity", "control", "effort", "reciprocity", "attitude") %in% names(df))

  attrs_by_alt <- NULL

  if (has_custom_cols) {
    if (is.null(slider_id)) slider_id <- paste0("cbc_q", unique(df$qID)[1])
    names_vec <- unname(name_map[as.character(alt_ids)])

    # Snapshot of what's actually shown to the respondent for this task
    # (attributes + assigned name per alternative), keyed by altID. Needed to
    # store the presented profiles alongside the slider answer, since the
    # design is randomized per-session and can't be reconstructed afterwards.
    attr_cols <- c("need", "identity", "control", "effort", "reciprocity", "attitude")
    df_by_alt <- df[match(alt_ids, df$altID), ]
    attrs_by_alt <- stats::setNames(
      lapply(seq_along(alt_ids), function(i) {
        vals <- as.list(df_by_alt[i, attr_cols])
        vals$nombre <- unname(name_map[as.character(alt_ids[i])])
        vals
      }),
      as.character(alt_ids)
    )

    attr_labels <- c(
      need        = "Su hogar llega a fin de mes con:",
      identity    = "País de nacimiento:",
      control     = "Requiere la beca porque:",
      effort      = "Estudia:",
      reciprocity = "Fuera de sus estudios:",
      attitude    = "Ve la beca como:"
    )
    ordered <- if (!is.null(attr_order)) attr_order else names(attr_labels)

    alts <- df |>
      mutate(
        nombre = name_map[as.character(altID)],
        nombre_formatted = dplyr::case_when(
          altID == 1 ~ sprintf("<span style='color: #28a745 !important; font-weight: 800;'>%s</span>", nombre),
          altID == 2 ~ sprintf("<span style='color: #007bff !important; font-weight: 800;'>%s</span>", nombre),
          TRUE ~ nombre
        )
      ) |>
      select(
        `Postulante:` = nombre_formatted,
        !!!setNames(rlang::syms(ordered), attr_labels[ordered])
      )
  } else {
    # Backward-compatible rendering for the original apple template
    alts <- df |>
      mutate(
        nombre = name_map[as.character(altID)],
        price = paste(scales::dollar(price), "/ lb"),
        image = paste0('<img src="', image, '" width=100>')
      ) |>
      select(
        `Profile:` = nombre,
        ` ` = image,
        `Price:` = price,
        `Type:` = type,
        `Freshness:` = freshness
      )
  }

  row.names(alts) <- NULL # Drop row names

  table <- kbl(t(alts), escape = FALSE) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "center",
      font_size = 15
    ) |>
    column_spec(1, bold = TRUE)

  list(
    render = function() { shiny::HTML(as.character(table)) },
    slider_id = if (has_custom_cols) slider_id else NULL,
    names = if (has_custom_cols) names_vec else NULL,
    attrs = attrs_by_alt
  )
}

build_default_conjoint_design <- function(resp_id, n_questions = 6, min_diff = 2) {
  niveles <- list(
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

  # Generate a pair of profiles that differ in at least min_diff attributes
  generate_pair <- function() {
    repeat {
      alt1 <- sapply(niveles, function(lvls) sample(lvls, 1))
      alt2 <- sapply(niveles, function(lvls) sample(lvls, 1))
      if (sum(alt1 != alt2) >= min_diff) return(list(alt1 = alt1, alt2 = alt2))
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
             "need", "identity", "control", "effort", "reciprocity", "attitude")]
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
  # Horiuchi, Kaufman, King & Komisarchik 2026): task 6 shows the practice
  # task's two profiles again with their sides swapped (altID 1 <-> 2), so a
  # consistent answer reflects the profiles rather than the side of the screen.
  df <- df |>
    filter(qID != 6) |>
    bind_rows(
      practice_profiles |>
        mutate(respID = respondentID, qID = 6L, obsID = 6L, altID = 3L - altID) |>
        arrange(altID)
    )

  # Random attribute order fixed for this respondent across the practice task
  # and all 6 questions
  attr_order <- sample(c("need", "identity", "control", "effort", "reciprocity", "attitude"))

  # Persist the display order so attribute-order effects (Hainmueller et al.
  # 2014, sec. 5.3.4) can be diagnosed later. Stored as a 6-character NICERA
  # code (Need, Identity, Control, Effort, Reciprocity, Attitude) - the first
  # letters are unique across the six attributes, so e.g. "EANIRC" means the
  # respondent saw effort first and control last. The order is not otherwise
  # recoverable: cbc_profiles always serializes attributes in a fixed
  # canonical order regardless of what was displayed. Sex is not included -
  # it is signaled in the profile header and always occupies the first row.
  attr_code <- c(need = "N", identity = "I", control = "C",
                 effort = "E", reciprocity = "R", attitude = "A")
  sd_store_value(paste(attr_code[attr_order], collapse = ""), "cbc_attr_order")
  
  # Asignación aleatoria a una de dos versiones de la encuesta. Todas parten
  # igual (welcome -> demographics -> conjoint) y luego difieren en el orden
  # de los bloques restantes:
  #   A: Meritocracia -> Actitudes + Percepción -> Merecimiento -> Cierre
  #   B: Cierre -> Merecimiento -> Actitudes + Percepción -> Meritocracia
  # donde Meritocracia = merit-1..merit-2, Actitudes + Percepción =
  # clasicos..neoliberal, Merecimiento = deserve-1..deserve-7 y Cierre =
  # id-final. Ambas terminan en end_normal. El ruteo está en sd_skip_if()
  # más abajo.
  version_encuesta <- sample(c("A", "B"), 1)
  sd_store_value(version_encuesta)
  # Si la persona retoma la encuesta (cookies), sd_store_value() conserva la
  # versión ya guardada en la base y descarta el nuevo sorteo; se usa esa
  # versión para rutear, si no quien retoma podría cambiar de versión a mitad
  # de camino y saltarse o repetir bloques.
  version_guardada <- session$userData$deferred_values$version_encuesta
  if (!is.null(version_guardada)) version_encuesta <- version_guardada

  # Practice task: rendered here (not in survey.qmd) so its names are drawn
  # per respondent and task 6 can reuse them. As before, one male and one
  # female name in random order.
  practice_names <- sample(c(sample(male_names, 1), sample(female_names, 1)))
  practice_table <- make_cbc_table(practice_profiles |> mutate(qID = 0L),
                                   attr_order = attr_order,
                                   fixed_names = practice_names,
                                   slider_id = "cbc_practice")
  output$cbc_practice_table <- practice_table$render

  # Create the options for each choice question (using the helper function above).
  # Task 6 is the practice task repeated with its sides swapped (see above), so
  # it also reuses the practice names, swapped - drawing new ones could change
  # a profile's name or even its gender, and it would no longer be the same
  # two profiles.
  tables <- lapply(1:5, function(q) make_cbc_table(df |> filter(qID == q), attr_order = attr_order))
  tables[[6]] <- make_cbc_table(df |> filter(qID == 6), attr_order = attr_order,
                                fixed_names = rev(practice_names))
  for (q in 1:6) {
    local({
      tbl <- tables[[q]]
      output[[paste0("cbc", q, "_table")]] <- tbl$render
    })
  }

  # Persist the attributes and name shown for each alternative in every cbc
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
  # practice task is included too (key "practice"): its names are drawn per
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

  # Send candidate names to JS via custom message after Shiny connects
  observe({
    for (tbl in all_tables) {
      if (!is.null(tbl$slider_id)) {
        session$sendCustomMessage("setCBCNames", list(
          id = tbl$slider_id,
          names = as.list(tbl$names)
        ))
      }
    }
  })

  # Ancla una regla de sd_skip_if() a una página: surveydown solo evalúa cada
  # regla en las páginas que contienen alguna pregunta citada como
  # input$<id>, así que en_pagina(input$<id>) limita la regla a la página de
  # esa pregunta. Siempre es TRUE (el argumento ni se evalúa), así que la
  # regla se cumple aunque la pregunta quede sin responder.
  en_pagina <- function(pregunta) TRUE

  # Define any conditional skip logic here (skip to page if a condition is true)
  #
  # Ruteo de las dos versiones (ver version_encuesta más arriba). Ojo:
  # sd_skip_if() solo salta hacia páginas que están MÁS ADELANTE en
  # survey.qmd (las reglas que apuntan hacia atrás se ignoran en silencio).
  # Los saltos hacia atrás quedan entonces en el page_next de sd_nav() en
  # survey.qmd, y aquí solo están los saltos hacia adelante:
  #
  #   página        | versión A             | versión B
  #   cbc_q6_page   | merit-1    (regla)    | id-final   (regla)
  #   merit-2       | clasicos   (page_next)| end_normal (regla)
  #   neoliberal    | deserve-1  (regla)    | merit-1    (regla)
  #   deserve-7     | id-final   (regla)    | clasicos   (page_next)
  #   id-final      | end_normal (regla)    | deserve-1  (page_next)
  sd_skip_if(
    input$consent_understand == "no" ~ "end_consent",

    version_encuesta == "A" & en_pagina(input$cbc_q6)   ~ "merit-1",
    version_encuesta == "B" & en_pagina(input$cbc_q6)   ~ "id-final",
    version_encuesta == "B" & en_pagina(input$merit_8)  ~ "end_normal",
    version_encuesta == "A" & en_pagina(input$neolib_4) ~ "deserve-1",
    version_encuesta == "B" & en_pagina(input$neolib_4) ~ "merit-1",
    version_encuesta == "A" & en_pagina(input$adc_7)    ~ "id-final",
    version_encuesta == "A" & en_pagina(input$gender)   ~ "end_normal"
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
  # be admitted for free, so paying extra to support them is a real choice).
  observeEvent(input$dec_1, {
    show_dec2 <- !is.null(input$dec_1) && input$dec_1 != 0
    runjs(sprintf(
      "var el = document.querySelector(\".question-container[data-question-id='dec_2']\");
       if (el) el.style.display = '%s';",
      if (show_dec2) "block" else "none"
    ))
  }, ignoreNULL = TRUE)

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
