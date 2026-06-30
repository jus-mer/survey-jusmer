Sys.setlocale("LC_ALL", "en_US.UTF-8")

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
#   'tidyr'
# ))

# Load packages
library(shiny)
library(surveydown)
library(shinyjs)
library(dplyr)
library(readr)
library(here)
library(kableExtra)
library(tidyr)

# Read in the full survey design file
design <- read_csv("data/choice_questions.csv")

db <- sd_db_connect(ignore = TRUE)

# UI setup --------------------------------------------------------------------

ui <- tagList(
  useShinyjs(),
  sd_ui(),
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

make_cbc_table <- function(df, attr_order = NULL) {
  male_names <- c("Mateo", "Lucas", "Benjamin", "Nicolas", "Daniel", "Santiago", "Tomas", "Joaquin")
  female_names <- c("Sofia", "Valentina", "Isidora", "Martina", "Camila", "Florencia", "Catalina", "Antonia")

  # Each profile independently draws a male or female name with p = 0.5.
  alt_ids <- sort(unique(df$altID))
  assigned <- sapply(alt_ids, function(i) {
    pool <- if (runif(1) < 0.5) male_names else female_names
    sample(pool, 1)
  })
  name_map <- stats::setNames(assigned, as.character(alt_ids))

  has_custom_cols <- all(c("need", "identity", "control", "effort", "reciprocity", "attitude") %in% names(df))

  if (has_custom_cols) {
    slider_id <- paste0("cbc_q", unique(df$qID)[1])
    names_vec <- unname(name_map[as.character(alt_ids)])

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
    names = if (has_custom_cols) names_vec else NULL
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

  # Random attribute order fixed for this respondent across all 6 questions
  attr_order <- sample(c("need", "identity", "control", "effort", "reciprocity", "attitude"))

  # Create the options for each choice question (using the helper function above)
  tables <- lapply(1:6, function(q) make_cbc_table(df |> filter(qID == q), attr_order = attr_order))
  for (q in 1:6) {
    local({
      tbl <- tables[[q]]
      output[[paste0("cbc", q, "_table")]] <- tbl$render
    })
  }

  # Send candidate names to JS via custom message after Shiny connects
  observe({
    for (tbl in tables) {
      if (!is.null(tbl$slider_id)) {
        session$sendCustomMessage("setCBCNames", list(
          id = tbl$slider_id,
          names = as.list(tbl$names)
        ))
      }
    }
  })

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
    input$consent_understand == "no" ~ "end_consent"
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
  observeEvent(input$confirm_dec1, {
    show_dec2 <- !is.null(input$dec_1) && input$dec_1 != 0
    runjs(sprintf(
      "var el = document.querySelector(\".question-container[data-question-id='dec_2']\");
       if (el) el.style.display = '%s';",
      if (show_dec2) "block" else "none"
    ))
  }, ignoreNULL = TRUE)

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)


