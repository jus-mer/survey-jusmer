Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak(c(
#   'surveydown-dev/surveydown', # Development version from GitHub
#   'here',
#   'glue',
#   'readr',
#   'dplyr'
# ))

# Load packages
library(surveydown)
library(shiny)
library(dplyr)
library(readr)
library(here)
library(kableExtra)
library(tidyr)

# Read in the full survey design file
# We'll use this in the server to create the choice questions
design <- read_csv(here("data", "choice_questions.csv"))

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = TRUE)

# UI setup --------------------------------------------------------------------

ui <- tagList(
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
          var pct1 = getSliderPct(sliderId);
          if (pct1 === null) return;

          var pct2 = 100 - pct1;
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
      /* Nuclear option - override EVERYTHING about IonRangeSlider appearance */
      .irs-bar, .irs-bar--single, .irs-bar-edge,
      .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { 
        background-color: #28a745 !important; 
        background: #28a745 !important;
        border-top-color: #28a745 !important;
        border-bottom-color: #28a745 !important;
        height: 8px !important;
        border: none !important;
      }
      .irs-line,
      .irs--shiny .irs-line {
        background-color: #007bff !important;
        background: #007bff !important;
        border-color: #007bff !important;
        background-image: none !important;
        height: 6px !important;
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
      /* Hide all numbers and grid markings */
      .irs-grid-text, .irs-min, .irs-max, .irs-single,
      .irs-grid, .irs-grid-pol, .irs-grid-pol.small {
        display: none !important;
        visibility: hidden !important;
        height: 0 !important;
        width: 0 !important;
        opacity: 0 !important;
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
          bar.style.background = '#28a745';
          bar.style.backgroundColor = '#28a745';
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
#
# Function to create the question options based on design values
#
# CUSTOMIZE THIS FUNCTION FOR YOUR STUDY:
#
# - Replace the attributes (type, price, freshness) with your own product features
# - Update the image display if needed (or remove if not using images)
# - Modify the formatting/layout of each option as desired
# - Modify the number of alternatives appropriately to your study (alt1, alt2, alt3)

make_cbc_table <- function(df) {
  male_names <- c("Mateo", "Lucas", "Benjamin", "Nicolas", "Daniel", "Santiago", "Tomas", "Joaquin")
  female_names <- c("Sofia", "Valentina", "Isidora", "Martina", "Camila", "Florencia", "Catalina", "Antonia")

  # Guarantee exactly one male and one female name per question pair.
  alt_ids <- sort(unique(df$altID))
  male_pick   <- sample(male_names, 1)
  female_pick <- sample(female_names, 1)
  assigned <- sample(c(male_pick, female_pick))  # randomize which alt is male/female
  name_map <- stats::setNames(assigned, as.character(alt_ids))

  has_custom_cols <- all(c("need", "identity", "control", "effort", "reciprocity", "attitude") %in% names(df))

  if (has_custom_cols) {
    slider_id <- paste0("cbc_q", unique(df$qID)[1])
    names_vec <- unname(name_map[as.character(alt_ids)])

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
        `Situación económica del hogar:` = need,
        `País de nacimiento:` = identity,
        `Requiere la beca porque:` = control,
        `Dedicación al estudio:` = effort,
        `Participación comunitaria:` = reciprocity,
        `Ve la beca como:` = attitude
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
      "El hogar llega con dificultad a fin de mes",
      "El hogar llega con holgura a fin de mes"
    ),
    identity = c("Chile", "Venezuela", "Perú"),
    control = c(
      "Postuló a otras becas pero no obtuvo financiamiento",
      "No alcanzó a postular a otras becas porque se venció el plazo"
    ),
    effort = c(
      "Estudia más que sus compañeros",
      "Estudia igual que sus compañeros",
      "Estudia menos que sus compañeros"
    ),
    reciprocity = c(
      "Participó activamente en voluntariado o apoyo comunitario",
      "No tuvo participación comunitaria"
    ),
    attitude = c("Una ayuda que agradece", "Algo que se merece")
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

  # If the design file still uses the original apple template columns,
  # auto-generate a Spanish conjoint design so pages cbc_q1 ... cbc_q6
  # match the new survey content.
  if (!all(c("rendimiento", "situacion_hogar", "educ_padres") %in% names(df))) {
    df <- build_default_conjoint_design(respondentID, n_questions = 6)
  }

  # Create the options for each choice question (using the helper function above)
  # NOTE: This example contains 6 choice questions - update as needed for your study
  tables <- lapply(1:6, function(q) make_cbc_table(df |> filter(qID == q)))
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

  # Define conditional skip logic (skip to page if a condition is true)
  sd_skip_if(
    sd_value("consent_age") == "no" ~ "end_consent",
    sd_value("consent_understand") == "no" ~ "end_consent"
  )

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
