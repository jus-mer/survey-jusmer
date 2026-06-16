# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

setwd("general-survey") # seteo de ruta del proyecto

# Load packages
library(shiny)
library(surveydown)
library(shinyjs)

db <- sd_db_connect(ignore = TRUE)

# UI setup --------------------------------------------------------------------

ui <- tagList(
  useShinyjs(),
  sd_ui()
)

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)
  sd_store_value(completion_code)
  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

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
  observe({
    rows <- c("educacion", "salud", "cuidado_ninos")
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
  })

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)