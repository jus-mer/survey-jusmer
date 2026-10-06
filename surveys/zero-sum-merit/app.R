Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(surveydown)
library(shiny)

# Database (Supabase). Credentials live in a local .env created ONCE with
# surveydown::sd_db_config() run from this folder. Use a Supabase project
# dedicated to this survey (do not reuse the one from the other surveys).
# .env is gitignored. For local tests without writes: SD_IGNORE_DB=true.
ignore_db <- tolower(Sys.getenv("SD_IGNORE_DB", "false")) %in% c("1", "true", "yes") ||
  !file.exists(".env")
db <- sd_db_connect(ignore = ignore_db, gssencmode = "disable")

ui <- sd_ui()

server <- function(input, output, session) {
  completion_code <- sd_completion_code(10)
  sd_store_value(completion_code)

  # zero_sum_widget() writes its own hidden inputs (values + answered flag);
  # nothing else to store here.

  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)
