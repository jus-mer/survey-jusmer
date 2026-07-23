# Monitoreo de cuotas - Pre-piloto (sin_comentarios)
#
# App Shiny separada e independiente de la encuesta: solo LEE de la misma
# base Postgres/Supabase (vía el .env de la encuesta) para mostrar el avance
# de cuotas de sexo, edad y nivel educativo en vivo. No escribe datos.
#
# Instalar una vez si falta algo:
# install.packages(c("shiny", "dplyr", "pool", "DBI", "here", "dotenv", "tibble"))
# pak::pak("surveydown-dev/surveydown")
#
# Correr localmente:
#   setwd("output/monitoreo_cuotas")   # o abrir esta carpeta como proyecto
#   shiny::runApp()

library(shiny)
library(dplyr)
library(tidyr)
library(pool)
library(DBI)
library(here)

# .env de la encuesta real (misma fuente de credenciales que su app.R;
# no se duplica el secreto en esta carpeta) --------------------------------
SURVEY_ENV_FILE <- here::here("surveys", "pre-piloto", "sin_comentarios", ".env")

# Meta total de muestra contratada (Netquest) -------------------------------
N_META_DEFAULT <- 3000

# Marco de cuotas: proporciones fijas del diseño muestral -------------------
marco <- tibble::tribble(
  ~variable, ~categoria,            ~p,
  "sexo",    "Hombre",              0.4776,
  "sexo",    "Mujer",               0.5224,
  "edad",    "18-29",               0.2165,
  "edad",    "30-44",               0.2908,
  "edad",    "45-59",               0.2392,
  "edad",    "60+",                 0.2534,
  "educ",    "Básica o menos",      0.2873,
  "educ",    "Media",               0.4137,
  "educ",    "Técnica superior",    0.0988,
  "educ",    "Universitaria o más", 0.2002
)

variable_labels <- c(sexo = "Sexo", edad = "Edad", educ = "Nivel educativo")

# Mapeo de códigos crudos de la encuesta -> categorías de cuota -------------
# (códigos tomados de survey.qmd: sd_question 'sexo' y 'education')
sexo_map <- c("0" = "Hombre", "1" = "Mujer") # "99" (Otro) queda fuera de la cuota

educ_bucket_map <- c(
  "0" = "Básica o menos", "1" = "Básica o menos", "2" = "Básica o menos",
  "3" = "Media", "4" = "Media",
  "5" = "Técnica superior", "6" = "Técnica superior",
  "7" = "Universitaria o más", "8" = "Universitaria o más", "9" = "Universitaria o más"
)

age_bucket <- function(age) {
  dplyr::case_when(
    is.na(age) | age < 18 ~ NA_character_,
    age <= 29 ~ "18-29",
    age <= 44 ~ "30-44",
    age <= 59 ~ "45-59",
    TRUE ~ "60+"
  )
}

bar_color <- function(avance) {
  dplyr::case_when(
    avance >= 1   ~ "#28a745",
    avance >= 0.7 ~ "#e0a800",
    TRUE          ~ "#dc3545"
  )
}

desvio_color <- function(desvio_pp) {
  # Umbral de +-3pp: el mismo efecto mínimo de interés usado en el diseño
  # muestral del estudio (documentation/04-conjoint-desing.qmd), reutilizado
  # aquí como referencia de "desviación relevante" en la composición entregada.
  dplyr::case_when(
    abs(desvio_pp) < 3 ~ "#6c757d",
    desvio_pp < 0      ~ "#dc3545",
    TRUE               ~ "#007bff"
  )
}

# Conexión a la base (solo lectura en la práctica: esta app nunca escribe) --
db <- surveydown::sd_db_connect(env_file = SURVEY_ENV_FILE, gssencmode = "disable")

if (is.null(db)) {
  stop("No se pudo conectar a la base de datos. Revisa el .env en: ", SURVEY_ENV_FILE)
}

fetch_responses <- function() {
  pool::poolWithTransaction(db$db, function(conn) {
    fields <- DBI::dbListFields(conn, db$table)
    cols <- intersect(c("sexo", "year_of_birth", "education", "time_end"), fields)
    query <- sprintf(
      "SELECT %s FROM %s",
      paste(DBI::dbQuoteIdentifier(conn, cols), collapse = ", "),
      DBI::dbQuoteIdentifier(conn, db$table)
    )
    DBI::dbGetQuery(conn, query)
  })
}

# UI -------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Monitoreo de cuotas — Pre-piloto (sin comentarios)"),
  tags$p(
    sprintf("Tabla conectada en Supabase: %s", db$table),
    style = "color:#6c757d; font-size: 0.9em; margin-top: -10px;"
  ),
  fluidRow(
    column(
      4,
      numericInput("meta_n", "Meta total de muestra (N)", value = N_META_DEFAULT, min = 1, step = 50),
      helpText("Muestra contratada con Netquest; ajusta si cambia.")
    ),
    column(
      4,
      actionButton("refrescar", "Actualizar ahora"),
      br(), br(),
      textOutput("ultima_actualizacion")
    ),
    column(
      4,
      h4(textOutput("n_total"))
    )
  ),
  hr(),
  uiOutput("bloques_cuota")
)

# Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Un mismo disparador para el timer automático (cada 15s) y el botón de
  # refresco manual, para que ambos fuercen la misma recarga desde la DB.
  refresh_tick <- reactiveVal(0)

  observe({
    invalidateLater(15000, session)
    isolate(refresh_tick(refresh_tick() + 1))
  })

  observeEvent(input$refrescar, {
    refresh_tick(refresh_tick() + 1)
  })

  raw_data <- eventReactive(refresh_tick(), fetch_responses(), ignoreNULL = FALSE)

  ultima_actualizacion_hora <- reactiveVal(Sys.time())
  observeEvent(raw_data(), {
    ultima_actualizacion_hora(Sys.time())
  })

  output$ultima_actualizacion <- renderText({
    paste("Última actualización:", format(ultima_actualizacion_hora(), "%H:%M:%S"))
  })

  # Recodifica lo crudo de la DB a las columnas sexo/edad/educ que usa el
  # marco de cuotas (mismos códigos que sd_question() en survey.qmd).
  datos <- reactive({
    df <- raw_data()
    req(df)

    df$completo <- if ("time_end" %in% names(df)) {
      !is.na(df$time_end) & df$time_end != ""
    } else {
      TRUE
    }

    anio_actual <- as.integer(format(Sys.Date(), "%Y"))

    df |>
      dplyr::filter(completo) |>
      dplyr::transmute(
        sexo = unname(sexo_map[as.character(sexo)]),
        educ = unname(educ_bucket_map[as.character(education)]),
        edad = age_bucket(suppressWarnings(anio_actual - as.integer(year_of_birth)))
      )
  })

  output$n_total <- renderText({
    paste("Respuestas completas:", nrow(datos()))
  })

  # Cumplimiento de cuotas: para cada celda del marco, cuántas respuestas
  # llevamos (n_obs), cuántas faltan para la meta (faltan) y qué tan
  # desviada está la composición actual respecto al objetivo (desvio_pp).
  cumplimiento <- reactive({
    meta_n <- input$meta_n
    req(meta_n)

    largo <- datos() |>
      tidyr::pivot_longer(everything(), names_to = "variable", values_to = "categoria")

    largo |>
      dplyr::count(variable, categoria, name = "n_obs") |>
      dplyr::right_join(marco, by = c("variable", "categoria")) |>
      dplyr::mutate(n_obs = dplyr::coalesce(n_obs, 0L)) |>
      dplyr::group_by(variable) |>
      dplyr::mutate(
        meta      = round(p * meta_n),
        faltan    = pmax(meta - n_obs, 0),
        avance    = dplyr::if_else(meta > 0, n_obs / meta, 0),
        total_obs = sum(n_obs),
        p_obs     = dplyr::if_else(total_obs > 0, n_obs / total_obs, 0),
        desvio_pp = (p_obs - p) * 100
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-total_obs)
  })

  # Respuestas sin categoría asignable (NA/"Otro"/"prefiero no decir"),
  # informativas: quedan fuera del marco de cuotas por diseño.
  sin_dato <- reactive({
    datos() |>
      tidyr::pivot_longer(everything(), names_to = "variable", values_to = "categoria") |>
      dplyr::filter(is.na(categoria)) |>
      dplyr::count(variable, name = "n_sin_dato")
  })

  render_bloque <- function(var, tabla, n_sin_dato_var) {
    filas <- lapply(seq_len(nrow(tabla)), function(i) {
      fila <- tabla[i, ]
      pct_barra <- min(100, round(fila$avance * 100))
      tags$tr(
        tags$td(fila$categoria, style = "white-space: nowrap;"),
        tags$td(sprintf("%d (%.1f%%)", fila$meta, fila$p * 100)),
        tags$td(sprintf("%d (%.1f%%)", fila$n_obs, fila$p_obs * 100)),
        tags$td(
          sprintf("%+.1f pp", fila$desvio_pp),
          style = sprintf("color:%s; font-weight:600;", desvio_color(fila$desvio_pp))
        ),
        tags$td(fila$faltan),
        tags$td(
          div(
            style = "background:#e9ecef; border-radius:4px; overflow:hidden; height:18px; width:100%;",
            div(style = sprintf(
              "background:%s; width:%d%%; height:100%%;",
              bar_color(fila$avance), pct_barra
            ))
          ),
          style = "min-width: 160px;"
        )
      )
    })

    tagList(
      h4(variable_labels[[var]]),
      tags$table(
        class = "table table-condensed",
        tags$thead(tags$tr(
          tags$th("Categoría"), tags$th("Meta"), tags$th("Observado"),
          tags$th("Desvío"), tags$th("Faltan"), tags$th("Avance")
        )),
        tags$tbody(filas)
      ),
      if (n_sin_dato_var > 0) {
        tags$p(
          sprintf("Sin dato / fuera de cuota: %d", n_sin_dato_var),
          style = "color:#6c757d; font-size: 0.9em;"
        )
      },
      hr()
    )
  }

  output$bloques_cuota <- renderUI({
    tabla <- cumplimiento()
    sin_dato_df <- sin_dato()

    tagList(
      lapply(c("sexo", "edad", "educ"), function(var) {
        n_sin_dato_var <- sin_dato_df$n_sin_dato[sin_dato_df$variable == var]
        if (length(n_sin_dato_var) == 0) n_sin_dato_var <- 0

        render_bloque(
          var,
          dplyr::filter(tabla, variable == var),
          n_sin_dato_var
        )
      })
    )
  })

  session$onSessionEnded(function() {
    # No cerramos el pool: es compartido a nivel de proceso R y se reutiliza
    # entre sesiones mientras la app siga corriendo.
  })
}

shinyApp(ui = ui, server = server)
