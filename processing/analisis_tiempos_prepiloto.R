# Tiempos de respuesta de tiempos-prepiloto.csv
# Construye, a partir de las respuestas crudas (filtradas a
# consent_understand == "yes"):
#   - tiempo total de la encuesta por caso (time_end - time_start)
#   - tiempo de respuesta de cada pregunta por caso, medido como el tiempo
#     transcurrido desde time_start hasta el timestamp de esa pregunta
#     (time_q_*), ya que el instrumento se ramifica en tres modulos
#     (assigned_module A/B/C) y cada respondente solo contesta uno
#   - un resumen agregado (n, media, mediana, min, max) por pregunta

input_csv <- "input/tbl/tiempos-prepiloto.csv"
out_dir   <- "output/tiempos-prepiloto"

df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", na.strings = "", encoding = "UTF-8")

# -----------------------------------------------------------------------------
# 1. Filtrar a consentimiento valido y parsear timestamps.
# -----------------------------------------------------------------------------

df <- df[!is.na(df$consent_understand) & df$consent_understand == "yes", ]

parse_ts <- function(x) {
  as.POSIXct(sub(" UTC$", "", x), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

df$time_start <- parse_ts(df$time_start)
df$time_end   <- parse_ts(df$time_end)

qcols <- grep("^time_q_", names(df), value = TRUE)
for (col in qcols) df[[col]] <- parse_ts(df[[col]])

# -----------------------------------------------------------------------------
# 2. Tiempo total de la encuesta por caso.
# -----------------------------------------------------------------------------

tiempo_caso <- data.frame(
  session_id       = df$session_id,
  respID           = df$respID,
  assigned_module  = df$assigned_module,
  time_start       = df$time_start,
  time_end         = df$time_end,
  tiempo_total_seg = as.numeric(difftime(df$time_end, df$time_start, units = "secs")),
  stringsAsFactors = FALSE
)
tiempo_caso$tiempo_total_min <- round(tiempo_caso$tiempo_total_seg / 60, 2)

# -----------------------------------------------------------------------------
# 3. Tiempo de respuesta de cada pregunta por caso:
#    - tiempo_seg: time_q_<pregunta> - time_start (acumulado desde el inicio)
#    - duracion_seg: time_q_<pregunta> - checkpoint anterior, donde el
#      checkpoint anterior es time_start (para la primera pregunta con valor)
#      o el time_q_* previo con valor de ESE caso, saltando las columnas del
#      modulo no asignado (que quedan NA). Aproxima el tiempo dedicado a esa
#      pregunta puntual.
#    Ambas quedan en NA para los casos que no vieron esa pregunta (modulo
#    distinto).
# -----------------------------------------------------------------------------

duracion_mat <- matrix(NA_real_, nrow = nrow(df), ncol = length(qcols),
                        dimnames = list(NULL, qcols))
for (i in seq_len(nrow(df))) {
  last_time <- df$time_start[i]
  for (col in qcols) {
    cur <- df[[col]][i]
    if (!is.na(cur)) {
      duracion_mat[i, col] <- as.numeric(difftime(cur, last_time, units = "secs"))
      last_time <- cur
    }
  }
}

detalle_list <- lapply(qcols, function(col) {
  data.frame(
    session_id      = df$session_id,
    respID          = df$respID,
    assigned_module = df$assigned_module,
    pregunta        = sub("^time_q_", "", col),
    tiempo_seg       = as.numeric(difftime(df[[col]], df$time_start, units = "secs")),
    duracion_seg     = duracion_mat[, col],
    stringsAsFactors = FALSE
  )
})
tiempo_pregunta_detalle <- do.call(rbind, detalle_list)
tiempo_pregunta_detalle <- tiempo_pregunta_detalle[!is.na(tiempo_pregunta_detalle$tiempo_seg), ]

# Desviacion estandar, por caso, de la duracion (duracion_seg) entre todas
# las preguntas que respondio: mide que tan variable fue su ritmo de
# respuesta pregunta a pregunta.
sd_duracion_caso <- stats::aggregate(
  duracion_seg ~ session_id,
  data = tiempo_pregunta_detalle,
  FUN = function(x) sd(x, na.rm = TRUE)
)
names(sd_duracion_caso)[2] <- "sd_duracion_pregunta_seg"
tiempo_caso <- merge(tiempo_caso, sd_duracion_caso, by = "session_id", all.x = TRUE)
tiempo_caso$sd_duracion_pregunta_seg <- round(tiempo_caso$sd_duracion_pregunta_seg, 2)

# -----------------------------------------------------------------------------
# 4. Resumen agregado por pregunta (n, media, mediana, sd, min, max) + modulo(s)
#    en que se presenta esa pregunta (inferido a partir de los datos: en que
#    assigned_module hay al menos un valor no vacio de time_q_<pregunta>).
# -----------------------------------------------------------------------------

resumen_list <- lapply(qcols, function(col) {
  pregunta <- sub("^time_q_", "", col)
  modulos_presentes <- sort(unique(df$assigned_module[!is.na(df[[col]])]))
  modulo_txt <- if (length(modulos_presentes) == 0) {
    NA_character_
  } else if (length(modulos_presentes) == length(unique(na.omit(df$assigned_module)))) {
    "Todos"
  } else {
    paste(modulos_presentes, collapse = "/")
  }

  x <- tiempo_pregunta_detalle$tiempo_seg[tiempo_pregunta_detalle$pregunta == pregunta]
  d <- tiempo_pregunta_detalle$duracion_seg[tiempo_pregunta_detalle$pregunta == pregunta]
  data.frame(
    pregunta = pregunta,
    modulo   = modulo_txt,
    n        = length(x),
    media_seg   = round(mean(x), 2),
    mediana_seg = round(median(x), 2),
    sd_seg      = round(sd(x), 2),
    min_seg     = round(min(x, Inf), 2),
    max_seg     = round(max(x, -Inf), 2),
    duracion_media_seg   = round(mean(d), 2),
    duracion_mediana_seg = round(median(d), 2),
    stringsAsFactors = FALSE
  )
})
tiempo_pregunta_resumen <- do.call(rbind, resumen_list)
tiempo_pregunta_resumen <- tiempo_pregunta_resumen[tiempo_pregunta_resumen$n > 0, ]

# -----------------------------------------------------------------------------
# 5. Estimacion del tiempo total si un mismo respondente tuviera que responder
#    la seccion comun + los tres modulos (A + B + C), en vez de solo el que le
#    toco por aleatorizacion. Para cada caso se suman las duraciones
#    (duracion_seg) de sus preguntas comunes y, por separado, las de su
#    modulo asignado; luego se promedia cada suma por separado y se suman los
#    4 promedios.
# -----------------------------------------------------------------------------

tiempo_pregunta_detalle <- merge(
  tiempo_pregunta_detalle,
  tiempo_pregunta_resumen[, c("pregunta", "modulo")],
  by = "pregunta", all.x = TRUE
)

suma_por_caso <- function(seccion) {
  sub <- tiempo_pregunta_detalle[tiempo_pregunta_detalle$modulo == seccion, ]
  stats::aggregate(duracion_seg ~ session_id, data = sub, FUN = sum, na.rm = TRUE)
}

comun_caso   <- suma_por_caso("Todos")
moduloA_caso <- suma_por_caso("A")
moduloB_caso <- suma_por_caso("B")
moduloC_caso <- suma_por_caso("C")

tiempo_estimado_total <- data.frame(
  seccion = c("Comun (todos)", "Modulo A", "Modulo B", "Modulo C"),
  n_casos = c(nrow(comun_caso), nrow(moduloA_caso), nrow(moduloB_caso), nrow(moduloC_caso)),
  promedio_seg = round(c(
    mean(comun_caso$duracion_seg),
    mean(moduloA_caso$duracion_seg),
    mean(moduloB_caso$duracion_seg),
    mean(moduloC_caso$duracion_seg)
  ), 2),
  stringsAsFactors = FALSE
)
tiempo_estimado_total$promedio_min <- round(tiempo_estimado_total$promedio_seg / 60, 2)

tiempo_estimado_total <- rbind(
  tiempo_estimado_total,
  data.frame(
    seccion = "Total estimado (comun + A + B + C)",
    n_casos = NA_integer_,
    promedio_seg = round(sum(tiempo_estimado_total$promedio_seg), 2),
    promedio_min = round(sum(tiempo_estimado_total$promedio_seg) / 60, 2)
  )
)

# -----------------------------------------------------------------------------
# 6. Guardar salidas.
# -----------------------------------------------------------------------------

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(tiempo_caso, file.path(out_dir, "tiempos-caso.csv"), row.names = FALSE, na = "")
write.csv(tiempo_pregunta_detalle, file.path(out_dir, "tiempos-pregunta-detalle.csv"), row.names = FALSE, na = "")
write.csv(tiempo_pregunta_resumen, file.path(out_dir, "tiempos-pregunta-resumen.csv"), row.names = FALSE, na = "")
write.csv(tiempo_estimado_total, file.path(out_dir, "tiempos-estimado-total.csv"), row.names = FALSE, na = "")

cat("Casos con consent_understand == 'yes':", nrow(tiempo_caso), "\n")
cat("Preguntas (time_q_*) resumidas:", nrow(tiempo_pregunta_resumen), "\n")
cat("Tiempo total estimado (comun + A + B + C):",
    tiempo_estimado_total$promedio_min[tiempo_estimado_total$seccion == "Total estimado (comun + A + B + C)"],
    "min\n")
cat("Guardado en:", out_dir, "\n")
