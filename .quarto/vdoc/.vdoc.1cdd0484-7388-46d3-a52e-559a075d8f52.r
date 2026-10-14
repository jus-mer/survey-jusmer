#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

#
#
#
#
#
#
#
#| label: setup
#| message: false

pacman::p_load(dplyr, DT, here)

here::i_am("pre-pilot-analysis/ppilot-analysis.qmd")
#
#
#
#| label: carga-comentarios

resumen <- read.csv(
  here::here("pre-pilot-analysis", "tbl/comentarios-prepiloto-resumen.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| warning: false
#| label: union bases

pacman::p_load(dplyr, DT)


comentarios_csv <- here::here("input", "tbl/comentarios-prepiloto.csv")
tiempos_csv     <- here::here("input", "tbl/tiempos-prepiloto.csv")

union_csv       <- here::here("pre-pilot-analysis", "tbl/tiempos-prepiloto-union.csv")

ids_muestra <- c(1394, 1924, 501, 1785, 1722, 1968, 374, 1878) # casos validos del primer prepiloto

comentarios <- read.csv(comentarios_csv, stringsAsFactors = FALSE, check.names = FALSE,
                         colClasses = "character", na.strings = "", encoding = "UTF-8")
comentarios <- comentarios[comentarios$respID %in% ids_muestra, ]

tiempos <- read.csv(tiempos_csv, stringsAsFactors = FALSE, check.names = FALSE,
                     colClasses = "character", na.strings = "", encoding = "UTF-8")

# comentarios-prepiloto.csv (primer pre-piloto) trae columnas adicionales (bloque de comentarios) que no existen en tiempos-prepiloto.csv (segundo pre-piloto); se conservan solo las columnas en comun para poder unir ambas bases.

comentarios <- comentarios[, names(tiempos)]

df_union <- rbind(tiempos, comentarios)

dir.create(dirname(union_csv), showWarnings = FALSE, recursive = TRUE)
write.csv(df_union, union_csv, row.names = FALSE, na = "")
#
#
#
#
#
#| warning: false
#| label: proc

df <- df_union

df <- df[!is.na(df$consent_understand) & df$consent_understand == "yes", ]

parse_ts <- function(x) {
  as.POSIXct(sub(" UTC$", "", x), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

df$time_start <- parse_ts(df$time_start)
df$time_end   <- parse_ts(df$time_end)

verifica_1 <- df %>%
  filter(respID == 817) # crear objeto para comprobar el for

qcols <- grep("^time_q_", names(df), value = TRUE)
for (col in qcols) df[[col]] <- parse_ts(df[[col]]) # este for convierte todas las columnas de "tiempo por pregunta" de texto a formato de fecha/hora utilizable (character a POSIXct)

# time_p_cbc_qN_page marca el momento en que el respondente LLEGA a la página de la tarea N (no cuando la deja), por lo que el término de la tarea N es la llegada a la página siguiente (tarea N+1). Tras la tarea 6 la página siguiente depende del módulo: A -> "clasicos", B -> "decomodifica"; el módulo C no registra marca de página tras la tarea 6 (sus preguntas siguen en la misma página), por lo que se usa la marca original de respuesta de cbc_q6 (`time_q_cbc_q6`), que corresponde al último movimiento del slider.
cbc_fin <- c(paste0("time_p_cbc_q", 2:6, "_page"), "time_p_clasicos")
fin_q6 <- function(d) {
  f <- function(col) parse_ts(d[[col]])
  fin <- f("time_p_clasicos")
  fin[is.na(fin)] <- f("time_p_decomodifica")[is.na(fin)]
  fin[is.na(fin)] <- d$time_q_cbc_q6[is.na(fin)] # debe llamarse antes de reemplazar time_q_cbc_q6
  fin
}
for (i in 1:5) {
  df[[paste0("time_q_cbc_q", i)]] <- parse_ts(df[[cbc_fin[i]]])
}
df$time_q_cbc_q6 <- fin_q6(df) # considera la marca temporal de llegada a la página siguiente como tiempo de término de cada tarea del conjoint
#
#
#
#
#
#
#
#
#
#
class(verifica_1$time_q_mj_1)
#
#
#
qcols <- grep("^time_q_", names(verifica_1), value = TRUE)
for (col in qcols) verifica_1[[col]] <- parse_ts(verifica_1[[col]])

for (i in 1:5) {
  verifica_1[[paste0("time_q_cbc_q", i)]] <- parse_ts(verifica_1[[cbc_fin[i]]])
}
verifica_1$time_q_cbc_q6 <- fin_q6(verifica_1) # replica en verifica_1 el mismo ajuste aplicado a df para las preguntas del conjoint

class(verifica_1$time_q_mj_1)
#
#
#
#
#
#
#

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

#
#
#
#| label: batterie-division
#| 

# Objetos que almacenan las preguntas de cada batería temática, para luego poder generar tablas de resumen por batería.

demo_1 <- c("respondent", "sexo", "year_of_birth", "education")

conjoint <- c("cbc_practice", "manip_check", "cbc_q1", "cbc_q2", "cbc_q3", "cbc_q4", "cbc_q5", "cbc_q6")

mjp <- c("mj_1", "mj_2", "mj_3")

mjp_new <- c("ref_1", "ref_2", "ref_3", "proy_1", "proy_2")

mjp_time <- c("pres_1", "pres_2", "pres_3", "pres_4", "pres_5", "pres_6")

comodifica <- c("valor_1", "valor_2", "valor_3", "valor_4", "valor_5")

exp_comodifica <- c("dec_1", "dec_2", "dec_3")

lucro <- c("lucro_1", "lucro_2", "lucro_3")

neoliberal <- c("neolib_1", "neolib_2", "neolib_3", "neolib_4")

nicera <- c("control_1", "control_2", "control_3", "actitud_1", "actitud_2", "reciproc_1", "reciproc_2", "reciproc_3", "actitud_4", "id_1", "id_2", "id_3", "nec_1", "nec_2", "nec_3", "esf_1", "esf_2", "esf_3", "esf_4", "esf_5")

aditional <- c("adc_qstn", "adc_5", "adc_6", "adc_7")

demo_2 <- c("gender", "income", "salud_prov", "educ_prov")
#
#
#
#
#
#| label: time-per-question


duracion_mat <- matrix(NA_real_, nrow = nrow(df), ncol = length(qcols),
                        dimnames = list(NULL, qcols))
for (i in seq_len(nrow(df))) {
  last_time <- df$time_start[i]
  for (col in qcols) {
    cur <- df[[col]][i]
    if (!is.na(cur)) {
      duracion_mat[i, col] <- as.numeric(difftime(cur, last_time, units = "secs")) # se calcula la diferencia entre un checkpoint (marca temporal de la pregunta) y el checkpoint anterior
      last_time <- cur
    }
  }
} # este for calcula cuánto tiempo tardó cada respondente en cada pregunta

# Tarea 1 del conjoint: el checkpoint anterior en el bucle es la respuesta de manip_check, que ocurre unos segundos antes de llegar a la página de la tarea 1. Se usa la llegada a su propia página como inicio para medir solo el tiempo en la tarea.
inicio_q1 <- df$time_p_cbc_q1_page
inicio_q1 <- parse_ts(inicio_q1)
ok_q1 <- !is.na(df$time_q_cbc_q1) & !is.na(inicio_q1)
duracion_mat[ok_q1, "time_q_cbc_q1"] <- as.numeric(difftime(df$time_q_cbc_q1[ok_q1], inicio_q1[ok_q1], units = "secs"))

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

#
#
#
#
#
#| label: verifica-time-per-question

verifica_1[qcols] <- lapply(verifica_1[qcols], parse_ts)

last_time <- verifica_1$time_start
verif_tbl <- data.frame(
  pregunta            = character(),
  checkpoint_anterior = as.POSIXct(character()),
  tiempo_pregunta     = as.POSIXct(character()),
  duracion_seg        = numeric(),
  stringsAsFactors    = FALSE
)

for (col in qcols) {
  cur <- verifica_1[[col]][1]
  if (!is.na(cur)) {
    verif_tbl <- rbind(verif_tbl, data.frame(
      pregunta            = sub("^time_q_", "", col),
      checkpoint_anterior = last_time,
      tiempo_pregunta     = cur,
      duracion_seg        = as.numeric(difftime(cur, last_time, units = "secs"))
    ))
    last_time <- cur
  }
} # se aplica el for a la base verifica_1

i_verifica <- which(df$respID == verifica_1$respID[1])
verif_tbl$duracion_mat_original <- unname(duracion_mat[i_verifica, paste0("time_q_", verif_tbl$pregunta)])

verif_tbl
#
#
#
#
#
#
#

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

#
#
#
#
#
#
#

tiempo_pregunta_detalle <- merge(
  tiempo_pregunta_detalle,
  tiempo_pregunta_resumen[, c("pregunta", "modulo")],
  by = "pregunta", all.x = TRUE
)

# El caso 374 se excluye en la Tabla Tiempo de respuesta por respondente (tabla 2) para no alterar los tiempos promedio de respuesta; sigue presente en el resto del reporte.
detalle_estimado <- tiempo_pregunta_detalle[tiempo_pregunta_detalle$respID != "374", ]

suma_por_caso <- function(seccion) {
  sub <- detalle_estimado[detalle_estimado$modulo == seccion, ]
  stats::aggregate(duracion_seg ~ session_id, data = sub, FUN = sum, na.rm = TRUE)
}

comun_caso   <- suma_por_caso("Todos")
moduloA_caso <- suma_por_caso("A")
moduloB_caso <- suma_por_caso("B")
moduloC_caso <- suma_por_caso("C")

tiempo_estimado_total <- data.frame(
  seccion = c("Común (todos)", "Modulo A", "Modulo B", "Modulo C"),
  n_casos = c(nrow(comun_caso), nrow(moduloA_caso), nrow(moduloB_caso), nrow(moduloC_caso)),
  mediana_seg = round(c(
    median(comun_caso$duracion_seg),
    median(moduloA_caso$duracion_seg),
    median(moduloB_caso$duracion_seg),
    median(moduloC_caso$duracion_seg)
  ), 2),
  stringsAsFactors = FALSE
)
tiempo_estimado_total$mediana_min <- round(tiempo_estimado_total$mediana_seg / 60, 2)

tiempo_estimado_total <- rbind(
  tiempo_estimado_total,
  data.frame(
    seccion = "Total estimado (común + A + B + C)",
    n_casos = NA_integer_,
    mediana_seg = round(sum(tiempo_estimado_total$mediana_seg), 2),
    mediana_min = round(sum(tiempo_estimado_total$mediana_seg) / 60, 2)
  )
)
#
#
#
#
#
out_dir <- here::here("output/tiempos-prepiloto")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(tiempo_caso, file.path(out_dir, "tiempos-caso.csv"), row.names = FALSE, na = "")
write.csv(tiempo_pregunta_detalle, file.path(out_dir, "tiempos-pregunta-detalle.csv"), row.names = FALSE, na = "")
write.csv(tiempo_pregunta_resumen, file.path(out_dir, "tiempos-pregunta-resumen.csv"), row.names = FALSE, na = "")
write.csv(tiempo_estimado_total, file.path(out_dir, "tiempos-estimado-total.csv"), row.names = FALSE, na = "")
#
#
#
#
#
#| label: carga-part-ranking
#| warning: false

tiempos_part <- read.csv(
  here::here("input", "tbl/tiempos-part.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
)
names(tiempos_part)[1] <- "respID"

tiempo_ranking <- read.csv(
  here::here("input", "tbl/tiempo-ranking.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
)
names(tiempo_ranking)[1] <- "respID"
#
#
#
#
#
#
#| label: tbl-comentarios
#| echo: false

comentario_cols <- grep("^comentario_", names(resumen), value = TRUE)

resumen |>
  filter(!is.na(n_comentarios), n_comentarios > 0) |>
  select(variable, label, n_comentarios, all_of(comentario_cols)) |>
  datatable(
    rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE),
    colnames = c(
      "Variable", "Label", "N comentarios",
      gsub("comentario_", "Comentario ", comentario_cols)
    )
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-tiempo-caso
#| warning: false
#| tbl-cap: Tiempo de respuesta por respondente

tiempo_caso |>
  filter(respID != "749") |>
  select(
    respID, assigned_module, time_start, time_end, tiempo_total_min,
    sd_duracion_pregunta_seg
  ) |>
  arrange(desc(tiempo_total_min)) |>
  datatable(
    filter = "top",
    rownames = FALSE,
    options = list(pageLength = 15, scrollX = TRUE),
    colnames = c(
      "respID", "Módulo", "Inicio", "Fin", "Tiempo total (min)",
      "DE duración por pregunta (s)"
    )
  )
#
#
#
#
#
#
#
#
#
#
#
#
#| label: baterias-functions
#| warning: false
#| include: false

baterias <- list(
  "Demográficos inicio" = demo_1,
  "Conjoint" = conjoint,
  "Market Justice Preferences" = mjp,
  "Market Justice Reflexive" = mjp_new,
  "Market Justice en el tiempo" = mjp_time,
  "Comodificación de servicios" = comodifica,
  "Escenarios de subvención" = exp_comodifica,
  "Lucro en servicios" = lucro,
  "Neoliberalismo" = neoliberal,
  "NICERA" = nicera,
  "Adicionales" = aditional,
  "Demográficos (bloque 2)" = demo_2
)

meta_cols <- c(
  "session_id", "time_start", "time_end", "completion_code", "respID",
  "assigned_module", "cbc_profiles", "browser", "ip_address",
  "consent_understand", "current_page"
)
vars_encuesta <- setdiff(names(df_union), meta_cols)
vars_encuesta <- vars_encuesta[!grepl("^time_q_|^time_p_", vars_encuesta)]

es_numerica <- function(x) {
  x <- x[!is.na(x)]
  length(x) > 0 && !anyNA(suppressWarnings(as.numeric(x)))
}

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

df_desc <- df_union[df_union$consent_understand == "yes", vars_encuesta]

tabla_desc <- do.call(rbind, lapply(vars_encuesta, function(v) {
  x <- df_desc[[v]]
  n_valid    <- sum(!is.na(x))
  n_blanco   <- sum(is.na(x))
  n_9899     <- sum(x %in% c("98", "99"), na.rm = TRUE)
  v_label    <- resumen$label[match(v, resumen$variable)]

  if (es_numerica(x)) {
    xn <- as.numeric(x)
    data.frame(
      variable = v, label = v_label, tipo = "Numérica", n = n_valid,
      n_9899 = n_9899, n_blanco = n_blanco,
      media   = round(mean(xn, na.rm = TRUE), 2),
      mediana = round(median(xn, na.rm = TRUE), 2),
      sd      = round(sd(xn, na.rm = TRUE), 2),
      min     = round(min(xn, na.rm = TRUE), 2),
      max     = round(max(xn, na.rm = TRUE), 2),
      n_categorias = NA_integer_,
      moda         = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      variable = v, label = v_label, tipo = "Categórica", n = n_valid,
      n_9899 = n_9899, n_blanco = n_blanco,
      media = NA_real_, mediana = NA_real_, sd = NA_real_,
      min = NA_real_, max = NA_real_,
      n_categorias = length(unique(x[!is.na(x)])),
      moda         = moda(x),
      stringsAsFactors = FALSE
    )
  }
}))

tabla_desc |>
  datatable(
    filter = "top",
    rownames = FALSE,
    options = list(pageLength = 25, scrollX = TRUE),
    colnames = c(
      "Variable", "Label", "Tipo", "N válido", "NA", "Sin responder",
      "Media", "Mediana", "DE", "Mín", "Máx", "N categorías", "Moda"
    )
  ) |>
  formatStyle(
    "sd",
    backgroundColor = styleInterval(1.0, c("white", "#f79b9b"))
  )

tabla_bateria <- function(nm) {
  ids <- baterias[[nm]]
  tab <- tiempo_pregunta_resumen[tiempo_pregunta_resumen$pregunta %in% ids, ]
  tab <- tab[order(match(tab$pregunta, ids)), ]
  tab$label <- resumen$label[match(tab$pregunta, resumen$variable)]

  tab |>
    select(pregunta, label, modulo, n, duracion_media_seg, duracion_mediana_seg) |>
    datatable(
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tab), scrollX = TRUE),
      colnames = c(
        "Pregunta", "Label", "Módulo", "N",
        "Duración media (s)", "Duración mediana (s)"
      )
    ) |>
    formatStyle(
      c("duracion_media_seg", "duracion_mediana_seg"),
      backgroundColor = styleInterval(30, c("white", "#f79b9b"))
    )
}
#
#
#
#
#| label: baterias-completo-def
#| warning: false
#| include: false

tabla_completa_bateria <- function(nm) {
  ids <- baterias[[nm]]
  tiempos <- tiempo_pregunta_resumen[tiempo_pregunta_resumen$pregunta %in% ids,
                                     c("pregunta", "n", "duracion_mediana_seg")]
  desc <- tabla_desc[tabla_desc$variable %in% ids,
                     c("variable", "label", "media", "mediana", "sd")]

  tab <- merge(tiempos, desc, by.x = "pregunta", by.y = "variable", all = TRUE)
  tab <- tab[order(match(tab$pregunta, ids)), ]
  tab <- tab[, c("pregunta", "label", "n", "duracion_mediana_seg", "media", "mediana", "sd")]

  tab |>
    datatable(
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tab), scrollX = TRUE),
      colnames = c(
        "Pregunta", "Label", "N", "Duración mediana (seg)", "Media", "Mediana", "DE"
      )
    ) |>
    formatStyle(
      "duracion_mediana_seg",
      backgroundColor = styleInterval(30, c("white", "#f79b9b"))
    ) |>
    formatStyle(
      "sd",
      backgroundColor = styleInterval(1.0, c("white", "#f79b9b"))
    )
}
#
#
#
#
#
#
#| label: tbl-descriptivos-generales
#| warning: false
#| tbl-cap: Descriptivos generales de los items actitudinales por batería

# Se excluyen las baterías que no son escalas actitudinales (demográficos, conjoint y escenarios de subvención), que están en otras métricas (años, pesos, categorías).
baterias_actitud <- setdiff(
  names(baterias),
  c("Demográficos inicio", "Demográficos (bloque 2)", "Conjoint", "Escenarios de subvención")
)

# Respuestas válidas de los items numéricos de una batería (se excluyen los códigos 98/99)
respuestas_bateria <- function(ids) {
  unlist(lapply(ids, function(v) {
    x <- df_desc[[v]]
    if (!es_numerica(x)) return(NULL)
    xn <- as.numeric(x)
    xn[!is.na(xn) & !xn %in% c(98, 99)]
  }))
}

# N de la batería: respondentes con al menos una respuesta válida en algún item numérico de la batería
n_respondentes_bateria <- function(ids) {
  ids_num <- ids[vapply(ids, function(v) es_numerica(df_desc[[v]]), logical(1))]
  valido <- sapply(ids_num, function(v) {
    xn <- suppressWarnings(as.numeric(df_desc[[v]]))
    !is.na(xn) & !xn %in% c(98, 99)
  })
  sum(rowSums(as.matrix(valido)) > 0)
}

fila_general <- function(nombre, ids) {
  r <- respuestas_bateria(ids)
  data.frame(
    bateria = nombre,
    n       = n_respondentes_bateria(ids),
    media   = round(mean(r), 2),
    mediana = round(median(r), 2),
    stringsAsFactors = FALSE
  )
}

tabla_general <- rbind(
  do.call(rbind, lapply(baterias_actitud, function(nm) fila_general(nm, baterias[[nm]]))),
  fila_general("Todas las baterías actitudinales", unlist(baterias[baterias_actitud], use.names = FALSE))
)

tabla_general |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", pageLength = nrow(tabla_general), scrollX = TRUE),
    colnames = c("Batería", "N", "Media", "Mediana")
  ) |>
  formatStyle(
    "bateria",
    target = "row",
    fontWeight = styleEqual("Todas las baterías actitudinales", "bold")
  )
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-mjp
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Market Justice Preferences

tabla_completa_bateria("Market Justice Preferences")
#
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-mjp-new
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Market Justice Reflexive

tabla_completa_bateria("Market Justice Reflexive")
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-mjp-tiempo
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Market Justice en el tiempo

tabla_completa_bateria("Market Justice en el tiempo")
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-comodifica
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Comodificación de servicios

tabla_completa_bateria("Comodificación de servicios")
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-lucro
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Lucro en servicios

tabla_completa_bateria("Lucro en servicios")
#
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-neoliberal
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Neoliberalismo

tabla_completa_bateria("Neoliberalismo")
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-nicera
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - NICERA

tabla_completa_bateria("NICERA")
#
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-adicional
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Adicionales

tabla_completa_bateria("Adicionales")
#
#
#
#
#
#
#
#
#| label: tbl-completo-demo1
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Demográficos inicio

tabla_completa_bateria("Demográficos inicio")
#
#
#
#
#
#
#
#| label: tbl-completo-demo2
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Demográficos (bloque 2)

tabla_completa_bateria("Demográficos (bloque 2)")
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-completo-conjoint
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Conjoint

tabla_completa_bateria("Conjoint")
#
#
#
#
#
#
#
#
#| label: tbl-completo-exp-comodifica
#| warning: false
#| tbl-cap: Tiempo de respuesta y descriptivos - Escenarios de subvención

tabla_completa_bateria("Escenarios de subvención")
#
#
#
#
#
#
#
#
#
#| warning: false
#| label: tbl-ranking
#| tbl-cap: Tiempos de respuesta Ranking de políticas

tiempo_ranking |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", pageLength = nrow(tiempo_ranking), scrollX = TRUE),
    colnames = c(
      "respID", "Precedente", "Educación", "Salud", "Pensiones",
      "Cuidado niños", "Carreteras", "Tiempo estimado (s)"
    )
  )
#
#
#
#
#
#
#
#
#
#| warning: false
#| label: tbl-ranking-desc
#| tbl-cap: Descriptivos Ranking de políticas

# Descriptivos de un conjunto de items numéricos de df (casos con consentimiento). Se excluyen los códigos 98/99 (No sé / Prefiero no responder).
descriptivos_items <- function(ids) {
  do.call(rbind, lapply(ids, function(v) {
    x <- suppressWarnings(as.numeric(df[[v]]))
    n_9899 <- sum(x %in% c(98, 99), na.rm = TRUE)
    x <- x[!is.na(x) & !x %in% c(98, 99)]
    data.frame(
      variable = v,
      label    = resumen$label[match(v, resumen$variable)],
      n        = length(x),
      n_9899   = n_9899,
      media    = round(mean(x), 2),
      mediana  = round(median(x), 2),
      sd       = round(sd(x), 2),
      min      = min(x),
      max      = max(x),
      stringsAsFactors = FALSE
    )
  }))
}

tabla_desc_items <- function(ids, prefijo = NULL) {
  tab <- descriptivos_items(ids)
  if (!is.null(prefijo)) tab$label <- paste0(prefijo[sub("_[0-9]+$", "", tab$variable)], ": ", tab$label)
  tab |>
    datatable(
      rownames = FALSE,
      options = list(dom = "t", pageLength = nrow(tab), scrollX = TRUE),
      colnames = c("Variable", "Label", "N", "NA (98/99)", "Media", "Mediana", "DE", "Mín", "Máx")
    ) |>
    formatStyle("sd", backgroundColor = styleInterval(1.0, c("white", "#f79b9b")))
}

ranking_ids <- c("ranking_politicas_educacion", "ranking_politicas_salud",
                 "ranking_politicas_pensiones", "ranking_politicas_cuidado_ninos",
                 "ranking_politicas_carreteras")

tabla_desc_items(ranking_ids)
#
#
#
#
#
#
#
#| warning: false
#| label: tbl-part
#| tbl-cap: Tiempos de respuesta batería de participación E/M

tiempos_part |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", pageLength = nrow(tiempos_part), scrollX = TRUE),
    colnames = c(
      "respID", "Precedente", "Primera pregunta part", "Última pregunta part",
      "Siguiente", "Tiempo aprox. batería (s)"
    )
  )
#
#
#
#
#
#
#
#
#| warning: false
#| label: tbl-part-desc
#| tbl-cap: Descriptivos batería de participación E/M (cargo, eficiencia y participación deseada)

tabla_desc_items(
  c(paste0("cargo_", 1:5), paste0("efic_", 1:5), paste0("part_", 1:5)),
  prefijo = c(cargo = "Quién participa más", efic = "Quién es más eficiente", part = "Quién debería participar más")
)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tiempos-merit
#| warning: false
#| tbl-cap: Tiempo de respuesta estimado - Meritocracia (EDUMERCO)

load(here::here("pre-pilot-analysis", "tbl/meritocracia.RData"))

merit_vars <- paste0("merit_0", 1:8)

carac_vars <- grep("^carac_a", names(meritocracia), value = TRUE)
des_vars   <- grep("^des_",    names(meritocracia), value = TRUE)
todas_vars <- c(carac_vars, merit_vars, des_vars)

merit_df <- meritocracia[, c("session_id", "created", "ended", todas_vars)]
#
#
#
#
#
merit_df <- merit_df[
  !is.na(merit_df$created) & !is.na(merit_df$ended) &
    stats::complete.cases(merit_df[, merit_vars]),
]

merit_df$created <- as.POSIXct(merit_df$created, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # transformación de string a fecha/hora
merit_df$ended   <- as.POSIXct(merit_df$ended,   format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

merit_df$duracion_total_seg    <- as.numeric(difftime(merit_df$ended, merit_df$created, units = "secs"))
merit_df$n_respondidas         <- rowSums(!is.na(merit_df[, todas_vars]))
merit_df$duracion_pregunta_seg <- merit_df$duracion_total_seg / merit_df$n_respondidas
#
#
#
#
#
merit_df <- merit_df[merit_df$duracion_pregunta_seg <= 300, ]

tiempo_merit_resumen <- data.frame(
  pregunta              = merit_vars,
  n                     = nrow(merit_df),
  duracion_media_seg    = round(mean(merit_df$duracion_pregunta_seg), 2),
  duracion_mediana_seg  = round(median(merit_df$duracion_pregunta_seg), 2),
  stringsAsFactors = FALSE
)
#
#
#
#
#
tiempo_merit_resumen |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", pageLength = nrow(tiempo_merit_resumen), scrollX = TRUE),
    colnames = c("Pregunta", "N", "Duración media (s)", "Duración mediana (s)")
  ) |>
  formatStyle(
    c("duracion_media_seg", "duracion_mediana_seg"),
    backgroundColor = styleInterval(30, c("white", "#f79b9b"))
  )
#
#
#
#
#
#
#
#
#
#| label: ranking y slider df
#| include: false


ranking_frame <- c(n_items = 5, duracion_mediana_total = 85)

sliders_frame <- c(n_items = 15, duracion_mediana_total = 31)
#
#
#
#| label: tbl-panorama-bateria
#| warning: false
#| tbl-cap: Panorama general de los tiempos de respuesta por batería temática

panorama_bateria <- do.call(rbind, lapply(names(baterias), function(nm) {
  ids <- baterias[[nm]]
  tab <- tiempo_pregunta_resumen[tiempo_pregunta_resumen$pregunta %in% ids, ]
  data.frame(
    bateria                = nm,
    n_items                = length(ids),
    duracion_mediana_total = round(sum(tab$duracion_mediana_seg), 2),
    stringsAsFactors = FALSE
  )
})) # se suma la duración mediana de las preguntas de cada batería temática, junto con su cantidad de ítems

panorama_bateria <- rbind(
  panorama_bateria,
  data.frame(
    bateria                = "Meritocracia",
    n_items                = length(merit_vars),
    duracion_mediana_total = round(sum(tiempo_merit_resumen$duracion_mediana_seg), 2),
    stringsAsFactors = FALSE
  )
) # se agrega la batería de meritocracia (estimada a partir de EDUMERCO) al panorama general

panorama_bateria$duracion_mediana_total <- round(panorama_bateria$duracion_mediana_total / 60, 2) # conversión de segundos a minutos

panorama_bateria <- rbind(
  panorama_bateria,
  data.frame(
    bateria                = "Ranking de políticas",
    n_items                = unname(ranking_frame["n_items"]),
    duracion_mediana_total = round(unname(ranking_frame["duracion_mediana_total"]) / 60, 2),
    stringsAsFactors = FALSE
  ),
  data.frame(
    bateria                = "Participación Estado/Mercado",
    n_items                = unname(sliders_frame["n_items"]),
    duracion_mediana_total = round(unname(sliders_frame["duracion_mediana_total"]) / 60, 2),
    stringsAsFactors = FALSE
  )
) # se agregan las baterías de ranking y sliders (estimadas por separado en el chunk "ranking y slider df")

seccion_1 <- c(
  "Demográficos inicio", "Market Justice Preferences", "Market Justice Reflexive",
  "Market Justice en el tiempo", "Comodificación de servicios", "Lucro en servicios",
  "Neoliberalismo", "NICERA", "Adicionales", "Demográficos (bloque 2)", "Meritocracia"
) # baterías del cuerpo principal de la encuesta

seccion_2 <- c(
  "Conjoint", "Escenarios de subvención", "Ranking de políticas", "Participación Estado/Mercado"
) # baterías correspondientes a módulos aparte (conjoint, escenarios y tareas interactivas)

panorama_seccion_1 <- panorama_bateria[match(seccion_1, panorama_bateria$bateria), ]
panorama_seccion_2 <- panorama_bateria[match(seccion_2, panorama_bateria$bateria), ]

subtotal_1 <- data.frame(
  bateria                = "Subtotal",
  n_items                = sum(panorama_seccion_1$n_items),
  duracion_mediana_total = round(sum(panorama_seccion_1$duracion_mediana_total), 2),
  stringsAsFactors = FALSE
) # subtotal de ítems y duración mediana de la primera sección

fila_total <- data.frame(
  bateria                = "Total",
  n_items                = sum(panorama_seccion_1$n_items, panorama_seccion_2$n_items),
  duracion_mediana_total = round(sum(panorama_seccion_1$duracion_mediana_total, panorama_seccion_2$duracion_mediana_total), 2),
  stringsAsFactors = FALSE
) # total general, sumando ambas secciones

fila_modulos <- data.frame(
  bateria                = "Módulos",
  n_items                = "N° de ítems",
  duracion_mediana_total = "Duración mediana total (min)",
  stringsAsFactors = FALSE
) # fila separadora que marca el inicio de la segunda sección (módulos aparte)

panorama_bateria <- rbind(
  panorama_seccion_1,
  subtotal_1,
  fila_modulos,
  panorama_seccion_2,
  fila_total
) # tabla final: sección 1, subtotal, separador de módulos, sección 2 y total general

panorama_bateria |>
  datatable(
    rownames = FALSE,
    options = list(dom = "t", pageLength = nrow(panorama_bateria)),
    colnames = c(
      "Batería temática", "N° de ítems", "Duración mediana total (min)"
    )
  ) |>
  formatStyle(
    "bateria",
    target = "row",
    fontWeight = styleEqual("Módulos", "bold")
  ) # se resalta la fila "Módulos", ya que solo replica los encabezados para marcar el inicio de la segunda sección
#
#
#
#
#
#
#
#
#
#
#
#
#
