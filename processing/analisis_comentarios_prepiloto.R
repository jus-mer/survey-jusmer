# Codebook / resumen descriptivo de comentarios-prepiloto.csv
# Construye, a partir de las respuestas crudas y de los labels definidos en
# surveys/pre-piloto/comentarios/survey.qmd, una tabla con:
#   - label de cada pregunta
#   - N de respuestas validas (no vacias)
#   - N de respuestas 98 / 99 / "No se" / "Prefiero no responder"
#   - promedio de respuestas validas (solo variables numericas)
#   - N de comentarios (variables que parten con "cmt" o terminan en "comment")
#   - una columna por cada comentario individual disponible en esas variables

input_csv <- "input/tbl/comentarios-prepiloto.csv"
qmd_path  <- "surveys/pre-piloto/comentarios/survey.qmd"
output_csv <- "output/comentarios-prepiloto-resumen.csv"

df <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE,
                colClasses = "character", encoding = "UTF-8")

# -----------------------------------------------------------------------------
# 1. Extraer mapa id -> label desde survey.qmd (parseando el codigo R, sin
#    ejecutarlo), replicando la numeracion automatica "(N) " que agrega el
#    wrapper de sd_question() para todo id que no empiece con "cmt_".
# -----------------------------------------------------------------------------

lines <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")

chunk_lines <- character(0)
in_chunk <- FALSE
for (ln in lines) {
  if (!in_chunk && grepl("^```\\{r", ln)) { in_chunk <- TRUE; next }
  if (in_chunk && grepl("^```\\s*$", ln)) { in_chunk <- FALSE; next }
  if (in_chunk) chunk_lines <- c(chunk_lines, ln)
}
code <- paste(chunk_lines, collapse = "\n")
exprs <- parse(text = code, keep.source = FALSE)

get_arg <- function(call_expr, name) {
  al <- as.list(call_expr)[-1]
  nm <- names(al)
  if (is.null(nm) || !(name %in% nm)) return(NULL)
  al[[name]]
}
as_chr_literal <- function(val) {
  if (is.null(val) || !is.character(val)) return(NA_character_)
  val
}

numbering_active <- FALSE
counter <- 0
ids <- character(0)
labels <- character(0)
rows_list <- list() # for type == "matrix": named vector of row labels, keyed by id

for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.symbol("<-"))) {
    lhs <- e[[2]]; rhs <- e[[3]]
    if (is.symbol(lhs) && identical(as.character(lhs), ".contador_preguntas") && identical(rhs, 0)) {
      numbering_active <- TRUE
    }
    if (is.symbol(lhs) && identical(as.character(lhs), "sd_question") &&
        is.symbol(rhs) && identical(as.character(rhs), ".sd_question_sin_numerar")) {
      numbering_active <- FALSE
    }
    next
  }
  if (is.call(e) && is.symbol(e[[1]]) && identical(as.character(e[[1]]), "sd_question")) {
    id_val    <- as_chr_literal(get_arg(e, "id"))
    label_val <- as_chr_literal(get_arg(e, "label"))
    if (is.na(id_val)) next
    if (numbering_active && !grepl("^cmt_", id_val)) {
      counter <- counter + 1
      if (!is.na(label_val)) label_val <- paste0("(", counter, ") ", label_val)
    }
    ids <- c(ids, id_val)
    labels <- c(labels, label_val)

    type_val <- get_arg(e, "type")
    row_val  <- get_arg(e, "row")
    if (!is.null(type_val) && is.character(type_val) && identical(type_val, "matrix") && !is.null(row_val)) {
      rv <- eval(row_val)
      rows_list[[id_val]] <- rv
    }
  }
}

map_df <- data.frame(id = ids, label = labels, stringsAsFactors = FALSE)
map_df <- map_df[!duplicated(map_df$id), ]

# Expandir preguntas tipo "matrix" (ej. ranking_politicas) en sub-columnas
# id_<fila>, tal como surveydown las escribe en la base de datos.
for (mid in names(rows_list)) {
  base_label <- map_df$label[map_df$id == mid]
  rv <- rows_list[[mid]]
  sub_ids    <- paste0(mid, "_", rv)
  sub_labels <- paste0(base_label, " - ", names(rv))
  map_df <- rbind(map_df, data.frame(id = sub_ids, label = sub_labels, stringsAsFactors = FALSE))
}

# La tarea de asignacion de becas (cbc_practice, cbc_q1..q6) se genera con el
# widget allocation_slider() (code/slider_widget.R), que llama a sd_question()
# con label = "" ; el enunciado real vive en el texto markdown de cada pagina.
cbc_prompt <- "Considerando las caracteristicas anteriores, cuanto dinero (beca) le daria a cada postulante?"
map_df <- rbind(
  map_df,
  data.frame(id = "cbc_practice", label = paste0(cbc_prompt, " (ejemplo de practica)"), stringsAsFactors = FALSE),
  data.frame(id = paste0("cbc_q", 1:6), label = paste0("(Tarea ", 1:6, ") ", cbc_prompt), stringsAsFactors = FALSE)
)

# Variables legadas: existen en la base porque el id fue renombrado en el
# survey.qmd (guion -> guion bajo) por compatibilidad con Shiny; conservan el
# mismo enunciado que su version vigente.
map_df <- rbind(
  map_df,
  data.frame(
    id = "id-correo",
    label = paste0(map_df$label[map_df$id == "id_correo"], " [variable legada, renombrada a id_correo]"),
    stringsAsFactors = FALSE
  ),
  data.frame(
    id = "feedback-2",
    label = paste0(map_df$label[map_df$id == "feedback_2"], " [variable legada, renombrada a feedback_2]"),
    stringsAsFactors = FALSE
  )
)

# -----------------------------------------------------------------------------
# 2. Seleccionar columnas "pregunta" (se excluyen metadatos de sesion y los
#    cronometros time_q_*/time_p_* que no son respuestas de contenido).
# -----------------------------------------------------------------------------

sys_cols <- c("session_id", "time_start", "time_end", "completion_code", "respID",
              "assigned_module", "browser", "ip_address", "cbc_profiles", "current_page")
qcols <- setdiff(names(df), sys_cols)
qcols <- qcols[!grepl("^time_q_|^time_p_", qcols)]

# -----------------------------------------------------------------------------
# 3. Calcular estadisticos por pregunta.
# -----------------------------------------------------------------------------

is_missing_code <- function(x) {
  xt <- trimws(x)
  xt_norm <- tolower(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", xt))
  xt %in% c("98", "99") | grepl("^no\\s*s[ée]$", xt_norm) | grepl("^prefiero no responder$", xt_norm)
}

is_comment_var <- function(col) {
  grepl("^cmt", col) | grepl("comment$", col) | col %in% c("feedback", "feedback_2", "feedback-2")
}

comment_cols <- qcols[is_comment_var(qcols)]
max_comments <- if (length(comment_cols) > 0) {
  max(sapply(comment_cols, function(cc) sum(df[[cc]] != "")))
} else 0

result <- lapply(qcols, function(col) {
  x <- df[[col]]
  non_empty <- x != ""
  n_valid <- sum(non_empty)

  miss_code <- non_empty & is_missing_code(x)
  n_missing_code <- sum(miss_code)

  # Valores "sustantivos": no vacios y no codigos de no-respuesta
  substantive <- x[non_empty & !miss_code]
  numeric_vals <- suppressWarnings(as.numeric(substantive))
  is_numeric <- length(substantive) > 0 && !any(is.na(numeric_vals))
  mean_valid <- if (is_numeric) mean(numeric_vals) else NA_real_

  is_comment <- is_comment_var(col)
  n_comments <- if (is_comment) n_valid else NA_integer_

  comment_texts <- if (is_comment) x[non_empty] else character(0)
  comment_vec <- rep(NA_character_, max_comments)
  if (length(comment_texts) > 0) {
    comment_vec[seq_along(comment_texts)] <- comment_texts
  }

  row <- c(
    list(
      variable = col,
      label = if (col %in% map_df$id) map_df$label[map_df$id == col][1] else NA_character_,
      n_valido = n_valid,
      n_no_sabe_no_responde = n_missing_code,
      promedio = round(mean_valid, 3),
      n_comentarios = n_comments
    ),
    setNames(as.list(comment_vec), paste0("comentario_", seq_len(max_comments)))
  )
  row
})

out_df <- do.call(rbind.data.frame, c(result, stringsAsFactors = FALSE))

dir.create(dirname(output_csv), showWarnings = FALSE, recursive = TRUE)
write.csv(out_df, output_csv, row.names = FALSE, na = "")

cat("Filas (preguntas):", nrow(out_df), "\n")
cat("Preguntas sin label encontrado:\n")
print(out_df$variable[is.na(out_df$label)])
cat("\nGuardado en:", output_csv, "\n")
