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
Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(surveydown)
library(dplyr)
library(kableExtra)
library(htmltools)
library(here)
source("code/slider_widget.R")
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
sd_question(
  type  = 'mc',
  id    = 'consent_understand',
  label = "Acepto participar en esta encuesta y entiendo que mis respuestas serán anónimas y utilizadas únicamente con fines de investigación.",
  option = c(
    '**Sí**' = 'yes',
    '**No**'  = 'no'
  )
)

sd_question(
  type  = "textarea",
  id    = "id_correo",
  label = "Agradeceríamos que nos dejara su nombre y correo electrónico en caso de que queramos contactarlo para hacerle preguntas respecto a sus comentarios y recomendaciones sobre la encuesta. Sus respuestas seguirán siendo anónimas y confidenciales",
)

sd_nav(
  page_next = "demographics",
  label_next = "Siguiente"
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

sd_question(
  type   = 'mc',
  id     = 'respondent',
  label  = "¿Desde qué dispositivo responde esta encuesta?",
  option = c(
    "Celular" = "celular",
    "Computador" = "pc",
    "Tablet" = "tablet"
  )
)

sd_question(
  type   = 'mc',
  id     = 'sexo',
  label  = "¿Cuál es su sexo?",
  option = c(
    "Masculino" = "0",
    "Femenino" = "1",
    "Otro" = "99"
  )
)

# Construct the list of years
 
years <- as.character(2008:1920)
names(years) <- years
years <- c("Prefiero no decir" = "prefer_not_say", years)

sd_question(
  type   = 'select',
  id     = 'year_of_birth',
  label  = "¿En qué año nació?",
  option = years
)

educations <- c(
  "Sin estudios"       = "0",
  "Educación básica incompleta"          = "1",
  "Educación básica completa" = "2",
  "Educación media incompleta" = "3",
  "Educación media completa" = "4",
  "Técnica superior incompleta (instituto o CFT)"    = "5",
  "Técnica superior completa (instituto o CFT)"    = "6",
  "Universitaria incompleta" = "7",
  "Universitaria completa"             = "8",
  "Estudios de posgrado (magíster o doctorado)"                 = "9",
  "Prefiero no decir"        = "prefer_not_to_say"
)

sd_question(
  type   = 'select',
  id     = 'education',
  label  = "¿Cuál es el nivel educativo más alto que ha completado? Si actualmente está estudiando, use el grado más alto que haya obtenido.",
  option = educations
)

sd_question(
  type  = "textarea",
  id    = "feedback",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(
  page_next = "cbc_practice-page",
  label_next = "Siguiente"
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
# Make a fixed data frame for the options

df <- tibble::tibble(
  altID       = c(1, 2),
  need        = c("Dificultad", "Holgura"),
  identity    = c("Chile", "Chile"),
  control     = c("Postuló a otras becas pero no obtuvo financiamiento", "No alcanzó a postular a tiempo a otras becas"),
  effort      = c("Más que sus compañeros", "Igual que sus compañeros"),
  reciprocity = c("Ha hecho voluntariado", "No ha hecho voluntariado"),
  attitude    = c("Una ayuda que agradece", "Una ayuda que agradece"),
)

male_names <- c("Mateo", "Lucas", "Benjamín", "Nicolás", "Daniel", "Santiago", "Tomás", "Joaquín")
female_names <- c("Sofía", "Valentina", "Isidora", "Martina", "Camila", "Florencia", "Catalina", "Antonia")
practice_names <- sample(c(sample(male_names, 1), sample(female_names, 1)), 2)

df <- df |>
  mutate(nombre = practice_names[altID])

alts <- df |>
  mutate(
    nombre_formatted = ifelse(
      altID == 1,
      sprintf('<span style="color: #28a745; font-weight: 800;">%s</span>', nombre),
      sprintf('<span style="color: #007bff; font-weight: 800;">%s</span>', nombre)
    )
  ) |>
  # Make nicer attribute labels
  select(
    `Postulante:` = nombre_formatted,
    `Su hogar llega a fin de mes con:` = need,
    `País de nacimiento:` = identity,
    `Requiere la beca porque:` = control,
    `Estudia:` = effort,
    `Fuera de sus estudios:` = reciprocity,
    `Ve la beca como:` = attitude
  )

row.names(alts) <- NULL # Drop row names

kbl(t(alts), escape = FALSE) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) |>
  column_spec(1, bold = TRUE)

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_practice", nombre1 = practice_names[1], nombre2 = practice_names[2])

htmltools::HTML("<br>")


#
#
#
#
#
sd_question(
  type  = "textarea",
  id    = "cbc_practice_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_intro",
  label_previous = "Anterior",
  label_next = "Siguiente"
)

#
#
#
#
#
#
#
#

sd_question(
  type = "mc",
  id = "manip_check",
  label = "Antes de comenzar, ¿en qué consistirá su tarea en esta sección?",
  option = c(
    "Repartir un monto de beca entre dos postulantes" = "reparto",
    "Elegir a un único postulante y descartar al otro" = "eleccion",
    "Evaluar la calidad de la universidad de cada postulante" = "universidad"
  )
)

sd_nav(
  page_next = "cbc_q1_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc1_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q1")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q1_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_q2_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc2_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q2")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q2_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_q3_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc3_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q3")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q3_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_q4_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc4_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q4")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q4_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_q5_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc5_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q5")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q5_comment",
  label = "Comentarios"
)

sd_nav(
  page_next = "cbc_q6_page",
  label_previous = "Anterior",
  label_next = "Siguiente"
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
sd_output("cbc6_table")

shiny::p(
  "Considerando las características anteriores, ¿cuánto dinero (beca) le daría a cada postulante?",
  style = "text-align: center; font-weight: 600; margin: 10px 0 8px 0;"
)

allocation_slider("cbc_q6")

htmltools::HTML("<br>")

sd_question(
  type  = "textarea",
  id    = "cbc_q6_comment",
  label = "Comentarios"
)
#
#
#
sd_nav(page_next = "clasicos",
  label_next = "Siguiente")
#
#
#
# Numeración automática de preguntas: desde "clasicos" hasta el final de
# "deserve-4". Cada sd_question() recibe un número correlativo antepuesto
# al label, salvo las cajas de comentario libre (id que empieza con "cmt_").
# Se restaura sd_question() sin numeración antes de la página "id-final".
.sd_question_sin_numerar <- sd_question
.contador_preguntas <- 0
sd_question <- function(...) {
  args <- list(...)
  es_comentario <- grepl("^cmt_", args$id)
  if (!es_comentario) {
    .contador_preguntas <<- .contador_preguntas + 1
    # Se usa "(N)" en vez de "N." porque el label pasa por un parser de
    # markdown que interpreta "1. texto" como el inicio de una lista
    # numerada (y la reinicia en 1 en cada pregunta). "(N)" sigue el mismo
    # formato ya usado en demographics/id-final y no se confunde con markdown.
    args$label <- paste0("(", .contador_preguntas, ") ", args$label)
  }
  do.call(.sd_question_sin_numerar, args)
}
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
escala_acuerdo_nsnr <- c(
  'Muy de acuerdo'    = '5',
  'De acuerdo' = '4',
  'Ni de acuerdo ni en desacuerdo'    = '3',
  'En desacuerdo' = '2',
  'Muy en desacuerdo' = '1',
  'No sé' = '98',
  'Prefiero no responder' = '99'
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'mj_1',
  label = "Es justo que en Chile las personas de altos ingresos puedan acceder a una mejor atención de salud que las personas con ingresos más bajos",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'mj_2',
  label = "Es justo que en Chile las personas de altos ingresos tengan una mejor educación para sus hijos que las personas con ingresos más bajos",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'mj_3',
  label = "Es justo que en Chile las personas de altos ingresos tengan mejores pensiones que las personas con ingresos más bajos",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = "textarea",
  id    = "cmt_clasicos",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "reflex",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'ref_1',
  label = "Es justo que **yo** tenga acceso a una mejor salud si puedo pagar por ello",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'    
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'ref_2',
  label = "Es justo que **yo** tenga acceso a una mejor educación si puedo pagar por ello",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'ref_3',
  label = "Es justo que **yo** tenga acceso a mejor pensión si puedo pagar por ello",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'proy_1',
  label = "Está bien que **mis hijos/as** tengan acceso a una mejor salud que otros niños si es que puedo pagar por ello",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'proy_2',
  label = "Está bien que **mis hijos/as** tengan acceso a una mejor educación que otros niños si es que puedo pagar por ello",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)

sd_question(
  type = "mc",
  id = "attention_check",
  label = "Esta pregunta es para verificar que está leyendo con atención. Por favor, deje esta pregunta en blanco",
  option = c(
    "Muy de acuerdo" = "muy_acuerdo",
    "De acuerdo" = "acuerdo",
    "En desacuerdo" = "desacuerdo",
    "Muy en desacuerdo" = "muy_desacuerdo"
  )
)

sd_question(
  type  = "textarea",
  id    = "cmt_reflex",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "ranking",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type   = "matrix",
  id     = "ranking_politicas",
  label  = "Asigne una **prioridad** a cada área de inversión de fondos públicos:",
  row    = c(
    "Educación"        = "educacion",
    "Salud"            = "salud",
    "Pensiones"        = "pensiones",
    "Salas cunas" = "cuidado_ninos",
    "Carreteras" = "carreteras"
  ),
  option = c(
    "1°" = "1",
    "2°" = "2",
    "3°" = "3",
    "4°" = "4",
    "5°" = "5"
  )
)
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'rank_1',
  label = "Educación",
  option = c(
    'Mucho más gasto'    = '5',
    'Más gasto' = '4',
    'Ni más ni menos de lo que se gasta actualmente'   = '3',
    'Menos gasto' = '2',
    'Mucho menos gasto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'rank_2',
  label = "Salud",
  option = c(
    'Mucho más gasto'    = '5',
    'Más gasto' = '4',
    'Ni más ni menos de lo que se gasta actualmente'     = '3',
    'Menos gasto' = '2',
    'Mucho menos gasto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'rank_3',
  label = "Pensiones",
  option = c(
    'Mucho más gasto'    = '5',
    'Más gasto' = '4',
    'Ni más ni menos de lo que se gasta actualmente'    = '3',
    'Menos gasto' = '2',
    'Mucho menos gasto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'rank_4',
  label = "Salas cuna",
  option = c(
    'Mucho más gasto'    = '5',
    'Más gasto' = '4',
    'Ni más ni menos de lo que se gasta actualmente'    = '3',
    'Menos gasto' = '2',
    'Mucho menos gasto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'rank_5',
  label = "Carreteras",
  option = c(
    'Mucho más gasto'    = '5',
    'Más gasto' = '4',
    'Ni más ni menos de lo que se gasta actualmente'    = '3',
    'Menos gasto' = '2',
    'Mucho menos gasto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_ranking",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "marketizacion",
  label_next = "Siguiente")
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
escala_justo <- c(
  'Muy desigual' = '5',
  'Desigual' = '4',
  'Ni igual ni desigual' = '3',
  'Poco desigual' = '2',
  'Nada desigual' = '1',
  'No sé' = '98',
  'Prefiero no responder' = '99'
)

escala_justo_injusto <- c(
  'Mucho más desigual' = '5',
  'Más desigual' = '4',
  'Ni igual ni desigual' = '3',
  'Menos desigual' = '2',
  'Mucho menos desigual' = '1',
  'No sé' = '98',
  'Prefiero no responder' = '99'
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'pres_1',
  label = "El acceso a la salud es desigual en Chile",
  option = escala_justo
)

sd_question(
  type  = 'mc',
  id    = 'pres_2',
  label = "El acceso a la educación es desigual en Chile",
  option = escala_justo
)

sd_question(
  type  = 'mc',
  id    = 'pres_3',
  label = "Las pensiones son desiguales en Chile",
  option = escala_justo
)
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'pres_4',
  label = "Salud",
  option = escala_justo_injusto
)

sd_question(
  type  = 'mc',
  id    = 'pres_5',
  label = "Educación",
  option = escala_justo_injusto
)

sd_question(
  type  = 'mc',
  id    = 'pres_6',
  label = "Pensiones",
  option = escala_justo_injusto
)

sd_question(
  type  = "textarea",
  id    = "cmt_percepcion_acceso",
  label = "Comentarios (opcional)"
)
#
#
#
#
sd_nav(page_next = "valor",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'valor_1',
  label = "Está bien que los hospitales públicos sean administrados por privados si ellos son capaces de brindar un mejor servicio",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = 'mc',
  id    = 'valor_2',
  label = "Está bien que los liceos públicos sean administrados por privados si ellos son capaces de brindar un mejor servicio",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = 'mc',
  id    = 'valor_3',
  label = "La calidad del sistema de salud debería ser igual para todos aunque esto signifique que mi familila no pueda pagar por mejores servicios ",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = 'mc',
  id    = 'valor_4',
  label = "La educación debería ser de igual calidad para todos aunque eso me impida pagar por una mejor para mi familia",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = 'mc',
  id    = 'valor_5',
  label = "Las pensiones deberían ser más igualitarias, aunque eso signifique que yo reciba una pensión similar a quienes aportaron menos que yo ",
  option = escala_acuerdo_nsnr
)

sd_question(
  type  = "textarea",
  id    = "cmt_valor",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "decomodifica",
  label_next = "Siguiente")
#
#
#
#
#
# Cada módulo de respuesta se numera desde (1), ya que un respondiente solo
# ve uno de los tres módulos (ver assigned_module en app.R).
.contador_preguntas <- 0
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
sd_question(
  type  = 'slider',
  id    = 'dec_3',
  label = "Imagine que usted está en un plan de Isapre que está evaluando dar acceso a un 10% de personas en su mismo plan que no pueden pagarlo. Para ello, usted debería pagar un monto adicional a su cotización. En tal situación, ¿cuánto más estaría dispuesto a pagar?",
  option = c(
    "0" = "0",
    "10%"      = "10",
    "20%"   = "20",
    "30%"      = "30",
    "40%" = "40",
    "50%" = "50",
    "60%" = "60",
    "70%" = "70",
    "80%" = "80",
    "90%" = "90",
    "100%" = "100"
  ),
  selected = '0'
)
#
#
#
#
#
#
#
sd_question(
  type = "slider",
  id = "dec_1",
  label = "Imagine que su hijo/a asiste a un colegio privado que tiene una matrícula de 100 alumnos. ¿Cuántos alumnos cree Ud. que se deberían admitir sin pagar en este colegio? ",
  option = c(
    "0" = "0",
    "10" = "10",
    "20" = "20",
    "30" = "30",
    "40" = "40",
    "50" = "50",
    "60" = "60",
    "70" = "70",
    "80" = "80",
    "90" = "90",
    "100" = "100"
  ),
  selected = '0'
)
#
#
#
shiny::actionButton("confirm_dec1", "Confirmar respuesta", class = "btn btn-primary mt-2 mb-3")
#
#
#
sd_question(
  type  = 'slider',
  id    = 'dec_2',
  label = "Para que esos alumnos puedan ser admitidos en el colegio, ¿cuánto más estaría dispuesto a pagar de la mensualidad para poder apoyar a estudiantes que no pagan?",
  option = c(
    "0" = "0",
    "10%"      = "10",
    "20%"   = "20",
    "30%"      = "30",
    "40%" = "40",
    "50%" = "50",
    "60%" = "60",
    "70%" = "70",
    "80%" = "80",
    "90%" = "90",
    "100%" = "100"
  ),
  selected = '0'
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_decomodifica",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "lucro",
  label_next = "Siguiente")
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
sd_question(
  type  = 'mc',
  id    = 'lucro_1',
  label = "Salud",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'lucro_2',
  label = "Educación",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'lucro_3',
  label = "Pensiones",
  option = c(
    'Muy justo'    = '5',
    'Justo' = '4',
    'Ni justo ni injusto'    = '3',
    'Injusto' = '2',
    'Muy injusto' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)

sd_question(
  type  = "textarea",
  id    = "cmt_rol_privados_estado",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "participa",
  label_next = "Siguiente")
#
#
#
#
#
#
#
#
#
# NOTA: se cambia el rótulo de los extremos del slider de "Estado"/"Privados"
# a "Solo el Estado"/"Solo privados" en las tres baterías, según documento de
# contenido (nota de diseño: equiparar el fraseo de forma simétrica). Se
# elimina la batería de "responsabilidad" (resp_1 a resp_5) y se reordenan
# las baterías: cargo, eficiencia, participación deseada.

escala_estado_privados <- c(
  "Estado" = "1",
  " "        = "2",
  "  "       = "3", # espacio único
  "Ambos por igual"  = "4",
  "   "      = "5",
  "    "     = "6",  # doble espacio único
  "Privados" = "7"
)
#
#
#
#
#
sd_question(
  type   = 'slider',
  id     = 'cargo_1',
  label  = "Salud",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'cargo_2',
  label  = "Educación",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'cargo_3',
  label  = "Pensiones",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'cargo_4',
  label  = "Salas cuna",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'cargo_5',
  label  = "Carreteras",
  option = escala_estado_privados,
  selected = '4'
)
#
#
#
#
#
#
#
sd_question(
  type   = 'slider',
  id     = 'efic_1',
  label  = "Salud",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'efic_2',
  label  = "Educación",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'efic_3',
  label  = "Pensiones",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'efic_4',
  label  = "Salas cuna",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'efic_5',
  label  = "Carreteras",
  option = escala_estado_privados,
  selected = '4'
)
#
#
#
#
#
#
#
sd_question(
  type   = 'slider',
  id     = 'part_1',
  label  = "Salud",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'part_2',
  label  = "Educación",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'part_3',
  label  = "Pensiones",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'part_4',
  label  = "Salas cuna",
  option = escala_estado_privados,
  selected = '4'
)

sd_question(
  type   = 'slider',
  id     = 'part_5',
  label  = "Carreteras",
  option = escala_estado_privados,
  selected = '4'
)
#
#
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_participa",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "neoliberal",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'neolib_1',
  label = "El Gobierno debería proporcionar a toda la población servicios básicos como la asistencia sanitaria y la asistencia jurídica de forma gratuita",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'neolib_2',
  label = "Si el Gobierno tiene que aumentar su deuda para ayudar a la gente, debería hacerlo",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'neolib_3',
  label = "La sociedad debería proporcionar recursos y servicios gratuitos a las personas que no pueden permitírselos",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'neolib_4',
  label = "Las personas con ingresos más elevados deberían pagar más impuestos que las que tienen ingresos más bajos",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_neoliberal",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "deserve-1",
  label_next = "Siguiente")
#
#
#
#
#
# Cada módulo de respuesta se numera desde (1), ya que un respondiente solo
# ve uno de los tres módulos (ver assigned_module en app.R).
.contador_preguntas <- 0
#
#
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'control_1',
  label = "Las personas que caen en la pobreza por sus propios errores deberían igualmente tener derecho a recibir apoyo del Estado",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'control_2',
  label = "Quienes son responsables de sus propios problemas igualmente merecen recibir beneficios sociales.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'control_3',
  label = "El Estado debería ayudar a las personas independientemente de si su situación fue causada por decisiones propias o por factores externos.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'actitud_1',
  label = "Las personas que reciben beneficios del Estado deberían mostrar más gratitud por la ayuda que reciben",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'actitud_2',
  label = "Está bien que las personas que reciben apoyo del Estado puedan expresar su descontento con las condiciones de los beneficios.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_deserve-1",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "deserve-2",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'reciproc_1',
  label = "Los beneficios sociales deberían estar reservados para quienes han contribuido previamente al sistema, por ejemplo cotizando o pagando impuestos",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'reciproc_2',
  label = "No es justo que personas que nunca han contribuido al sistema reciban los mismos beneficios que quienes sí lo han hecho.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'reciproc_3',
  label = "Quienes han trabajado y aportado al sistema durante más tiempo merecen mejores beneficios sociales que quienes han contribuido poco o nada",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'actitud_4',
  label = "El acceso a los beneficios sociales debería ser el mismo para todos, independientemente de si han cotizado o no en el pasado.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'id_1',
  label = "Al momento de asignar beneficios sociales, las personas nacidas en Chile deberían tener prioridad por sobre quienes llegaron de otros países.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'id_2',
  label = "Los migrantes deberían tener acceso a los mismos beneficios sociales que los chilenos.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'id_3',
  label = "Solo quienes llevan varios años viviendo en Chile deberían poder acceder a los programas de protección social del Estado.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_deserve-2",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "deserve-3",
  label_next = "Siguiente")
#
#
#
#
#
#
#
#
sd_question(
  type  = 'mc',
  id    = 'nec_1',
  label = "Los beneficios sociales deberían estar reservados exclusivamente para quienes viven en situación de pobreza real.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'nec_2',
  label = "Las personas que tienen suficientes recursos económicos propios no deberían recibir apoyo del Estado.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'nec_3',
  label = "Solo quienes se encuentran en situación de extrema necesidad deberían poder acceder a los beneficios sociales.",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'esf_1',
  label = "Las personas que reciben beneficios sociales deberían estar haciendo algo activamente para mejorar su situación, como buscar trabajo o capacitarse",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'esf_2',
  label = "Quienes reciben ayuda del Estado pero no hacen ningún esfuerzo por salir adelante merecen menos apoyo que quienes sí lo intentan",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'esf_3',
  label = "El Estado debería apoyar a las personas aunque no estén haciendo ningún esfuerzo visible por cambiar su situación",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'esf_4',
  label = "Es razonable que la duración de un beneficio social dependa del esfuerzo que está haciendo la persona para dejar de necesitarlo",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'esf_5',
  label = "No importa si alguien está buscando trabajo o no: si necesita ayuda, el Estado debería dársela igual",
  option = escala_acuerdo_nsnr
)
#
#
#
sd_question(
  type  = "textarea",
  id    = "cmt_deserve-3",
  label = "Comentarios (opcional)"
)
#
#
#
sd_nav(page_next = "deserve-4",
  label_next = "Siguiente")
#
#
#
#
#
#
#
sd_question(
  type   = 'mc',
  id     = 'adc_qstn',
  label  = "¿Cuál de las siguientes afirmaciones describe mejor su situación actual, considerando el total de ingresos que tiene actualmente?",
  option = c(
    'Nos alcanza bien, hasta podemos ahorrar' = '4',
    'Nos alcanza justo y podemos darnos algunos gustos sin mayores problemas'      = '3',
    'Nos alcanza con dificultad, ajustando gastos '       = '2',
    'No nos alcanza, tenemos grandes problemas'      = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
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
sd_question(
  type  = 'mc',
  id    = 'adc_5',
  label = "Que mi situación financiera empeore los próximos años.",
  option = c(
    'A menudo' = '5',
    'Regularmente' = '4',
    'A veces' = '3',
    'Raramente' = '2',
    'Nunca' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'adc_6',
  label = "Quedar desempleado",
  option = c(
    'A menudo' = '5',
    'Regularmente' = '4',
    'A veces' = '3',
    'Raramente' = '2',
    'Nunca' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_question(
  type  = 'mc',
  id    = 'adc_7',
  label = "Que mis hijos/as y las próximas generaciones lo tendrán mucho más difícil",
  option = c(
    'A menudo' = '5',
    'Regularmente' = '4',
    'A veces' = '3',
    'Raramente' = '2',
    'Nunca' = '1',
    'No sé' = '98',
    'Prefiero no responder' = '99'
  )
)
#
#
#
sd_nav(page_next = "id-final",
  label_next = "Siguiente")
#
#
#
# Fin de la numeración automática de preguntas; se restaura sd_question()
# original para las preguntas adicionales de esta página.
sd_question <- .sd_question_sin_numerar
#
#
#
#
#
#
#
#
#

genders <- c(
  "Hombre"                   = "male",
  "Mujer"                    = "female", 
  "No binario"               = "non_binary", 
  "Hombre trans"             = "trans_male",
  "Mujer trans"              = "trans_female",
  "Prefiero no responder"        = "99"
)

sd_question(
  type   = 'select',
  id     = 'gender',
  label  = "¿Cuál es su identidad de género?",
  option = genders
)

incomes <- c(
  "$3.100.000 o más mensuales"          = "10",
  "De $2.100.000 a $3.099.999 mensuales" = "9",
  "De $1.600.000 a $2.099.999 mensuales"        = "8",
  "De $1.300.000 a $1.599.999 mensuales"        = "7",
  "De $1.050.000 a $1.299.999 mensuales"      = "6",
  "De $850.000 a $1.049.999 mensuales"    = "5",
  "De $680.000 a $849.999 mensuales"    = "4",
  "De $520.000 a $679.999 mensuales"    = "3",
  "De $345.000 a $519.999 mensuales"    = "2",
  "Hasta $344.999 mensuales"          = "1",
  "No sé" = '98',
  "Prefiero no responder"        = "99"
)

sd_question(
  type   = 'select',
  id     = 'income',
  label  = "¿Cuál es su ingreso anual del hogar (de todas las fuentes) antes de impuestos y otras deducciones?",
  option = incomes
)

sd_question(
  type   = 'mc',
  id     = 'salud_prov',
  label  = "¿Qué plan de salud tiene?",
  option = c(
    "FONASA" = "fonasa",
    "ISAPRE" = "isapre"
  )
)

sd_question(
  type   = 'mc',
  id     = 'educ_prov',
  label  = "¿En qué tipo de colegio estudió?",
  option = c(
    "Particular privado" = "privado",
    "Particular subvencionado" = "subvencionado",
    "Municipal" = "municipal"
  )
)

sd_question(
  type  = "textarea",
  id    = "feedback_2",
  label =
  "¿Tiene algún comentario u observación sobre esta encuesta? Puede ingresar cualquier observación de forma, contenido o diseño en el cuadro a continuación."
)
#
#
#
#
#
sd_nav(page_next = "end_normal",
  label_next = "Siguiente")
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
sd_close()
#
#
#
#
#
#
#
#
#
sd_close()
#
#
#
