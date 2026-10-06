A minimum template for starting from scratch.

To create this template, run this command in your R console:

```r
surveydown::sd_create_survey(
  #path = "path/to/survey"
)
```

There is a `template = "default"` argument in the `sd_create_survey()` function, but it is set as default so no need to specify it.

Refer to the [Start with a template](https://surveydown.org/docs/getting-started#start-with-a-template) section for more details.




## Datos clave conexión a supabase:

- si aparece esto: db <- sd_db_connect(ignore = TRUE), es porque no se ha configurado correctamente la conexión a Supabase, y por lo tanto no se están guardando los datos de respuestas online. 

- En survey.qmd, en el yaml survey settings, add:  set-mode: database

Activé conexión a base de datos por defecto en app.R:30.
Antes estaba fijo en modo no guardado (ignore = TRUE).
Ahora usa una variable de entorno:

SD_IGNORE_DB=false (o vacío): guarda en la base configurada (Supabase).
SD_IGNORE_DB=true: no guarda (modo pruebas locales).
Protegí credenciales para que no se suban por error en .gitignore:15.
Se ignorarán .env y .Renviron (también en subcarpetas).
Estado de validación

No aparecieron errores nuevos por estos cambios.
Siguen avisos previos de lint en app.R, pero no están relacionados con Supabase.
Siguiente paso para que efectivamente escriba en Supabase

Configurar credenciales una vez con surveydown::sd_db_config() en el entorno donde corre la app.
Verificar que SD_IGNORE_DB no esté en true en producción.
Reiniciar la app desplegada.

## SUpabase

Supabase funciona en base a proyectos.Un mismo proyecto puede tener varias bases de datos, y cada base de datos puede tener varias tablas. En este caso, se creó un proyecto llamado survey-jusmer, con una base de datos llamada prepilot.

organizaión: JUSMER
ingreso con Github account
use: educational

project name: survey-jusmer
database password prepilot: jusmer-prepilot

Agregar integrantes menú barra a la izquierda Teams

Una vez configurado el proyecto en Supabase, seguir indicaciones de la documentación de surveydown: https://surveydown.org/docs/storing-data, pero en resumen

- ir a connect
- Direct -> marcar transaction pooler, abajo se generan credenciales
- ir al proyecto/carpeta donde está la app y ejecutar en R: surveydown::sd_db_config()
- ingresar credenciales que aparecen en Supabase (host, user, password, database), también table (por defecto, responses) y la password de esta base de datos ( jusmer-prepilot)

- Esto genera un .env file con los datos de conexión a la base de datos, que se debe proteger para que no se suba a repositorios públicos (agregarlo a .gitignore).


Creat policy for data: 
in sql: 
auth.role() = 'authenticated'

Agregar arriba en la app.R: 
  library(surveydown)

  # Connects to database
  db <- sd_db_connect()
