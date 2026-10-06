# Deploy a Posit Connect Cloud — encuesta "sin_comentarios"

Documenta el procedimiento completo para desplegar este survey (surveydown:
`app.R` + `survey.qmd`) en **Posit Connect Cloud**, y cómo queda conectado a la
base de datos en Supabase. Complementa las notas existentes sobre Supabase en
[README.md](README.md) y el procedimiento equivalente para Hugging Face en
[HUGGINGFACE.md](HUGGINGFACE.md).

## 1. Prerrequisitos

### 1.1 R y el paquete `rsconnect`

Connect Cloud necesita `rsconnect` **>= 1.6.0** (versiones anteriores solo
apuntan a shinyapps.io):

```r
install.packages("rsconnect")
```

`rsconnect` también necesita que **todos** los paquetes que usa el survey estén
instalados localmente — corre el survey en local, así que ya lo están —, porque
empaqueta las dependencias desde la librería local (no usa Dockerfile ni
`packages.txt`, a diferencia de Hugging Face).

### 1.2 Iniciar sesión en Connect Cloud (una sola vez)

El login es interactivo (abre un navegador), así que debe correrse en **tu
propia consola de R** — no se puede automatizar desde un script ni un chat:

```r
rsconnect::connectCloudUser()
```

Se abre una ventana al navegador con `login.posit.cloud`; hay que autorizar ahí
(eligiendo la cuenta si se pide). El token OAuth queda guardado donde
`rsconnect` lo busca (`~/.config/R/rsconnect/accounts/connect.posit.cloud/`), y
el script de deploy lo reutiliza automáticamente — nunca se vuelve a pasar a
mano, ni se imprime, ni se guarda en un archivo editable.

**Importante:** el login debe correr con el mismo R que usará el script de
deploy (`Rscript`, el R del sistema) — **no** en la consola de R Interactive de
VS Code ni de RStudio, que pueden apuntar a una instalación de R distinta con
su propia carpeta de configuración. Para verificar que el login quedó en el
lugar correcto:

```bash
Rscript -e 'rsconnect::accounts()'
# debe listar una fila con server = connect.posit.cloud
```

Si no aparece, correr el login directamente desde una terminal plana:

```bash
Rscript -e 'rsconnect::connectCloudUser()'
```

## 2. Cómo se conecta la app a Supabase

Igual que en Hugging Face (ver [HUGGINGFACE.md § 2](HUGGINGFACE.md)): `app.R`
siempre intenta conectarse a la base de datos configurada
(`db <- sd_db_connect(ignore = ignore_db)`), salvo que la variable de entorno
`SD_IGNORE_DB=true` esté seteada. Esto es independiente del campo YAML
`set-mode: database` en `survey.qmd` (esa clave no existe en surveydown, no
tiene efecto — ver la nota en HUGGINGFACE.md § 2.3).

Para que el script de deploy a Connect Cloud detecte automáticamente que debe
subir las credenciales, `survey.qmd` también debe tener la clave **real**:

```yaml
survey-settings:
  set-mode: database   # nota histórica, sin efecto en surveydown
  mode: database        # clave real — el script de deploy la lee para decidir
                         # si sube las credenciales SD_* como secrets
```

Las credenciales locales viven en el `.env` de esta carpeta (generado una vez
con `surveydown::sd_db_config()`, protegido en `.gitignore`, nunca se sube al
repo ni se empaqueta en el deploy).

## 3. Deploy paso a paso

Con el login de Connect Cloud ya hecho (paso 1), desde esta carpeta del survey
(o pasando `--dir`):

```bash
~/.claude/skills/surveydown-skill/deploy-posit-cloud/deploy.sh \
  --title "Pre-piloto Encuesta JUSMER (tiempo)" \
  --slug pre-piloto-v2 \
  --dir surveys/pre-piloto/sin_comentarios
```

Qué hace, en orden:

1. Valida `rsconnect` (versión, cuenta de Connect Cloud registrada) y lee la
   clave `mode:` de `survey.qmd`.
2. Si `mode: database` y existe un `.env` real en la carpeta, carga las 6
   variables `SD_HOST/PORT/DBNAME/USER/TABLE/PASSWORD` para subirlas como
   **content secrets** (nunca se imprimen).
3. **Reconstruye `_survey/`** (borra y vuelve a renderizar con `sd_ui()` +
   `run_config()`) para que el primer arranque en el servidor sea rápido
   (import de caché en vez de re-renderizar Quarto).
4. Agrega temporalmente un `.Rprofile` gestionado (se retira automáticamente
   al terminar, no queda en la copia local) para que ese `_survey/` se importe
   de forma determinística en un cold start.
5. Empaqueta los archivos del survey (excluye `.git/`, `.env`, `.Renviron`,
   `preview_data.csv`, el `survey.html`/`survey_files/` sueltos de la raíz,
   etc.) y corre `rsconnect::deployApp()` contra `connect.posit.cloud`.
6. Fija el **título visible** y el **acceso público** vía la API de Connect
   Cloud.
7. Fija la **URL personalizada** (`--slug`) y republica si hace falta para que
   la URL personalizada quede activa de inmediato.
8. Informa el uso de cupo (`N/5` aplicaciones en el plan gratuito).
9. Verifica la URL pública con una petición HTTP y la reporta junto al enlace
   al dashboard.

URL resultante:
`https://<cuenta>-<slug>.share.connect.posit.cloud/`
(en este caso: <https://jus-mer-pre-piloto-v2.share.connect.posit.cloud/>)

Dashboard de este contenido:
<https://connect.posit.cloud/jus-mer/content/019f6772-6c05-4512-b3df-ee334461ecf1>

Un primer publish tarda unos **pocos minutos** (Connect Cloud instala las
dependencias de R de forma remota). Los redeploys son más rápidos.

## 4. Verificación post-deploy

1. Abrir la URL pública y completar una respuesta de prueba.
2. En Supabase → Table editor → tabla `responses` (o el valor de `SD_TABLE`),
   confirmar que la fila apareció.
3. Si la app no logra conectarse a la base de datos, revisar en el dashboard
   de Connect Cloud (enlace arriba) → **Settings → Variables** que las 6
   variables `SD_*` estén presentes como secrets.

## 5. Actualizar el deploy tras editar la encuesta

Basta con volver a correr el mismo comando del paso 3 (mismo `--title` y
`--slug`/`--name`). `deploy.R` reconstruye `_survey/` desde el `survey.qmd`
actual y actualiza el contenido existente en Connect Cloud — no consume un
nuevo cupo del plan gratuito.

## 6. Cupo del plan gratuito (5 aplicaciones)

El plan gratuito de Connect Cloud permite **5 aplicaciones**. `deploy.sh`
informa el uso actual (`N/5`) al final de cada deploy. Redesplegar cualquiera
de las 5 existentes (mismo `--title`/`--slug`) siempre está bien; para
desplegar una aplicación **nueva** estando en el límite hay que retirar o
reemplazar una existente, o usar otra plataforma (Hugging Face, ver
[HUGGINGFACE.md](HUGGINGFACE.md)).

