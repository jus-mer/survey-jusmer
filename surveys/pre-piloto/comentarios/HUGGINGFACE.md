# Deploy a Hugging Face Spaces — encuesta "comentarios"

Documenta el procedimiento completo para desplegar este survey (surveydown: `app.R` +
`survey.qmd`) en Hugging Face Spaces, y cómo queda conectado a la base de datos en
Supabase. Complementa las notas existentes sobre Supabase en [README.md](README.md).

## 1. Prerrequisitos

### 1.1 Cuenta y token de Hugging Face

1. Crear cuenta en [huggingface.co](https://huggingface.co) si no existe.
2. Generar un token de tipo **Write** en <https://huggingface.co/settings/tokens>
   (`New token` → role `Write`).

### 1.2 Instalar el CLI `hf`

El CLI viene en el paquete `huggingface_hub`. En Ubuntu/Debian, `pip` global está
bloqueado ("externally managed environment"), así que se instala aislado con `pipx`:

```bash
sudo apt update && sudo apt install -y pipx
pipx ensurepath
# abrir una terminal nueva (o `source ~/.bashrc`) para que el PATH se actualice
pipx install huggingface_hub
hf version   # verifica que quedó instalado
```

### 1.3 Iniciar sesión (una sola vez)

```bash
hf auth login        # pega el token en el prompt oculto
hf auth whoami        # confirma la cuenta activa
```

El token queda guardado en el keychain del sistema operativo — **nunca** se pega en
un chat, un dotfile (`.bashrc`/`.zshrc`) ni un archivo versionado. `hf`,
`huggingface_hub` y el script de deploy lo leen automáticamente de ahí.

## 2. Cómo se conecta la app a Supabase

### 2.1 El proyecto en Supabase

- Organización: **JUSMER** (ingreso con cuenta de GitHub, plan educational).
- Proyecto: `survey-jusmer` → base de datos `prepilot`.
- La tabla de respuestas se llama `responses` (valor por defecto de `SD_TABLE`).

### 2.2 Credenciales locales (`.env`)

Las credenciales se configuran **una vez** por survey, desde R, parado en la carpeta
del survey:

```r
surveydown::sd_db_config()
```

Esto pide los datos que entrega Supabase (Connect → Direct → Transaction pooler:
host, user, password, database) y genera un archivo `.env` local con:

```
SD_HOST=...
SD_PORT=...
SD_DBNAME=...
SD_USER=...
SD_TABLE=responses
SD_PASSWORD=...
```

`.env` está en `.gitignore` — nunca se sube al repositorio.

### 2.3 Cómo lo lee `app.R`

En `app.R` (líneas ~39-40):

```r
ignore_db <- tolower(Sys.getenv("SD_IGNORE_DB", "false")) %in% c("1", "true", "yes")
db <- sd_db_connect(ignore = ignore_db)
```

Por defecto (sin `SD_IGNORE_DB` seteada) la app **siempre** intenta conectarse a la
base de datos configurada — vía `.env` en local, o vía variables de entorno en el
servidor. Esto es independiente del campo `set-mode: database` que aparece en el
YAML de `survey.qmd`: ese campo no es una clave reconocida por surveydown (la clave
real de la librería es `mode`, no `set-mode`), así que no tiene efecto — es solo una
anotación. El comportamiento real de guardar-o-no-guardar lo controla únicamente
`SD_IGNORE_DB`.

- `SD_IGNORE_DB` sin definir, o `false` → guarda respuestas en Supabase (producción).
- `SD_IGNORE_DB=true` → no guarda nada, útil para pruebas locales de la interfaz.

### 2.4 Cómo llegan las credenciales al Space de Hugging Face

Un Space no tiene acceso al `.env` local (nunca se sube, queda excluido explícitamente
del paquete). En su lugar, las 6 variables `SD_*` se configuran como **Secrets** del
Space (Settings → *Variables and secrets* → *New secret*), que Hugging Face inyecta
como variables de entorno al contenedor en tiempo de ejecución. `sd_db_connect()` las
detecta automáticamente ahí cuando no encuentra un `.env`.

El script de deploy (paso 3) hace esta sincronización por ti: si el survey está en
modo database y existe un `.env` real junto a él, sube esas 6 variables como Secrets
justo después de publicar el Space — sin imprimir nunca los valores en la terminal.

## 3. Deploy paso a paso

Con `hf` ya autenticado (paso 1), desde la raíz del repo:

```bash
~/.claude/skills/surveydown-skill/deploy-hugging-face/deploy.sh \
  --space tomasurzuam/jusmer-pre-piloto-comentarios \
  --dir surveys/pre-piloto/comentarios \
  --title "Pre-Piloto Encuesta Justicia y Merecimiento — Comentarios" \
  --wait
```

Qué hace, en orden:

1. **Empaqueta** los archivos runtime del survey (`app.R`, `survey.qmd`, `data/`,
   `images/`, `code/`, `*.yml`) — excluye `_survey/`, `preview_data.csv`,
   `rsconnect/`, `.git/`, etc. El contenedor **renderiza `survey.qmd` con Quarto al
   arrancar** (Quarto viene instalado en la imagen), así que nunca se sube una
   versión pre-renderizada.
2. Agrega el `Dockerfile` compartido (imagen `rocker` + Quarto + `surveydown`
   instalado desde GitHub) y un `packages.txt` generado automáticamente a partir de
   los `library()`/`require()` del survey.
3. Crea el Space (SDK Docker) si no existe, y sube todo con `hf upload` — autentica
   como la cuenta activa de `hf auth whoami`.
4. **Sincroniza los Secrets**: como el survey usa la base de datos y hay un `.env`
   real en la carpeta, sube `SD_HOST/PORT/DBNAME/USER/TABLE/PASSWORD` como Secrets
   del Space.
5. Con `--wait`, sondea el estado del Space hasta que queda `RUNNING` y confirma con
   un `HTTP 200` sobre la URL final.

URL resultante: `https://<owner>-<space-name>.hf.space`
(en este caso: <https://tomasurzuam-jusmer-pre-piloto-comentarios.hf.space>)

Un primer build tarda unos **2-5 minutos**. Si falla, revisar los logs en
`https://huggingface.co/spaces/<owner>/<space-name>?logs=build`.

## 4. Verificación post-deploy

1. Abrir la URL del Space y completar una respuesta de prueba.
2. En Supabase → Table editor → tabla `responses` (o el valor de `SD_TABLE`),
   confirmar que la fila apareció.
3. Si aparece el banner **"DATABASE NOT CONNECTED"** dentro de la encuesta:
   - Revisar Settings → *Variables and secrets* del Space: las 6 `SD_*` deben estar
     como **Secrets**, no como Variables públicas.
   - Reiniciar el Space (Settings → *Restart this Space*) para que tome las
     variables nuevas.

## 5. Actualizar el deploy tras editar la encuesta

Basta con volver a correr el mismo comando del paso 3. `deploy.sh` sobrescribe el
contenido del Space (`hf upload --delete "*"`) y dispara un rebuild automático. No
hace falta re-renderizar `_survey/` a mano — a diferencia de shinyapps.io, el
contenedor de Hugging Face renderiza `survey.qmd` con Quarto en cada arranque.

## 6. Cuota de hardware (free tier)

La cuenta gratuita de Hugging Face (CPU Basic) permite ~3 Spaces **corriendo** al
mismo tiempo (no hay límite en cuántos se pueden crear). Si se supera, el Space
queda "quota-paused" y no se despierta solo. Revisar cupo disponible antes de crear
otro Space:

```bash
~/.claude/skills/surveydown-skill/deploy-hugging-face/check-quota.sh tomasurzuam
```

## 7. Spaces y URLs en Hugging Face

Al momento de generar el deploy de un proyecto, Hugging Face genera una URL similar a GitHub Pages: utiliza el nombre de usuario y del proyecto para identificar el dominio. 

## 8. Organizaciones y Spaces en Hugging Face

Cuando se genera el deploy de un proyecto desde una cuenta personal, la URL va a llevar el username de esta cuenta. Esto se puede cambiar de dos maneras: 1) pagando suscripción mensual a un plan que permite modificar el dominio 2) traspasando el proyecto a una organización.

La opción 2) es la más recomendable ya que se puede realizar de forma gratuita. Para ello se debe:

- Crear previamente una organización en Hugging Face (idealmente que tenga colaboradores)
- Entrar al proyecto alojado en la cuenta personal
- Clickear settings (botón superior deerecho)
- Encontrar la sección "Rename or transfer this Space"
- Seleccionar la organización a la cual se quiere transferir el proyecto
- Elegir el nombre del proyecto
- Apretar el botón "I understand, move this space"

Una vez hecho esto, el proyecto se traspasa y empienza a generarse el nuevo deploy, ahora alojado dentro de la organización (esto puede demorar un par de minutos).

## Nota: mismo survey también en shinyapps.io

Esta encuesta también está desplegada en shinyapps.io
(<https://jusmer.shinyapps.io/comentarios/>), apuntando a la **misma** base de datos
Supabase (mismo `.env` / mismas credenciales). Ambos despliegues escriben en la
misma tabla — mantenerlos sincronizados en contenido (mismo `survey.qmd`/`app.R`) si
se usan en paralelo, para no mezclar respuestas de versiones distintas de la
encuesta.
