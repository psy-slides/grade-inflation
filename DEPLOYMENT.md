# DEPLOYMENT — Pubblicazione dell'app

L'app è una normale app Shiny a file singolo (`app.R` + `R/` + `config/` + `inst/`):
non richiede database, autenticazione né storage persistente.

## 1. Deploy su shinyapps.io

Prerequisiti: account su [shinyapps.io](https://www.shinyapps.io) e pacchetto
`rsconnect`.

```r
install.packages("rsconnect")

# Credenziali (una tantum per macchina; NON committarle)
rsconnect::setAccountInfo(
  name   = "NOME_ACCOUNT",
  token  = "TOKEN",
  secret = "SECRET"
)

# Deploy dalla radice della repository
rsconnect::deployApp(
  appDir = ".",
  appName = "laboratorio-voti-percentili",
  appFiles = c("app.R",
               list.files("R", recursive = TRUE, full.names = TRUE),
               list.files("config", full.names = TRUE),
               list.files("inst", recursive = TRUE, full.names = TRUE)),
  forceUpdate = TRUE
)
```

Note:

- escludere esplicitamente `tests/`, `scripts/`, i file `.md` e `renv/` dal bundle
  (l'elenco `appFiles` sopra lo fa già);
- la prima pubblicazione installa i pacchetti sul server: può richiedere alcuni minuti;
- sul piano gratuito l'app si sospende dopo un periodo di inattività e ha un monte ore
  mensile limitato.

## 2. Generazione del manifest (Posit Connect / deploy da git)

```bash
Rscript scripts/create_manifest.R
```

crea `manifest.json` nella radice (da committare se si usa il deploy git-backed di
Posit Connect). Il manifest registra i pacchetti e le versioni correnti.

## 3. Credenziali e variabili da NON committare

- token e secret di `rsconnect` (restano nella configurazione locale utente;
  la cartella `rsconnect/` generata dal deploy è in `.gitignore`);
- l'app non usa variabili d'ambiente, chiavi API o segreti propri.

## 4. Limiti della persistenza

L'app **non salva nulla lato server**: ogni sessione ricrea lo scenario dai parametri
e dal seed. Chi vuole conservare uno scenario deve scaricare la configurazione
(YAML/JSON) dalla tab «Configura lo scenario» e ricaricarne i valori manualmente.
Su shinyapps.io il filesystem è effimero: qualunque file scritto a runtime viene
perso al riavvio dell'istanza (per questo i download avvengono solo lato client).

## 5. Shinylive (prospettiva futura, non promessa)

L'app usa solo pacchetti disponibili in webR nella maggior parte dei casi (`shiny`,
`bslib`, `ggplot2`, `dplyr`, `DT`, `yaml`), quindi una conversione con
[shinylive](https://posit-dev.github.io/r-shinylive/) è plausibile ma **non è stata
verificata**: prima di annunciarla occorre testare rendering dei grafici, MathJax e
prestazioni della simulazione in WebAssembly.
