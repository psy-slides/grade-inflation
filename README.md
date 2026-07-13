# Laboratorio voti e percentili — app Shiny simulativa

App R Shiny interamente basata su **dati simulati** per esplorare il valore informativo
dei voti universitari (scala 18–30), gli effetti di grade inflation ed effetto soffitto,
e possibili indicatori individuali basati sui **percentili dei voti agli esami**.

> **Disclaimer.** Tutti i dati mostrati sono simulati. L'app non usa dati reali, non
> riproduce corsi di studio esistenti (i nomi di CdS ed esami sono fittizi) e **non è
> uno strumento validato o immediatamente implementabile**: è un laboratorio
> esplorativo con cui osservare che cosa accade al valore informativo dei voti e al
> recupero dell'abilità simulata al variare dei parametri dello scenario.

## Che cosa si può esplorare

- grade inflation ed effetto soffitto (voti concentrati su 30);
- differenze di generosità/difficoltà tra esami (parametri della simulazione);
- dispersione dei voti e affidabilità generale degli esami (bassa / media / alta);
- numerosità degli studenti e stabilità dei riferimenti percentili;
- esami opzionali, scelta casuale o strategica, preferenza dei migliori per esami difficili;
- regole di attribuzione dei percentili con molti pari merito (rango medio, intervallo, conservativo);
- aggregazione dei risultati (voto medio, media ingenua dei percentili, normal score).

L'app **non è costruita per dimostrare che i percentili siano superiori**: esistono
scenari in cui migliorano il recupero dell'abilità simulata, scenari in cui il vantaggio
è minimo e scenari in cui peggiorano il confronto.

## Prerequisiti

- R ≥ 4.3 (sviluppata e verificata con R 4.5.1);
- pacchetti: `shiny`, `bslib`, `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
  `scales`, `yaml`, `jsonlite`, `DT`, `testthat` (per i test), `rsconnect` (per il deploy).

## Installazione

Con [renv](https://rstudio.github.io/renv/) (consigliato, versioni riproducibili):

```r
install.packages("renv")
renv::restore()   # installa le versioni registrate in renv.lock
```

Oppure manualmente:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "purrr",
                   "tibble", "scales", "yaml", "jsonlite", "DT", "testthat",
                   "rsconnect"))
```

## Avvio dell'app

Dalla radice della repository:

```r
shiny::runApp()
```

oppure da terminale:

```bash
Rscript scripts/run_app.R          # porta 4321
Rscript scripts/run_app.R 8080     # porta a scelta
```

All'avvio viene simulato automaticamente lo scenario di default (3 CdS × 200 studenti,
media target 27,5, affidabilità media, scelta casuale degli opzionali). Ogni modifica ai
parametri diventa attiva **solo** premendo «Simula scenario».

## Test e smoke test

```bash
Rscript tests/testthat.R      # suite testthat (motore statistico puro)
Rscript scripts/smoke_test.R  # scenario end-to-end + costruzione dell'oggetto app
```

## Struttura della repository

```
app.R                  entry point Shiny (UI + server, carica R/ e R/modules/)
R/                     motore statistico puro, testabile senza Shiny
  config.R             caricamento configurazione, fasce di affidabilità
  curriculum_generation.R  CdS ed esami fittizi (delta_j, gruppi opzionali)
  student_generation.R     popolazione simulata (theta ~ N(0,1))
  optional_choice.R        scelta degli opzionali (utilità + softmax)
  grade_simulation.R       modello latente, calibrazione della media, voti
  grade_transformation.R   soglia 18, soffitto 30, primo voto verbalizzato
  percentile_methods.R     rango medio, intervallo pari merito, conservativo, clipping
  score_aggregation.R      normal score, score aggregato, percentile finale, decile
  recovery_metrics.R       confronto stime vs theta (r, RMSE, top 10%, coppie, …)
  scenario_builder.R       orchestrazione + statistiche per esame + spie
  validation.R             validazione delle configurazioni
  plot_helpers.R / table_helpers.R / ui_helpers.R / formatting.R
R/modules/             moduli Shiny (una tab ciascuno)
config/defaults.yml    default, fasce di affidabilità, soglie delle spie, preset
inst/app/www/          CSS personalizzato
tests/                 suite testthat
scripts/               run_app.R, smoke_test.R, create_manifest.R
README.md METHODS.md DEPLOYMENT.md PLAN.md
```

## Le tab dell'app

1. **Configura lo scenario** — parametri, preset, sezione avanzata, «Simula scenario»,
   riepilogo dello scenario attivo, export (YAML/JSON/CSV).
2. **Quadro generale** — value box, distribuzione complessiva dei voti (discreta 18–30),
   distribuzione dei theta, relazione theta–voto, mappa di CdS ed esami, lettura
   sintetica automatica (descrittiva, non prescrittiva).
3. **Distribuzioni degli esami** — filtri per CdS/esame/tipo, barre discrete, heatmap
   esame × voto, confronto latente/osservato, spie di stabilità e risoluzione.
4. **Studente ipotetico** — libretto costruito dall'utente; percentili, intervalli dei
   pari merito e indicatori aggregati. **Nessuna ground truth**: l'app non conosce una
   "vera abilità" di questo studente.
5. **Studente simulato** — estrazione casuale (anche per fascia di theta); abilità vera
   nota, confronto delle tre posizioni sulla stessa scala, errori di recupero.
6. **Recovery complessivo** — metriche e grafici per tutti gli studenti: voto medio vs
   indicatore percentile (e media ingenua), per CdS e per percorso opzionale.
7. **Metodi e assunzioni** — modello generativo, formule e traduzione in linguaggio
   ordinario, limiti.

## Deploy

Vedi [DEPLOYMENT.md](DEPLOYMENT.md) (shinyapps.io, manifest, credenziali, limiti).

## Limiti principali

- dati interamente simulati, un solo ciclo ipotetico (niente anni accademici multipli);
- una sola dimensione di abilità; discriminazione (`lambda`) ed errore (`sigma`)
  comuni a tutti gli esami — l'eterogeneità riguarda solo la generosità (`delta_j`);
- la procedura dei tentativi sotto 18 è illustrativa (nessun apprendimento tra tentativi);
- nessun CFU differenziato, lode, IRT o modello gerarchico (estensioni possibili);
- i risultati descrivono lo scenario simulato corrente, non proprietà universali dei metodi.

Dettagli metodologici completi in [METHODS.md](METHODS.md); decisioni architetturali e
roadmap in [PLAN.md](PLAN.md).
