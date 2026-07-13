# PLAN — Laboratorio simulativo su voti universitari e percentili

Documento di pianificazione tecnica. Registra le decisioni architetturali, le milestone,
ciò che è incluso nel primo MVP e ciò che è esplicitamente rinviato.

## Obiettivo

App R Shiny interamente basata su dati simulati per esplorare il valore informativo dei
voti universitari (18–30) e di indicatori individuali basati sui percentili, al variare di:
grade inflation / effetto soffitto, generosità relativa degli esami, affidabilità globale,
numerosità, esami opzionali e meccanismi di scelta, gestione dei pari merito, metodi di
aggregazione.

L'app è un laboratorio esplorativo, non uno strumento validato né implementabile così com'è.
Interfaccia in italiano, codice in inglese.

## Decisioni architetturali

1. **App Shiny modulare semplice** (no golem/rhino): `app.R` + `R/` (motore statistico puro)
   + `R/modules/` (moduli Shiny). Il motore non dipende da Shiny ed è testabile con testthat.
2. **Oggetto scenario unico** prodotto da `build_scenario(config)`: lista con classe
   `grade_scenario` contenente `config, courses, exams, students, enrollments, grades,
   exam_statistics, percentile_scores, student_scores, recovery, metadata`.
3. **Reattività**: la simulazione parte solo con `eventReactive(input$simulate)`. I controlli
   modificati ma non applicati sono segnalati da un badge "parametri modificati".
4. **Configurazione centralizzata** in `config/defaults.yml` (default, fasce di affidabilità,
   soglie delle spie, limiti di validazione, pool di nomi fittizi). Nessun numero magico nel codice.
5. **Modello generativo**: `theta_i ~ N(0,1)`; `y*_ij = base + delta_j + lambda*theta_i + eps_ij`
   con `lambda = 1` e `sigma_eps` comuni a tutti gli esami (semplificazione dichiarata).
   `sigma_eps` derivato dalla fascia di affidabilità target (bassa 0.35 / media 0.60 / alta 0.80,
   default **media**) via `sigma = sqrt((1-rel)/rel)`.
6. **Trasformazione voti**: y*>30 → 30; 18–30 → arrotondamento a intero; y*<18 → nuovo
   tentativo con stessa theta (primo voto verbalizzato), con tetto tecnico `max_attempts`
   configurato e gestione esplicita del fallimento.
7. **Calibrazione della media**: `base_location` trovato con ricerca numerica unidimensionale
   (`uniroot`) su una funzione **deterministica** che calcola la media osservata attesa per
   esame integrando su una griglia di quantili di theta (niente Monte Carlo costoso).
   Tolleranza documentata: ±0.10 punti; scostamenti maggiori vengono segnalati nei metadati.
8. **Scelta degli opzionali**: utilità deterministica
   `strategic_strength * expected_grade_std + hard_pref * theta_i * difficulty_std` e
   campionamento softmax con temperatura; la componente casuale individuale è realizzata dal
   campionamento stesso (equivalente a rumore Gumbel). Default: scelta casuale (entrambi 0).
   `difficulty_j = -delta_j` standardizzata nel gruppo (più alta = più difficile).
9. **Percentili**: riferimento = tutti e soli gli studenti simulati che hanno sostenuto quello
   specifico esame (ciclo unico, nessuno storico). Metodi: rango medio (default),
   conservativo L/N (opzionale); intervallo dei pari merito [L/N, (L+E)/N] sempre calcolato
   come diagnostica. Clipping `[0.5/N, 1-0.5/N]` prima di `qnorm`.
10. **Aggregazione principale**: media dei normal score `qnorm(p)` per studente → rango medio
    dello score tra **tutti** gli studenti simulati di tutti i CdS → percentile finale e decile.
    Metodi di confronto: voto medio grezzo, voto medio standardizzato (pooled), media ingenua
    dei percentili (etichettata come tale).
11. **Stime dell'abilità su scala confrontabile**: z-score empirico del voto medio (pooled);
    normal score del percentile aggregato (clippato). Trasformazioni documentate in METHODS.md
    e nella tab Metodi; mai etichettate genericamente "abilità stimata".
12. **Spie separate**: (a) stabilità del riferimento percentile basata su N effettivo
    (fragile <40, moderata 40–99, buona ≥100, soglie in config); (b) risoluzione della
    distribuzione (valori distinti, quota modale, quota di 30). Colore + icona + testo.
    Spia di carriera dello studente = livello peggiore tra i suoi esami (regola dichiarata).
13. **Grafici**: ggplot2, voti mostrati come distribuzioni discrete 18–30 (bar chart),
    nessuna densità smussata che nasconda i 30; DT per le tabelle.
14. **Esami opzionali senza iscritti**: nessun calcolo dei percentili, messaggio diagnostico,
    l'app non fallisce.
15. **Studente ipotetico**: collocato nelle distribuzioni simulate SENZA aggiungerlo al
    riferimento; nessuna ground truth mostrata. Studente simulato: tab separata con theta,
    recovery individuale e grafico di confronto delle tre posizioni.

## Milestone

- **F1** Struttura repo, configurazione, funzioni statistiche pure, test unitari. ✔ pianificata
- **F2** Generazione scenario completa (curriculum, studenti, scelte, voti, percentili,
  aggregazione, recovery), smoke test da riga di comando. ✔ pianificata
- **F3** Moduli Shiny (7 tab), stile bslib, validazione input, download. ✔ pianificata
- **F4** Documentazione (README, METHODS, DEPLOYMENT), renv.lock, verifica avvio, pulizia. ✔ pianificata

## Incluso nell'MVP

- 7 tab: Configura scenario, Quadro generale, Distribuzioni, Studente ipotetico,
  Studente simulato, Recovery complessivo, Metodi e assunzioni.
- 7 preset di scenario; parametri principali + sezione avanzata (N per CdS separati,
  temperatura softmax, max tentativi).
- Download: configurazione (YAML/JSON), esami CSV, studenti CSV, recovery CSV, PNG dei
  grafici principali. Tutti i file esportati includono seed, data e disclaimer.
- Test automatici per trasformazioni, percentili, aggregazione, scelta opzionali,
  scenario builder, metriche di recovery.

## Rinviato (architettura predisposta, non implementato)

- Anni accademici multipli, pooling storico, dati reali, IRT/modelli gerarchici,
  shrinkage bayesiano, CFU differenziati, lode, login/database, report HTML complesso,
  studi Monte Carlo ripetuti (le funzioni pure sono già riutilizzabili a tale scopo),
  Shinylive (non promesso: da verificare).

## Rischi tecnici

- Calibrazione della media con target vicino a 30: il soffitto rende la media osservata
  poco sensibile a `base_location`; la ricerca è vincolata a un intervallo e lo scostamento
  residuo è riportato nei metadati invece di forzare precisione fittizia.
- Gruppi opzionali molto sbilanciati con scelta strategica intensa: gestiti dalla spia di
  stabilità e dal caso limite "0 iscritti".
- Prestazioni: operazioni vettorializzate su tabelle; scenari fino a 5 CdS × 1000 studenti.
