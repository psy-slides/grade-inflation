# METHODS — Modello generativo e metodi statistici

Documentazione tecnica del motore simulativo. Tutti i valori numerici citati sono
centralizzati in `config/defaults.yml`.

## 1. Data generating process (DGP)

Un unico ciclo ipotetico (nessun anno accademico multiplo, nessuno storico).

**Abilità.** Ogni studente *i* ha un'abilità generale latente:

    theta_i ~ N(0, 1)

**Prestazione latente.** Per ogni tentativo dello studente *i* nell'esame *j*:

    y*_ij = base_location + delta_j + lambda * theta_i + epsilon_ij
    epsilon_ij ~ N(0, sigma_epsilon^2)

**Vincoli deliberati (semplificazioni dichiarate):**

- `lambda = 1` per **tutti** gli esami: nessuna differenza di discriminazione;
- `sigma_epsilon` **comune a tutti** gli esami: nessun errore specifico per insegnamento;
- l'eterogeneità tra esami passa **solo** per `delta_j` (posizione/generosità).

**Generosità degli esami.** `delta_j ~ N(0, tau_exam^2)`, centrati a media ~0 su tutti
gli esami (così la calibrazione della media agisce solo su `base_location`).
`tau_exam` è il controllo "eterogeneità tra esami" (0–4 punti latenti).
La difficoltà derivata è `difficulty_j = -delta_j` (valori più alti = esame più
difficile); "più generoso" e "più difficile" sono descrizioni tecniche dei parametri.

## 2. Affidabilità globale

La fascia scelta dall'utente (bassa / media / alta; **default: media**) fissa
l'affidabilità latente target:

    reliability = lambda^2 / (lambda^2 + sigma_epsilon^2)

con valori 0.35 / 0.60 / 0.80. Da cui, con `lambda = 1`:

    sigma_epsilon = sqrt((1 - reliability) / reliability)

Da non confondere con la **stabilità del riferimento percentile** (§ 8), che dipende
dalla numerosità effettiva del singolo esame.

## 3. Trasformazione in voti osservati

Per ogni tentativo:

- `y* > 30` → voto 30 (soffitto);
- `18 <= y* <= 30` → arrotondamento all'intero più vicino (30 incluso da 29.5 in su);
- `y* < 18` → tentativo **non verbalizzato**.

**Primo voto verbalizzato.** Se il tentativo non è verbalizzato, si estrae un nuovo
`epsilon` mantenendo lo stesso `theta`, finché il valore raggiunge almeno 18; si
conserva solo il primo voto verbalizzato. È una semplificazione illustrativa di
carriere completate: nessuna dinamica di apprendimento tra tentativi. Il numero di
tentativi è salvato internamente (`grades$n_attempts`) ma non è centrale nell'UI.
Un tetto tecnico (`max_attempts`, default 1000) protegge dai casi numerici estremi:
il superamento è un fallimento esplicito (voto NA, conteggiato nei metadati e
segnalato tra gli avvisi), non un errore silenzioso.

## 4. Calibrazione della media osservata

Il target (default 27.5) riguarda la **media osservata complessiva**, che non coincide
con la posizione latente per effetto di arrotondamento, soglia a 18 (con retry) e
soffitto a 30. `base_location` è calibrato con `uniroot` su una funzione
**deterministica**: per ogni esame, l'attesa condizionata del voto osservato viene
integrata su una griglia di 101 quantili di N(0,1) per theta (punti medi), usando le
probabilità esatte degli intervalli di arrotondamento di una normale troncata a 18.
Gli esami sono pesati per la quota attesa di iscritti (obbligatori: 1; alternative
opzionali: 1/n_alternative, approssimazione a scelta casuale).

**Tolleranza documentata: ±0.10 punti** sulla media attesa deterministica; sul campione
simulato si aggiunge l'errore campionario. Se il target non è raggiungibile (soffitto
troppo forte), la ricerca si ferma ai limiti dell'intervallo e lo scostamento viene
riportato nei metadati e tra gli avvisi — nessuna precisione fittizia.

La stessa macchina fornisce `expected_grade_j` (voto atteso per esame), riusato dal
meccanismo di scelta strategica.

## 5. Scelta degli esami opzionali

Ogni CdS ha gruppi di alternative; ogni studente sceglie una alternativa per gruppo.
Utilità deterministica per studente *i* e alternativa *j*:

    utility_ij = strategic_strength * expected_grade_std_j
               + hard_exam_preference * theta_i * difficulty_std_j

con `expected_grade_std` e `difficulty_std` standardizzati **entro il gruppo di
scelta**. Probabilità di scelta: `softmax(utility / temperatura)`. La componente
casuale individuale è realizzata dal campionamento multinomiale stesso (equivalente ad
aggiungere rumore Gumbel alle utilità). Con entrambi i parametri a 0 (default) la
scelta è uniforme. Il meccanismo resta probabilistico: gli studenti con theta basso
non sono forzati deterministicamente verso gli esami facili.

## 6. Popolazione di riferimento dei percentili

Per ciascun esame: **tutti e soli gli studenti simulati che lo hanno sostenuto**,
nell'unico ciclo simulato. Nessun anno precedente, nessuna media storica, nessun
pooling tra coorti, nessun dato esterno. Per gli opzionali il riferimento è il
sottoinsieme di chi li ha scelti: la numerosità effettiva può differire fortemente
tra esami ed è sempre mostrata. Un esame opzionale senza iscritti resta "non
osservato": nessun percentile, messaggio diagnostico, nessun errore dell'app.

## 7. Percentili e pari merito

Per un voto x in un esame con N voti (L strettamente inferiori, E uguali):

| quantità | formula | ruolo |
|---|---|---|
| rango medio | `p_mid = (L + 0.5·E) / N` | **default** |
| intervallo pari merito | `[L/N, (L+E)/N]` | sempre disponibile come diagnostica |
| conservativo | `p_cons = L/N` | opzionale, esplicitamente "severo" |
| superiore | `p_up = (L+E)/N` | solo illustrativo (tab Metodi), non operativo |

In linguaggio semplice: con il rango medio, chi condivide lo stesso voto occupa la
posizione centrale dell'intervallo attribuibile al gruppo dei pari merito.
L'intervallo mostra che il percentile centrale **non elimina** la perdita di
informazione dovuta ai molti pari merito (tipicamente molti 30).

**Clipping prima della trasformazione normale.** Per evitare esattamente 0 e 1:

    p_clipped = pmin(pmax(p, 0.5/N), 1 - 0.5/N)

**Studente ipotetico.** Il suo voto è collocato rispetto al riferimento simulato
*senza* essere aggiunto al riferimento (scelta documentata).

## 8. Spie diagnostiche (due concetti distinti)

1. **Stabilità del riferimento percentile** — dipende dall'N effettivo dell'esame:
   fragile (N < 40), moderata (40 ≤ N < 100), buona (N ≥ 100). Soglie in config.
2. **Risoluzione della distribuzione** — capacità discriminativa osservata: scarsa se
   pochi valori distinti (≤ 4), o quota modale ≥ 50%, o quota di 30 ≥ 50%; media se
   valori distinti ≤ 7 o quota modale ≥ 35%; buona altrimenti.

Un esame può avere N elevato e risoluzione pessima (quasi tutti 30) o N piccolo e
discreta dispersione. Le spie usano sempre colore + icona + testo.
**Spia di carriera** dello studente: il livello di stabilità **peggiore** tra gli
esami sostenuti (regola conservativa dichiarata).

## 9. Aggregazione

Metodo principale:

1. percentile dell'esame (metodo configurato) → clipping → `z_ij = qnorm(p_ij)`;
2. `score_i = mean(z_ij)` sugli esami sostenuti;
3. rango medio di `score_i` rispetto a **tutti gli studenti simulati di tutti i CdS**
   (gli esami sono già standardizzati sul proprio riferimento, quindi il confronto
   finale unisce percorsi diversi);
4. dal rango: percentile finale (0–100) e decile (1–10, `ceiling(p·10)` limitato).

Metodi di confronto calcolati in parallelo: voto medio grezzo; voto medio
standardizzato (z-score sull'intera popolazione, pooled: le differenze di generosità
tra percorsi restano visibili di proposito); media semplice dei percentili
(**esplicitamente indicata come metodo ingenuo**).

Nessun intervallo aggregato formale nel primo MVP: l'app espone gli intervalli dei
singoli esami e la spia sintetica di carriera.

## 10. Scale confrontabili e recovery

Confronti sempre su scala standardizzata:

- **abilità vera simulata**: z-score empirico di theta nella popolazione;
- **posizione ricavata dal voto medio**: z-score empirico del voto medio individuale;
- **posizione ricavata dai percentili**: `qnorm` del percentile aggregato clippato;
- **media ingenua dei percentili**: z-score empirico della media semplice dei percentili.

Nessuna statistica è etichettata genericamente come "abilità stimata".

**Metriche** (per indicatore): r di Pearson e rho di Spearman con theta; RMSE e MAE
sulla scala standardizzata; bias medio; accordo sul top 10% (insiemi a soglia sul 90°
percentile: sensibilità = quota del top vero identificata, precisione = quota del top
stimato corretta, sovrapposizione = Jaccard); quota di coppie ordinate correttamente
(enumerazione esatta fino a 500.000 coppie, altrimenti campione casuale di coppie;
i pareggi nella stima contano 0.5). Metriche anche per CdS (theta ristandardizzato
entro CdS) e per percorso opzionale.

Il "vantaggio" di un metodo è sempre presentato come **differenza empirica nello
scenario corrente**; quando il percentile va peggio, il testo lo dice con la stessa
chiarezza.

## 11. Limiti

- una sola dimensione di abilità; niente multidimensionalità né specificità di dominio;
- discriminazione ed errore comuni: gli scenari non esplorano esami più/meno "rumorosi";
- retry sotto 18 senza apprendimento; niente ritiri, abbandoni o non-frequentanti;
- niente CFU, lode, coorti multiple, IRT, modelli gerarchici o shrinkage (rinviati);
- i nomi di CdS/esami sono fittizi; nessuna corrispondenza con insegnamenti reali;
- l'app non è validata e non costituisce una proposta amministrativa.
