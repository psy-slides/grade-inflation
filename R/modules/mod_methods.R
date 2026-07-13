# Modulo "Metodi e assunzioni": documentazione metodologica in linguaggio piano,
# con formule accompagnate sempre da una traduzione ordinaria.

mod_methods_ui <- function(id) {
  ns <- shiny::NS(id)

  section <- function(title, ...) bslib::card(
    bslib::card_header(title), bslib::card_body(...))

  shiny::tagList(
    shiny::withMathJax(),
    tab_header("Metodi e assunzioni",
               "Come funziona la simulazione, quali semplificazioni adotta e come leggerne i limiti."),
    simulated_data_note(),
    section(
      "Modello generativo",
      shiny::p(paste(
        "Ogni studente possiede un'abilita' generale latente, indicata con theta,",
        "estratta da una normale standard. Ogni tentativo d'esame produce una",
        "prestazione latente:")),
      shiny::p("$$y^*_{ij} = \\text{base} + \\delta_j + \\lambda\\,\\theta_i + \\varepsilon_{ij}, \\qquad \\varepsilon_{ij} \\sim N(0, \\sigma^2_\\varepsilon)$$"),
      shiny::p(paste(
        "In parole: il risultato di uno studente in un esame dipende da un livello",
        "generale comune (base), dalla generosita' relativa dell'esame (delta),",
        "dall'abilita' dello studente e da una componente casuale.")),
      shiny::tags$ul(
        shiny::tags$li(shiny::HTML(paste(
          "<strong>delta<sub>j</sub></strong>: posizione dell'esame, estratta da",
          "N(0, tau<sup>2</sup>) e centrata a zero. Valori piu' alti = esame",
          "\"piu' generoso\", valori piu' bassi = \"piu' difficile\" (descrizioni",
          "tecniche dei parametri, non giudizi sui docenti). La difficolta'",
          "derivata e' definita come −delta."))),
        shiny::tags$li(shiny::HTML(paste(
          "<strong>lambda = 1 per tutti gli esami</strong> e",
          "<strong>sigma uguale per tutti gli esami</strong>: e' una",
          "semplificazione dichiarata. L'eterogeneita' tra esami riguarda solo la",
          "generosita' (delta), non la capacita' discriminativa ne' l'errore",
          "specifico dell'insegnamento."))),
        shiny::tags$li("Non esistono anni accademici multipli, trend temporali o dinamiche di apprendimento: la simulazione copre un unico ciclo ipotetico.")
      )
    ),
    section(
      "Affidabilita' generale degli esami",
      shiny::p("La fascia scelta (bassa / media / alta, default: media) fissa l'affidabilita' latente target:"),
      shiny::p("$$\\text{affidabilita'} = \\frac{\\lambda^2}{\\lambda^2 + \\sigma^2_\\varepsilon} \\in \\{0{,}35;\\; 0{,}60;\\; 0{,}80\\}$$"),
      shiny::p(paste(
        "da cui si ricava sigma. In parole: con affidabilita' alta i voti",
        "riflettono soprattutto l'abilita'; con affidabilita' bassa pesa di piu'",
        "il caso. Questa e' una proprieta' del processo che genera i voti.")),
      shiny::div(class = "alert alert-light border", shiny::HTML(paste(
        "<strong>Attenzione a non confondere due concetti:</strong>",
        "(1) l'<em>affidabilita' generale degli esami</em> appena definita;",
        "(2) la <em>stabilita' del riferimento percentile</em> di uno specifico",
        "esame, che dipende soprattutto da quanti studenti lo hanno sostenuto",
        "(spia fragile / moderata / buona). Un esame puo' essere molto affidabile",
        "ma avere un riferimento fragile perche' scelto da pochi studenti.")))
    ),
    section(
      "Dalla prestazione latente al voto verbalizzato",
      shiny::tags$ul(
        shiny::tags$li("valori latenti sopra 30 diventano 30 (soffitto);"),
        shiny::tags$li("valori tra 18 e 30 vengono arrotondati all'intero;"),
        shiny::tags$li(paste(
          "valori sotto 18 non vengono verbalizzati: il tentativo si ripete con la",
          "stessa abilita' dello studente finche' si ottiene almeno 18, e si",
          "conserva solo il primo voto verbalizzato."))
      ),
      shiny::p(paste(
        "E' una semplificazione illustrativa delle carriere completate: non",
        "modella l'apprendimento tra un tentativo e l'altro. Un limite tecnico",
        "molto elevato di tentativi protegge da casi numerici estremi; il numero",
        "di tentativi e' registrato internamente ma non e' centrale",
        "nell'interfaccia. Il voto medio complessivo e' calibrato sul target",
        "scelto tramite una ricerca numerica sulla \"base\", con tolleranza",
        "documentata di ±0,10 punti (deterministica, su griglia di quantili).."))
    ),
    section(
      "Scelta degli esami opzionali",
      shiny::p("Per lo studente i e l'alternativa j del gruppo di scelta:"),
      shiny::p("$$U_{ij} = s \\cdot \\widetilde{g}_j + h \\cdot \\theta_i \\cdot \\widetilde{d}_j, \\qquad P(\\text{scelta}=j) = \\text{softmax}(U_{ij}/T)$$"),
      shiny::p(paste(
        "dove g e' il voto atteso dell'alternativa (standardizzato nel gruppo),",
        "d la sua difficolta' (= −delta, standardizzata nel gruppo; valori piu'",
        "alti = esame piu' difficile), s la forza strategica, h la preferenza dei",
        "migliori per gli esami difficili e T la temperatura. La componente",
        "casuale individuale e' realizzata dal campionamento softmax stesso.",
        "Con s = h = 0 la scelta e' casuale (default)."))
    ),
    section(
      "Percentili e pari merito",
      shiny::p("Per un voto x in un esame con N voti, di cui L strettamente inferiori a x ed E uguali a x:"),
      shiny::p("$$p_{\\text{medio}} = \\frac{L + 0{,}5\\,E}{N}, \\qquad [p_{\\text{inf}},\\, p_{\\text{sup}}] = \\left[\\frac{L}{N},\\, \\frac{L+E}{N}\\right]$$"),
      shiny::tags$ul(
        shiny::tags$li(paste(
          "Rango medio (default): chi condivide lo stesso voto occupa la posizione",
          "centrale dell'intervallo attribuibile al gruppo dei pari merito.")),
        shiny::tags$li(paste(
          "L'intervallo dei pari merito e' sempre mostrato come diagnostica: con",
          "moltissimi 30 il percentile centrale non elimina la perdita di",
          "informazione dovuta ai voti uguali.")),
        shiny::tags$li(paste(
          "Metodo conservativo (opzionale, severo): ogni gruppo di pari merito",
          "riceve il limite inferiore della propria fascia, p = L/N.")),
        shiny::tags$li(shiny::HTML(paste(
          "Esiste anche il metodo \"superiore\" p = (L+E)/N, qui illustrato solo",
          "per completezza della gamma di scelte possibili: non e' proposto come",
          "opzione operativa.")))
      ),
      shiny::p(shiny::HTML(paste(
        "<strong>Riferimento:</strong> tutti e soli gli studenti simulati che",
        "hanno sostenuto quello specifico esame, nell'unico ciclo simulato.",
        "Niente anni precedenti, medie storiche o pooling tra coorti. Per gli",
        "esami opzionali il riferimento e' il sottoinsieme di chi li ha scelti:",
        "per questo la numerosita' effettiva puo' differire molto tra esami.")))
    ),
    section(
      "Aggregazione dei risultati",
      shiny::p("Prima della trasformazione normale, i percentili vengono compressi per evitare esattamente 0 e 1:"),
      shiny::p("$$p^{clip} = \\min\\!\\left(\\max\\!\\left(p, \\tfrac{0{,}5}{N}\\right),\\, 1 - \\tfrac{0{,}5}{N}\\right), \\qquad z_{ij} = \\Phi^{-1}(p^{clip}_{ij}), \\qquad \\text{score}_i = \\tfrac{1}{k}\\sum_j z_{ij}$$"),
      shiny::p(paste(
        "Lo score di ogni studente e' poi collocato con rango medio rispetto a",
        "TUTTI gli studenti simulati di tutti i CdS: poiche' ogni esame e' gia'",
        "standardizzato rispetto al proprio riferimento, il confronto finale puo'",
        "unire percorsi diversi. Da questo rango derivano percentile finale",
        "(0–100) e decile (1–10).")),
      shiny::p(paste(
        "Metodi di confronto calcolati in parallelo: voto medio grezzo, voto",
        "medio standardizzato sull'intera popolazione, media semplice dei",
        "percentili (esplicitamente indicata come metodo ingenuo). L'app non",
        "mostra un intervallo aggregato formale: espone gli intervalli dei",
        "singoli esami e una spia sintetica di stabilita' della carriera",
        "(il livello peggiore tra gli esami sostenuti)."))
    ),
    section(
      "Scale confrontabili e ground truth",
      shiny::tags$ul(
        shiny::tags$li(shiny::HTML(
          "<strong>Abilita' vera simulata</strong>: theta, standardizzato empiricamente nella popolazione simulata.")),
        shiny::tags$li(shiny::HTML(paste(
          "<strong>Posizione ricavata dal voto medio</strong>: z-score empirico",
          "del voto medio individuale nell'intera popolazione (le differenze di",
          "generosita' tra percorsi restano visibili di proposito)."))),
        shiny::tags$li(shiny::HTML(paste(
          "<strong>Posizione ricavata dai percentili</strong>: normal score",
          "Phi<sup>−1</sup> del percentile aggregato finale (compresso)."))),
        shiny::tags$li(shiny::HTML(paste(
          "<strong>Media ingenua dei percentili</strong>: z-score empirico della",
          "media semplice dei percentili, incluso solo come confronto.")))
      ),
      shiny::p(paste(
        "Lo studente ipotetico e' costruito dall'utente: per lui NON esiste",
        "un'abilita' vera e nessuna delle posizioni va letta come \"vera",
        "preparazione\". Lo studente simulato, invece, appartiene alla popolazione",
        "generata: la sua theta e' nota e consente di misurare gli errori di",
        "recupero."))
    ),
    section(
      "Limiti principali",
      shiny::tags$ul(
        shiny::tags$li("Tutti i dati sono simulati: nessuna corrispondenza con corsi, esami o studenti reali."),
        shiny::tags$li("Un'unica dimensione di abilita'; discriminazione ed errore comuni a tutti gli esami."),
        shiny::tags$li("Un solo ciclo: nessuno storico, nessun confronto tra coorti, nessuna dinamica temporale."),
        shiny::tags$li("La procedura dei tentativi sotto 18 e' illustrativa e non modella l'apprendimento."),
        shiny::tags$li("Niente CFU differenziati, lode, prove esterne standardizzate o modelli IRT/gerarchici (estensioni possibili, non incluse)."),
        shiny::tags$li("L'app esplora la capacita' informativa degli indicatori nei diversi scenari; non dimostra la superiorita' universale di un metodo ne' fornisce raccomandazioni istituzionali.")
      )
    )
  )
}

mod_methods_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) NULL)
}
