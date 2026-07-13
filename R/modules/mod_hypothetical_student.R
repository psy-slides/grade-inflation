# Modulo "Studente ipotetico": l'utente costruisce un libretto voto per voto e
# l'app lo colloca nelle distribuzioni simulate. NESSUNA ground truth: niente
# theta, niente errori di recovery, nessuna "vera abilita'" retro-simulata.

mod_hypothetical_student_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320, open = "always",
      shiny::selectInput(ns("course"), "Corso di studio", choices = NULL),
      shiny::uiOutput(ns("optional_pickers")),
      shiny::helpText(paste(
        "Cambiando CdS o alternativa opzionale, l'elenco degli esami e i",
        "riferimenti percentili si aggiornano di conseguenza."))
    ),
    tab_header("Studente ipotetico",
               "Inserisci un voto per ogni esame del percorso e osserva percentili, intervalli dei pari merito e indicatori aggregati."),
    hypothetical_note(),
    bslib::card(
      bslib::card_header("Voti degli esami del percorso"),
      bslib::card_body(shiny::uiOutput(ns("grade_inputs")))
    ),
    bslib::card(
      bslib::card_header("Posizionamento nei singoli esami"),
      bslib::card_body(DT::DTOutput(ns("exam_results")))
    ),
    bslib::card(
      bslib::card_header("Riepilogo aggregato"),
      bslib::card_body(
        shiny::uiOutput(ns("summary_boxes")),
        shiny::uiOutput(ns("summary_notes"))
      )
    )
  )
}

mod_hypothetical_student_server <- function(id, scenario) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(scenario(), {
      sc <- scenario()
      shiny::updateSelectInput(
        session, "course",
        choices = stats::setNames(sc$courses$course_id, sc$courses$course_name))
    })

    course_exams <- shiny::reactive({
      sc <- scenario()
      shiny::req(input$course)
      sc$exam_statistics[sc$exam_statistics$course_id == input$course, ]
    })

    output$optional_pickers <- shiny::renderUI({
      df <- course_exams()
      groups <- unique(stats::na.omit(df$choice_group))
      if (length(groups) == 0) {
        return(shiny::helpText("Questo CdS non prevede esami opzionali."))
      }
      lapply(seq_along(groups), function(i) {
        alternatives <- df[!is.na(df$choice_group) & df$choice_group == groups[i], ]
        shiny::radioButtons(
          ns(paste0("group_", i)),
          sprintf("Gruppo opzionale %d", i),
          choices = stats::setNames(alternatives$exam_id, alternatives$exam_name)
        )
      })
    })

    # Percorso completo dello studente ipotetico: obbligatori + alternative scelte.
    path_exams <- shiny::reactive({
      df <- course_exams()
      groups <- unique(stats::na.omit(df$choice_group))
      chosen <- vapply(seq_along(groups), function(i) {
        input[[paste0("group_", i)]] %||% NA_character_
      }, character(1))
      shiny::req(!anyNA(chosen))
      rbind(df[df$mandatory, ], df[df$exam_id %in% chosen, ])
    })

    reference_grades <- shiny::reactive({
      sc <- scenario()
      grades <- sc$grades[!sc$grades$failed, ]
      split(grades$grade, grades$exam_id)
    })

    output$grade_inputs <- shiny::renderUI({
      df <- path_exams()
      refs <- reference_grades()
      cards <- lapply(seq_len(nrow(df)), function(i) {
        exam <- df[i, ]
        ref <- refs[[exam$exam_id]] %||% integer(0)
        if (length(ref) == 0) {
          return(bslib::card(
            bslib::card_header(exam$exam_name),
            bslib::card_body(unobserved_exam_message(exam$exam_name))
          ))
        }
        default_grade <- max(18, min(30, round(exam$mean_grade)))
        bslib::card(
          class = "exam-input-card",
          bslib::card_header(
            shiny::div(class = "d-flex justify-content-between align-items-center",
                       shiny::span(exam$exam_name),
                       stability_badge(exam$stability,
                                       "Stabilita' del riferimento percentile (dipende da N)"))
          ),
          bslib::card_body(
            shiny::div(
              class = "d-flex gap-3 align-items-start",
              shiny::div(
                class = "exam-input-controls",
                shiny::numericInput(ns(paste0("grade_", exam$exam_id)),
                                    "Voto (18–30)", value = default_grade,
                                    min = 18, max = 30, step = 1, width = "110px"),
                shiny::div(class = "small text-muted",
                           sprintf("N = %d · media %s · 30: %s",
                                   exam$n, fmt_grade(exam$mean_grade),
                                   fmt_percent(exam$share_top)))
              ),
              mini_grade_histogram(ref)
            )
          )
        )
      })
      do.call(bslib::layout_column_wrap,
              c(list(width = 1 / 2, fill = FALSE), cards))
    })

    # Posizionamento per esame: percentile con il metodo configurato + intervallo
    # dei pari merito, senza aggiungere lo studente al riferimento.
    hypothetical_results <- shiny::reactive({
      sc <- scenario()
      df <- path_exams()
      refs <- reference_grades()
      method <- sc$config$percentile_method
      rows <- lapply(seq_len(nrow(df)), function(i) {
        exam <- df[i, ]
        ref <- refs[[exam$exam_id]] %||% integer(0)
        grade <- input[[paste0("grade_", exam$exam_id)]]
        if (length(ref) == 0 || is.null(grade) || is.na(grade)) return(NULL)
        grade <- as.integer(round(grade))
        shiny::validate(shiny::need(
          grade >= 18 && grade <= 30,
          sprintf("Il voto di \"%s\" deve essere tra 18 e 30.", exam$exam_name)))
        p <- percentile_of_grade(grade, ref)
        p_raw <- if (method == "rango_medio") p$p_mid else p$p_conservative
        tibble::tibble(
          exam_name = exam$exam_name, grade = grade,
          p_mid = p$p_mid, p_lower = p$p_lower, p_upper = p$p_upper,
          z = percentile_to_normal_score(p_raw, p$n_ref),
          n_ref = p$n_ref, stability = exam$stability,
          highlight_ref = list(ref)
        )
      })
      dplyr::bind_rows(rows)
    })

    output$exam_results <- DT::renderDT({
      res <- hypothetical_results()
      shiny::validate(shiny::need(nrow(res) > 0,
                                  "Inserisci i voti per vedere il posizionamento."))
      app_datatable(student_exams_display(res), page_length = 12)
    })

    output$summary_boxes <- shiny::renderUI({
      sc <- scenario()
      res <- hypothetical_results()
      shiny::validate(shiny::need(nrow(res) > 0, "In attesa dei voti."))
      aggregate_z <- mean(res$z)
      placement <- place_score_in_population(aggregate_z,
                                             sc$student_scores$aggregate_z)
      worst <- min(res$stability)
      vb <- function(title, value, subtitle = NULL) bslib::value_box(
        title = title, value = value, shiny::p(subtitle),
        theme = bslib::value_box_theme(bg = "#eef4fc", fg = "#153a63"))
      bslib::layout_column_wrap(
        width = 1 / 4, fill = FALSE,
        vb("Voto medio", fmt_grade(mean(res$grade), 2)),
        vb("Score aggregato", fmt_z(aggregate_z), "media dei normal score"),
        vb("Percentile finale", fmt_percentile(placement$percentile),
           "rispetto a tutti gli studenti simulati"),
        vb("Decile", placement$decile),
        vb("Stabilita' della carriera",
           paste(stability_icon(worst), stability_label(worst)),
           "livello peggiore tra gli esami del percorso")
      )
    })

    output$summary_notes <- shiny::renderUI({
      res <- hypothetical_results()
      if (nrow(res) == 0) return(NULL)
      big_ties <- res[res$p_upper - res$p_lower >= 0.3, ]
      shiny::tagList(
        if (nrow(big_ties) > 0) shiny::div(
          class = "alert alert-warning small",
          sprintf(paste(
            "In %d esami l'intervallo dei pari merito e' molto ampio: il",
            "percentile centrale non elimina la perdita di informazione dovuta",
            "ai molti voti uguali (spesso molti 30)."), nrow(big_ties))),
        shiny::p(class = "text-muted small", paste(
          "Il percentile finale colloca lo score aggregato rispetto agli score",
          "degli studenti simulati di tutti i CdS, senza aggiungere lo studente",
          "ipotetico al riferimento."))
      )
    })
  })
}
