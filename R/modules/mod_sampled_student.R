# Modulo "Studente simulato": estrazione casuale (o per profilo di theta) di uno
# studente realmente appartenente alla popolazione simulata, con ground truth.

mod_sampled_student_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320, open = "always",
      shiny::selectInput(ns("course"), "Corso di studio",
                         choices = c("Tutti" = "tutti")),
      shiny::radioButtons(
        ns("profile"), "Profilo di abilita' (theta)",
        choices = c("Casuale" = "casuale", "Fascia bassa" = "bassa",
                    "Fascia media" = "media", "Fascia alta" = "alta"),
        selected = "casuale"),
      shiny::actionButton(ns("draw"), "Estrai uno studente",
                          class = "btn-primary w-100", icon = shiny::icon("dice")),
      shiny::helpText(paste(
        "L'estrazione e' realmente casuale tra gli studenti che rispettano i",
        "filtri: non seleziona casi favorevoli."))
    ),
    tab_header("Studente simulato",
               "Per uno studente della popolazione simulata l'abilita' vera e' nota: si puo' osservare quanto voto medio e percentili la recuperano."),
    shiny::uiOutput(ns("student_content"))
  )
}

mod_sampled_student_server <- function(id, scenario) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(scenario(), {
      sc <- scenario()
      shiny::updateSelectInput(
        session, "course",
        choices = c("Tutti" = "tutti",
                    stats::setNames(sc$courses$course_id, sc$courses$course_name)))
      current_student(NULL)
    })

    current_student <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$draw, {
      sc <- scenario()
      app_config <- load_app_config()
      pool <- sc$students
      if (input$course != "tutti") {
        pool <- pool[pool$course_id == input$course, ]
      }
      profiles <- app_config$theta_profiles
      pool <- switch(input$profile,
        bassa = pool[pool$theta < profiles$low_below, ],
        media = pool[pool$theta >= profiles$low_below &
                       pool$theta < profiles$high_at_least, ],
        alta = pool[pool$theta >= profiles$high_at_least, ],
        pool)
      if (nrow(pool) == 0) {
        shiny::showNotification(
          "Nessuno studente simulato rispetta i filtri scelti.", type = "warning")
        return()
      }
      current_student(pool$student_id[sample.int(nrow(pool), 1)])
    })

    student_detail <- shiny::reactive({
      sc <- scenario()
      id_sel <- current_student()
      shiny::req(id_sel)
      student <- sc$students[sc$students$student_id == id_sel, ]
      scores <- sc$student_scores[sc$student_scores$student_id == id_sel, ]
      theta_std <- sc$recovery$theta_std
      exams <- dplyr::inner_join(
        sc$percentile_scores[sc$percentile_scores$student_id == id_sel, ],
        sc$exam_statistics[, c("exam_id", "exam_name", "mandatory",
                               "choice_group", "stability")],
        by = "exam_id")
      list(
        student = student,
        course_name = sc$courses$course_name[
          sc$courses$course_id == student$course_id],
        scores = scores,
        exams = exams,
        theta_std = theta_std$theta_std[theta_std$student_id == id_sel]
      )
    })

    output$student_content <- shiny::renderUI({
      if (is.null(current_student())) {
        return(shiny::div(
          class = "alert alert-light border",
          "Premi \"Estrai uno studente\" per campionare dalla popolazione simulata."))
      }
      shiny::tagList(
        shiny::uiOutput(ns("student_boxes")),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Tre posizioni sulla stessa scala standardizzata"),
            bslib::card_body(
              shiny::plotOutput(ns("positions_plot"), height = 260),
              shiny::p(class = "text-muted small", paste(
                "La linea tratteggiata e' l'abilita' vera simulata (standardizzata);",
                "i segmenti mostrano l'errore di ciascuna posizione stimata."))
            )
          ),
          bslib::card(
            bslib::card_header("Confronto con l'abilita' vera"),
            bslib::card_body(shiny::uiOutput(ns("recovery_detail")))
          )
        ),
        bslib::card(
          bslib::card_header("Percorso e risultati nei singoli esami"),
          bslib::card_body(DT::DTOutput(ns("exam_table")))
        )
      )
    })

    output$student_boxes <- shiny::renderUI({
      d <- student_detail()
      optional_names <- d$exams$exam_name[!d$exams$mandatory]
      vb <- function(title, value, subtitle = NULL) bslib::value_box(
        title = title, value = value, shiny::p(subtitle),
        theme = bslib::value_box_theme(bg = "#eef4fc", fg = "#153a63"))
      bslib::layout_column_wrap(
        width = 1 / 4, fill = FALSE,
        vb("Studente", d$student$student_id, d$course_name),
        vb("Abilita' vera (theta)", fmt_z(d$student$theta), "simulata, nota"),
        vb("Voto medio", fmt_grade(d$scores$mean_grade, 2)),
        vb("Percentile finale", fmt_percentile(d$scores$aggregate_percentile),
           sprintf("decile %d", d$scores$decile)),
        vb("Opzionali scelti",
           if (length(optional_names) > 0) paste(optional_names, collapse = ", ")
           else "nessuno"),
        vb("Stabilita' carriera",
           paste(stability_icon(d$scores$career_stability),
                 stability_label(d$scores$career_stability)),
           "livello peggiore tra gli esami sostenuti")
      )
    })

    output$positions_plot <- shiny::renderPlot({
      d <- student_detail()
      positions <- tibble::tibble(
        label = c("Abilita' vera simulata",
                  "Posizione ricavata dal voto medio",
                  "Posizione ricavata dai percentili"),
        value = c(d$theta_std,
                  d$scores$estimate_from_mean_grade,
                  d$scores$estimate_from_percentiles)
      )
      plot_student_positions(positions, theta = d$theta_std)
    }, res = 96)

    output$recovery_detail <- shiny::renderUI({
      d <- student_detail()
      err_mean <- d$scores$estimate_from_mean_grade - d$theta_std
      err_pct <- d$scores$estimate_from_percentiles - d$theta_std
      row <- function(label, value) shiny::tags$tr(
        shiny::tags$td(label), shiny::tags$td(class = "text-end", value))
      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$tbody(
          row("Abilita' vera simulata (standardizzata)", fmt_z(d$theta_std)),
          row("Posizione ricavata dal voto medio",
              fmt_z(d$scores$estimate_from_mean_grade)),
          row("Errore del voto medio", fmt_z(err_mean)),
          row("Posizione ricavata dai percentili",
              fmt_z(d$scores$estimate_from_percentiles)),
          row("Errore dei percentili", fmt_z(err_pct)),
          row("Score aggregato (media dei normal score)",
              fmt_z(d$scores$aggregate_z)),
          row("Media ingenua dei percentili",
              fmt_percentile(d$scores$mean_percentile_naive))
        )
      )
    })

    output$exam_table <- DT::renderDT({
      d <- student_detail()
      df <- d$exams
      df <- df[order(!df$mandatory, df$exam_name), ]
      display <- student_exams_display(df)
      display[["Tipo"]] <- ifelse(df$mandatory, "Obbligatorio", "Opzionale")
      app_datatable(display, page_length = 12)
    })
  })
}
