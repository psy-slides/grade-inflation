# Modulo "Quadro generale": value box, mappa di CdS ed esami, distribuzioni
# complessive e box interpretativo prudente generato automaticamente.

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tab_header("Quadro generale",
               "Che tipo di scenario e' stato simulato? Uno sguardo d'insieme a voti, abilita' e struttura dei percorsi."),
    shiny::uiOutput(ns("value_boxes")),
    bslib::card(
      bslib::card_header("Lettura sintetica dello scenario"),
      bslib::card_body(shiny::uiOutput(ns("interpretation")))
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Distribuzione complessiva dei voti (18–30)"),
        shiny::plotOutput(ns("grade_distribution"), height = 300),
        bslib::card_footer(shiny::downloadButton(
          ns("dl_grade_plot"), "PNG", class = "btn-sm"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Distribuzione delle abilita' vere simulate"),
        shiny::plotOutput(ns("theta_distribution"), height = 300)
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Relazione tra abilita' vera e voto osservato"),
        shiny::plotOutput(ns("theta_vs_grade"), height = 320)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Numerosita' effettiva per esame"),
        shiny::plotOutput(ns("enrollment_plot"), height = 320)
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Media per esame e quota di 30"),
      shiny::plotOutput(ns("means_plot"), height = 380)
    ),
    bslib::card(
      bslib::card_header("Mappa dei corsi di studio e degli esami"),
      bslib::card_body(DT::DTOutput(ns("exam_table")))
    )
  )
}

mod_overview_server <- function(id, scenario) {
  shiny::moduleServer(id, function(input, output, session) {
    grades_ok <- shiny::reactive({
      sc <- scenario()
      sc$grades[!sc$grades$failed, ]
    })

    output$value_boxes <- shiny::renderUI({
      sc <- scenario()
      g <- grades_ok()
      observed <- sc$exam_statistics[sc$exam_statistics$n > 0, ]
      theta_grade_cor <- {
        df <- dplyr::inner_join(g, sc$students, by = "student_id")
        stats::cor(df$theta, df$grade)
      }
      vb <- function(title, value, subtitle = NULL) bslib::value_box(
        title = title, value = value, p(subtitle),
        theme = bslib::value_box_theme(bg = "#eef4fc", fg = "#153a63")
      )
      bslib::layout_column_wrap(
        width = 1 / 4, fill = FALSE,
        vb("Studenti totali", fmt_int(nrow(sc$students))),
        vb("Corsi di studio", nrow(sc$courses)),
        vb("Esami distinti", nrow(sc$exams)),
        vb("Voto medio", fmt_grade(mean(g$grade), 2)),
        vb("Deviazione standard", fmt_grade(stats::sd(g$grade), 2)),
        vb("Quota di 30", fmt_percent(mean(g$grade == 30))),
        vb("Valori distinti per esame", fmt_num(mean(observed$n_distinct), 1),
           "media tra gli esami"),
        vb("Correlazione voto–theta", fmt_correlation(theta_grade_cor),
           "sui singoli esami")
      )
    })

    output$interpretation <- shiny::renderUI({
      sc <- scenario()
      g <- grades_ok()
      observed <- sc$exam_statistics[sc$exam_statistics$n > 0, ]
      share30 <- mean(g$grade == 30)
      spread_means <- diff(range(observed$mean_grade))
      fragile_exams <- sum(observed$stability == "fragile", na.rm = TRUE)
      poor_resolution <- sum(observed$resolution == "scarsa", na.rm = TRUE)

      phrases <- c(
        if (share30 >= 0.4) {
          "Lo scenario mostra un'elevata concentrazione di voti al massimo: entro molti esami la distribuzione distingue poco gli studenti piu' capaci (effetto soffitto)."
        } else if (share30 >= 0.15) {
          "Una parte non trascurabile dei voti si colloca al massimo della scala: l'effetto soffitto riduce in parte la risoluzione nella fascia alta."
        } else {
          "La quota di voti al massimo e' contenuta: la parte alta della scala conserva una certa capacita' di distinzione."
        },
        if (spread_means >= 2.5) {
          "Le medie dei singoli esami sono molto eterogenee: in queste condizioni il voto medio individuale puo' dipendere sensibilmente dal percorso scelto."
        } else if (spread_means >= 1) {
          "Le medie degli esami differiscono in misura moderata tra loro."
        } else {
          "Gli esami hanno medie simili: il percorso scelto incide poco sul voto medio."
        },
        if (fragile_exams > 0) sprintf(
          "%d esami hanno un riferimento percentile fragile (poche decine di studenti): i percentili su questi esami vanno letti con cautela.",
          fragile_exams),
        if (poor_resolution > 0) sprintf(
          "%d esami mostrano una risoluzione scarsa della scala (molti pari merito), indipendentemente dalla numerosita'.",
          poor_resolution)
      )
      shiny::tagList(
        lapply(phrases, shiny::p),
        shiny::p(class = "text-muted small", paste(
          "Descrizione generata automaticamente sullo scenario simulato corrente;",
          "non esprime valutazioni su corsi o docenti reali."))
      )
    })

    output$grade_distribution <- shiny::renderPlot({
      plot_grade_distribution(grades_ok()$grade)
    }, res = 96)
    output$theta_distribution <- shiny::renderPlot({
      plot_theta_distribution(scenario()$students)
    }, res = 96)
    output$theta_vs_grade <- shiny::renderPlot({
      df <- dplyr::inner_join(grades_ok(), scenario()$students, by = "student_id")
      plot_theta_vs_grade(df)
    }, res = 96)
    output$enrollment_plot <- shiny::renderPlot({
      plot_exam_enrollment(scenario()$exam_statistics)
    }, res = 96)
    output$means_plot <- shiny::renderPlot({
      plot_exam_means(scenario()$exam_statistics)
    }, res = 96)

    output$exam_table <- DT::renderDT({
      sc <- scenario()
      app_datatable(exam_statistics_display(sc$exam_statistics, sc$courses),
                    page_length = 12)
    })

    output$dl_grade_plot <- shiny::downloadHandler(
      filename = function() sprintf("distribuzione-voti-seed%s.png",
                                    scenario()$metadata$seed),
      content = function(file) {
        save_plot_png(file, plot_grade_distribution(
          grades_ok()$grade,
          title = "Distribuzione complessiva dei voti (dati simulati)",
          subtitle = sprintf("Seed %s — %s", scenario()$metadata$seed,
                             format(scenario()$metadata$created_at, "%d/%m/%Y"))))
      }
    )
  })
}
