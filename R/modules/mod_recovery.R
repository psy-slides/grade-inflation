# Modulo "Recovery complessivo": confronto empirico tra indicatori nel recuperare
# l'abilita' simulata. Il "vantaggio" e' sempre riferito allo scenario corrente.

mod_recovery_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    tab_header("Recovery complessivo",
               "Quanto bene il voto medio e l'indicatore percentile recuperano l'abilita' vera simulata, per tutti gli studenti dello scenario."),
    shiny::uiOutput(ns("headline_boxes")),
    bslib::card(
      bslib::card_header("Confronto empirico nello scenario corrente"),
      bslib::card_body(shiny::uiOutput(ns("advantage_text")))
    ),
    bslib::card(
      bslib::card_header("Metriche di recovery"),
      bslib::card_body(
        DT::DTOutput(ns("metrics_table")),
        shiny::p(class = "text-muted small", paste(
          "Tutte le stime sono su scala standardizzata (trasformazioni",
          "documentate nella tab \"Metodi e assunzioni\"). La media ingenua dei",
          "percentili e' inclusa solo come termine di confronto."))
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Theta vs posizione dal voto medio"),
        shiny::plotOutput(ns("scatter_mean"), height = 330)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Theta vs posizione dai percentili"),
        shiny::plotOutput(ns("scatter_pct"), height = 330)
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Errori delle due stime lungo theta"),
        shiny::plotOutput(ns("errors_plot"), height = 330)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Distribuzioni delle stime"),
        shiny::plotOutput(ns("distributions_plot"), height = 330)
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Classificazione per decili — voto medio"),
        shiny::plotOutput(ns("decile_mean"), height = 360)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Classificazione per decili — percentili"),
        shiny::plotOutput(ns("decile_pct"), height = 360)
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Recovery per corso di studio"),
        bslib::card_body(
          shiny::plotOutput(ns("by_course_plot"), height = 300),
          shiny::p(class = "text-muted small",
                   "Correlazioni calcolate entro ciascun CdS (theta ristandardizzato).")
        )
      ),
      bslib::card(
        bslib::card_header("Recovery per percorso opzionale"),
        bslib::card_body(DT::DTOutput(ns("by_path_table")))
      )
    ),
    bslib::card(
      bslib::card_header("Esporta"),
      bslib::card_body(shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::downloadButton(ns("dl_scatter_png"), "Grafico recovery (PNG)"),
        shiny::downloadButton(ns("dl_errors_png"), "Grafico errori (PNG)")
      ))
    )
  )
}

mod_recovery_server <- function(id, scenario) {
  shiny::moduleServer(id, function(input, output, session) {

    recovery_df <- shiny::reactive({
      sc <- scenario()
      df <- dplyr::inner_join(sc$student_scores,
                              sc$recovery$theta_std, by = "student_id")
      df <- dplyr::inner_join(df, sc$students[, c("student_id", "course_id")],
                              by = "student_id")
      df$true_decile <- percentile_to_decile(midrank_percentile(df$theta_std))
      df
    })

    main_metrics <- shiny::reactive({
      overall <- scenario()$recovery$overall
      list(
        mean = overall[overall$method == "Voto medio (standardizzato)", ],
        pct = overall[overall$method == "Percentili aggregati (normal score)", ]
      )
    })

    output$headline_boxes <- shiny::renderUI({
      m <- main_metrics()
      vb <- function(title, value, subtitle) bslib::value_box(
        title = title, value = value, shiny::p(subtitle),
        theme = bslib::value_box_theme(bg = "#eef4fc", fg = "#153a63"))
      bslib::layout_column_wrap(
        width = 1 / 4, fill = FALSE,
        vb("r con theta — voto medio", fmt_correlation(m$mean$pearson),
           "correlazione di Pearson"),
        vb("r con theta — percentili", fmt_correlation(m$pct$pearson),
           "correlazione di Pearson"),
        vb("RMSE — voto medio", fmt_error(m$mean$rmse), "scala standardizzata"),
        vb("RMSE — percentili", fmt_error(m$pct$rmse), "scala standardizzata")
      )
    })

    output$advantage_text <- shiny::renderUI({
      m <- main_metrics()
      diff_r <- m$pct$pearson - m$mean$pearson
      sentence <- if (!is.finite(diff_r)) {
        "Le correlazioni non sono calcolabili in questo scenario (varianza degenerata)."
      } else if (abs(diff_r) < 0.01) {
        sprintf(paste(
          "Nel presente scenario i due indicatori recuperano l'abilita' simulata",
          "in modo sostanzialmente equivalente (differenza di correlazione: %s)."),
          fmt_correlation(diff_r, 3))
      } else if (diff_r > 0) {
        sprintf(paste(
          "Nel presente scenario la correlazione con theta e' maggiore di %s per",
          "l'indicatore percentile rispetto al voto medio. Si tratta di una",
          "differenza empirica in questo scenario, non di una proprieta'",
          "universale del metodo."), fmt_correlation(diff_r, 3))
      } else {
        sprintf(paste(
          "Nel presente scenario l'indicatore percentile recupera l'abilita'",
          "simulata PEGGIO del voto medio (differenza di correlazione: %s).",
          "Puo' accadere, ad esempio, quando studenti molto capaci si concentrano",
          "negli stessi esami o quando i riferimenti percentili sono fragili."),
          fmt_correlation(diff_r, 3))
      }
      shiny::p(sentence)
    })

    output$metrics_table <- DT::renderDT({
      app_datatable(recovery_overall_display(scenario()$recovery$overall),
                    page_length = 5, searching = FALSE)
    })

    output$scatter_mean <- shiny::renderPlot({
      plot_recovery_scatter(recovery_df(), "estimate_from_mean_grade",
                            "Voto medio (standardizzato)")
    }, res = 96)
    output$scatter_pct <- shiny::renderPlot({
      plot_recovery_scatter(recovery_df(), "estimate_from_percentiles",
                            "Percentili aggregati (normal score)")
    }, res = 96)
    output$errors_plot <- shiny::renderPlot({
      plot_recovery_errors(recovery_df())
    }, res = 96)
    output$distributions_plot <- shiny::renderPlot({
      plot_estimate_distributions(recovery_df())
    }, res = 96)
    output$decile_mean <- shiny::renderPlot({
      df <- recovery_df()
      est_decile <- percentile_to_decile(
        midrank_percentile(df$estimate_from_mean_grade))
      plot_decile_matrix(df$true_decile, est_decile, "Voto medio (standardizzato)")
    }, res = 96)
    output$decile_pct <- shiny::renderPlot({
      df <- recovery_df()
      plot_decile_matrix(df$true_decile, df$decile,
                         "Percentili aggregati (normal score)")
    }, res = 96)
    output$by_course_plot <- shiny::renderPlot({
      sc <- scenario()
      plot_recovery_by_course(sc$recovery$by_course, sc$courses)
    }, res = 96)

    output$by_path_table <- DT::renderDT({
      by_path <- scenario()$recovery$by_path
      shiny::validate(shiny::need(
        nrow(by_path) > 0, "Lo scenario corrente non prevede esami opzionali."))
      display <- tibble::tibble(
        "CdS" = by_path$course_id,
        "Percorso opzionale" = by_path$path,
        "Indicatore" = by_path$method,
        "N" = by_path$n,
        "r con theta" = fmt_correlation(by_path$pearson),
        "RMSE" = fmt_error(by_path$rmse)
      )
      app_datatable(display, page_length = 10)
    })

    output$dl_scatter_png <- shiny::downloadHandler(
      filename = function() sprintf("recovery-scatter-seed%s.png",
                                    scenario()$metadata$seed),
      content = function(file) save_plot_png(
        file, plot_recovery_scatter(recovery_df(), "estimate_from_percentiles",
                                    "Percentili aggregati (normal score)"))
    )
    output$dl_errors_png <- shiny::downloadHandler(
      filename = function() sprintf("recovery-errori-seed%s.png",
                                    scenario()$metadata$seed),
      content = function(file) save_plot_png(file,
                                             plot_recovery_errors(recovery_df()))
    )
  })
}
