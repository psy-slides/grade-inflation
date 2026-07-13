# Modulo "Distribuzioni degli esami": filtri per CdS/esame/tipo, distribuzioni
# discrete 18-30, heatmap, confronto latente/osservato, statistiche e spie.

mod_distributions_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320, open = "always",
      shiny::selectInput(ns("course"), "Corso di studio", choices = NULL),
      shiny::radioButtons(ns("type"), "Tipo di esame",
                          choices = c("Tutti" = "tutti",
                                      "Obbligatori" = "obbligatori",
                                      "Opzionali" = "opzionali"),
                          selected = "tutti"),
      shiny::selectizeInput(ns("exams"), "Esami (vuoto = tutti i filtrati)",
                            choices = NULL, multiple = TRUE),
      shiny::checkboxInput(ns("show_latent"),
                           "Confronta con la distribuzione latente", value = FALSE)
    ),
    tab_header("Distribuzioni degli esami",
               "I voti osservati restano valori discreti da 18 a 30: le barre mostrano esattamente quanti studenti ricevono ogni voto."),
    shiny::uiOutput(ns("aggregation_note")),
    shiny::uiOutput(ns("unobserved_messages")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Distribuzione dei voti per esame"),
      shiny::plotOutput(ns("distributions_plot"), height = "auto")
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Heatmap esame × voto (quota di studenti)"),
        shiny::plotOutput(ns("heatmap_plot"), height = 340)
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Numerosita' delle alternative opzionali"),
        shiny::plotOutput(ns("optional_plot"), height = 340)
      )
    ),
    shiny::uiOutput(ns("latent_card")),
    bslib::card(
      bslib::card_header("Statistiche descrittive e spie diagnostiche"),
      bslib::card_body(
        shiny::p(class = "text-muted small", paste(
          "\"Stabilita' riferimento\" dipende dalla numerosita' effettiva;",
          "\"Risoluzione\" dalla capacita' della distribuzione osservata di",
          "distinguere gli studenti (valori distinti, pari merito, quota di 30).",
          "Un esame puo' avere N elevato e risoluzione scarsa, o viceversa.")),
        DT::DTOutput(ns("stats_table"))
      )
    )
  )
}

mod_distributions_server <- function(id, scenario) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(scenario(), {
      sc <- scenario()
      shiny::updateSelectInput(
        session, "course",
        choices = c("Tutti i CdS" = "tutti",
                    stats::setNames(sc$courses$course_id, sc$courses$course_name)))
    })

    filtered_exams <- shiny::reactive({
      sc <- scenario()
      df <- dplyr::left_join(sc$exam_statistics,
                             sc$courses[, c("course_id", "course_name")],
                             by = "course_id")
      # All'avvio input$course puo' essere "" finche' updateSelectInput non
      # arriva al client: in quel caso nessun filtro.
      if (shiny::isTruthy(input$course) && input$course != "tutti") {
        df <- df[df$course_id == input$course, ]
      }
      if (identical(input$type, "obbligatori")) df <- df[df$mandatory, ]
      if (identical(input$type, "opzionali")) df <- df[!df$mandatory, ]
      df
    })

    shiny::observeEvent(filtered_exams(), {
      df <- filtered_exams()
      choices <- if (nrow(df) == 0) character(0) else {
        stats::setNames(df$exam_id,
                        paste0(df$exam_name, " (", df$course_name, ")"))
      }
      shiny::updateSelectizeInput(session, "exams", choices = choices,
                                  selected = intersect(input$exams, df$exam_id))
    })

    selected_exams <- shiny::reactive({
      df <- filtered_exams()
      if (length(input$exams) > 0) df <- df[df$exam_id %in% input$exams, ]
      shiny::validate(shiny::need(nrow(df) > 0,
                                  "Nessun esame corrisponde ai filtri selezionati."))
      df
    })

    selected_grades <- shiny::reactive({
      sc <- scenario()
      df <- selected_exams()
      grades <- sc$grades[!sc$grades$failed & sc$grades$exam_id %in% df$exam_id, ]
      grades <- dplyr::left_join(
        grades, df[, c("exam_id", "exam_name", "course_name")], by = "exam_id")
      grades$panel_label <- paste0(grades$exam_name, " (", grades$course_name, ")")
      grades
    })

    output$aggregation_note <- shiny::renderUI({
      if (nrow(selected_exams()) <= 1) return(NULL)
      shiny::div(class = "alert alert-light border small", paste(
        "Stai osservando piu' esami insieme: l'insieme dei voti e' una",
        "descrizione aggregata, non una singola distribuzione omogenea, perche'",
        "ogni esame ha il proprio riferimento."))
    })

    output$unobserved_messages <- shiny::renderUI({
      df <- selected_exams()
      empty <- df[df$n == 0, ]
      if (nrow(empty) == 0) return(NULL)
      shiny::tagList(lapply(empty$exam_name, unobserved_exam_message))
    })

    n_panels <- shiny::reactive(dplyr::n_distinct(selected_grades()$panel_label))

    output$distributions_plot <- shiny::renderPlot({
      grades <- selected_grades()
      shiny::validate(shiny::need(nrow(grades) > 0,
                                  "Nessun voto osservato per gli esami selezionati."))
      plot_grade_distributions_faceted(grades)
    }, res = 96, height = function() 150 + 160 * ceiling(n_panels() / 3))

    output$heatmap_plot <- shiny::renderPlot({
      grades <- selected_grades()
      shiny::validate(shiny::need(nrow(grades) > 0, "Nessun voto osservato."))
      plot_exam_grade_heatmap(grades)
    }, res = 96)

    output$optional_plot <- shiny::renderPlot({
      sc <- scenario()
      optional <- sc$exam_statistics[!sc$exam_statistics$mandatory, ]
      shiny::validate(shiny::need(
        nrow(optional) > 0,
        "Lo scenario corrente non prevede esami opzionali."))
      plot_exam_enrollment(optional)
    }, res = 96)

    output$latent_card <- shiny::renderUI({
      if (!isTRUE(input$show_latent)) return(NULL)
      ns <- session$ns
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Distribuzione latente e distribuzione osservata"),
        bslib::card_body(
          shiny::p(class = "text-muted small", paste(
            "La curva mostra la distribuzione della prestazione latente del primo",
            "tentativo (prima di soglia a 18, soffitto a 30 e arrotondamento).",
            "Richiede la selezione di un singolo esame.")),
          shiny::plotOutput(ns("latent_plot"), height = 340)
        )
      )
    })

    output$latent_plot <- shiny::renderPlot({
      sc <- scenario()
      df <- selected_exams()
      shiny::validate(shiny::need(
        nrow(df) == 1 && df$n[1] > 0,
        "Seleziona un singolo esame osservato per il confronto latente/osservato."))
      grades <- sc$grades[!sc$grades$failed & sc$grades$exam_id == df$exam_id[1], ]
      latent_sd <- sqrt(sc$metadata$lambda^2 + sc$metadata$sigma_eps^2)
      plot_latent_vs_observed(
        grades$grade,
        latent_mean = sc$metadata$base_location + df$delta[1],
        latent_sd = latent_sd,
        title = paste0(df$exam_name[1], " (", df$course_name[1], ")"))
    }, res = 96)

    output$stats_table <- DT::renderDT({
      sc <- scenario()
      app_datatable(exam_statistics_display(selected_exams(), sc$courses),
                    page_length = 12)
    })
  })
}
