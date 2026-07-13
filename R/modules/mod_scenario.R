# Modulo "Configura lo scenario": parametri, preset, simulazione esplicita,
# riepilogo dello scenario attivo, download. Restituisce il reactive scenario.

mod_scenario_ui <- function(id) {
  ns <- shiny::NS(id)
  app_config <- load_app_config()
  d <- app_config$defaults
  lim <- app_config$limits

  sidebar_content <- bslib::sidebar(
    width = 380, open = "always",
    shiny::selectInput(
      ns("preset"), label_with_info(
        "Preset di scenario",
        "I preset compilano i controlli; la simulazione parte solo con il pulsante."),
      choices = c("— scegli un preset —" = "", names(app_config$presets))
    ),
    shiny::sliderInput(ns("n_courses"), "Corsi di studio",
                       min = lim$n_courses[[1]], max = lim$n_courses[[2]],
                       value = d$n_courses, step = 1, ticks = FALSE),
    shiny::sliderInput(ns("n_mandatory"), "Esami obbligatori per CdS",
                       min = 1, max = lim$n_mandatory[[2]],
                       value = d$n_mandatory, step = 1, ticks = FALSE),
    shiny::sliderInput(ns("n_optional_groups"), "Gruppi di esami opzionali per CdS",
                       min = lim$n_optional_groups[[1]],
                       max = lim$n_optional_groups[[2]],
                       value = d$n_optional_groups, step = 1, ticks = FALSE),
    shiny::conditionalPanel(
      condition = "input.n_optional_groups > 0", ns = ns,
      shiny::sliderInput(ns("n_alternatives"), "Alternative per gruppo opzionale",
                         min = lim$n_alternatives_per_group[[1]],
                         max = lim$n_alternatives_per_group[[2]],
                         value = d$n_alternatives_per_group, step = 1,
                         ticks = FALSE)
    ),
    shiny::uiOutput(ns("exam_count_note")),
    shiny::sliderInput(ns("n_students"), "Studenti per CdS",
                       min = 50, max = lim$n_students_per_course[[2]],
                       value = d$n_students_per_course, step = 25, ticks = FALSE),
    shiny::sliderInput(ns("target_mean"), "Voto medio complessivo desiderato",
                       min = lim$target_mean_grade[[1]],
                       max = lim$target_mean_grade[[2]],
                       value = d$target_mean_grade, step = 0.1, ticks = FALSE),
    shiny::sliderInput(
      ns("heterogeneity"),
      label_with_info("Eterogeneita' tra esami",
                      paste("Deviazione standard delle posizioni degli esami sulla",
                            "scala latente: 0 = esami con la stessa generosita',",
                            "4 = medie molto diverse.")),
      min = lim$exam_heterogeneity[[1]], max = lim$exam_heterogeneity[[2]],
      value = d$exam_heterogeneity, step = 0.1, ticks = FALSE),
    shiny::radioButtons(
      ns("reliability"),
      label_with_info("Affidabilita' generale degli esami",
                      paste("Quanto i voti riflettono l'abilita' anziche' errore",
                            "casuale (comune a tutti gli esami). Diversa dalla",
                            "stabilita' del riferimento percentile, che dipende",
                            "dalla numerosita'.")),
      choices = c("Bassa" = "bassa", "Media" = "media", "Alta" = "alta"),
      selected = d$reliability_level, inline = TRUE),
    shiny::radioButtons(
      ns("choice_mode"), "Scelta degli esami opzionali",
      choices = c("Scelta casuale" = "casuale", "Scelta strategica" = "strategica"),
      selected = d$choice_mode),
    shiny::conditionalPanel(
      condition = "input.choice_mode == 'strategica'", ns = ns,
      shiny::sliderInput(ns("strategic_strength"),
                         "Forza della scelta strategica",
                         min = 0, max = lim$strategic_strength[[2]],
                         value = d$strategic_strength, step = 0.1, ticks = FALSE),
      shiny::sliderInput(ns("hard_pref"),
                         "Preferenza dei migliori per gli esami piu' difficili",
                         min = 0, max = lim$hard_exam_preference[[2]],
                         value = d$hard_exam_preference, step = 0.1,
                         ticks = FALSE),
      shiny::helpText(paste(
        "Una scelta strategica piu' intensa concentra gli studenti negli esami",
        "con voti attesi piu' alti. Il secondo parametro introduce selezione:",
        "gli studenti con abilita' maggiore possono preferire esami piu' difficili."))
    ),
    shiny::radioButtons(
      ns("percentile_method"),
      label_with_info("Metodo percentile",
                      paste("Rango medio: chi condivide lo stesso voto occupa la",
                            "posizione centrale della fascia dei pari merito.",
                            "Conservativo: ogni gruppo riceve il limite inferiore",
                            "della fascia (metodo severo, non default).")),
      choices = c("Rango medio (default)" = "rango_medio",
                  "Conservativo (limite inferiore)" = "conservativo"),
      selected = d$percentile_method),
    shiny::numericInput(ns("seed"), "Seed (riproducibilita')",
                        value = d$seed, step = 1),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Impostazioni avanzate",
        shiny::textInput(
          ns("students_custom"),
          label_with_info("N per CdS separati",
                          "Interi separati da virgola, uno per CdS (es. 200,150,300). Vuoto = N globale."),
          value = d$students_per_course_custom, placeholder = "es. 200,150,300"),
        shiny::sliderInput(ns("softmax_temperature"), "Temperatura della softmax",
                           min = lim$softmax_temperature[[1]],
                           max = lim$softmax_temperature[[2]],
                           value = d$softmax_temperature, step = 0.1,
                           ticks = FALSE),
        shiny::numericInput(ns("max_attempts"),
                            "Numero massimo di tentativi per esame",
                            value = d$max_attempts,
                            min = lim$max_attempts[[1]],
                            max = lim$max_attempts[[2]])
      )
    ),
    shiny::uiOutput(ns("validation_messages")),
    shiny::uiOutput(ns("dirty_badge")),
    shiny::actionButton(ns("simulate"), "Simula scenario",
                        class = "btn-primary btn-lg w-100",
                        icon = shiny::icon("play"))
  )

  bslib::layout_sidebar(
    sidebar = sidebar_content,
    tab_header(
      "Configura lo scenario",
      paste("Definisci un ciclo ipotetico di carriere universitarie simulate e",
            "osserva come cambiano il valore informativo dei voti e il recupero",
            "dell'abilita' simulata. I parametri diventano attivi solo premendo",
            "\"Simula scenario\".")),
    simulated_data_note(),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        bslib::card_header("Scenario attivo"),
        bslib::card_body(shiny::uiOutput(ns("active_summary")))
      ),
      bslib::card(
        bslib::card_header("Note e avvisi"),
        bslib::card_body(shiny::uiOutput(ns("scenario_warnings")))
      )
    ),
    bslib::card(
      bslib::card_header("Esporta lo scenario"),
      bslib::card_body(
        shiny::p(class = "text-muted", paste(
          "Tutti i file esportati includono seed, data di generazione,",
          "configurazione e il disclaimer sui dati simulati.")),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::downloadButton(ns("dl_config_yaml"), "Configurazione (YAML)"),
          shiny::downloadButton(ns("dl_config_json"), "Configurazione (JSON)"),
          shiny::downloadButton(ns("dl_exams_csv"), "Esami (CSV)"),
          shiny::downloadButton(ns("dl_students_csv"), "Studenti (CSV)"),
          shiny::downloadButton(ns("dl_recovery_csv"), "Recovery (CSV)")
        )
      )
    )
  )
}

mod_scenario_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    app_config <- load_app_config()

    # I preset compilano i controlli; non lanciano la simulazione.
    shiny::observeEvent(input$preset, {
      preset <- app_config$presets[[input$preset]]
      if (is.null(preset)) return()
      shiny::updateSliderInput(session, "target_mean",
                               value = preset$target_mean_grade)
      shiny::updateSliderInput(session, "heterogeneity",
                               value = preset$exam_heterogeneity)
      shiny::updateRadioButtons(session, "reliability",
                                selected = preset$reliability_level)
      shiny::updateRadioButtons(session, "choice_mode",
                                selected = preset$choice_mode)
      shiny::updateSliderInput(session, "strategic_strength",
                               value = preset$strategic_strength)
      shiny::updateSliderInput(session, "hard_pref",
                               value = preset$hard_exam_preference)
    })

    output$exam_count_note <- shiny::renderUI({
      n_groups <- input$n_optional_groups
      n_alt <- if (n_groups > 0) input$n_alternatives else 0
      total <- input$n_mandatory + n_groups * n_alt
      taken <- input$n_mandatory + n_groups
      shiny::helpText(sprintf(
        "Esami offerti per CdS: %d. Esami sostenuti da ogni studente: %d.",
        total, taken))
    })

    current_config <- shiny::reactive({
      shiny::req(input$seed, input$max_attempts)
      make_scenario_config(
        n_courses = input$n_courses,
        n_mandatory = input$n_mandatory,
        n_optional_groups = input$n_optional_groups,
        n_alternatives_per_group = if (input$n_optional_groups > 0)
          input$n_alternatives else app_config$defaults$n_alternatives_per_group,
        n_students_per_course = input$n_students,
        students_per_course_custom = input$students_custom %||% "",
        target_mean_grade = input$target_mean,
        exam_heterogeneity = input$heterogeneity,
        reliability_level = input$reliability,
        choice_mode = input$choice_mode,
        strategic_strength = if (input$choice_mode == "strategica")
          input$strategic_strength else 0,
        hard_exam_preference = if (input$choice_mode == "strategica")
          input$hard_pref else 0,
        softmax_temperature = input$softmax_temperature,
        percentile_method = input$percentile_method,
        max_attempts = input$max_attempts,
        seed = input$seed
      )
    })

    output$validation_messages <- shiny::renderUI({
      check <- validate_scenario_config(current_config(), app_config)
      if (check$valid && length(check$warnings) == 0) return(NULL)
      items <- c(
        lapply(check$errors, function(e)
          shiny::div(class = "text-danger small", shiny::icon("circle-xmark"), e)),
        lapply(check$warnings, function(w)
          shiny::div(class = "text-warning-emphasis small",
                     shiny::icon("triangle-exclamation"), w))
      )
      shiny::div(class = "validation-box", items)
    })

    # La simulazione parte SOLO con il pulsante (ignoreNULL = FALSE la lancia
    # una volta all'avvio con i default, cosi' le tab non sono vuote).
    scenario <- shiny::eventReactive(input$simulate, ignoreNULL = FALSE, {
      config <- current_config()
      check <- validate_scenario_config(config, app_config)
      shiny::validate(shiny::need(
        check$valid,
        paste("Configurazione non valida:", paste(check$errors, collapse = " "))))
      shiny::withProgress(message = "Simulazione dello scenario", value = 0, {
        build_scenario(config, app_config, progress = function(msg, frac) {
          shiny::setProgress(value = frac, detail = msg)
        })
      })
    })

    applied_config <- shiny::reactiveVal(NULL)
    shiny::observeEvent(scenario(), applied_config(scenario()$config))

    output$dirty_badge <- shiny::renderUI({
      applied <- applied_config()
      if (is.null(applied)) return(NULL)
      if (identical(unclass(current_config()), unclass(applied))) {
        shiny::div(class = "sim-status sim-status-ok",
                   shiny::icon("check"),
                   "I controlli corrispondono allo scenario visualizzato.")
      } else {
        shiny::div(class = "sim-status sim-status-dirty",
                   shiny::icon("triangle-exclamation"),
                   paste("Parametri modificati ma non ancora applicati:",
                         "premi \"Simula scenario\" per aggiornare."))
      }
    })

    output$active_summary <- shiny::renderUI({
      sc <- scenario()
      md <- sc$metadata
      dl <- function(term, value) shiny::tagList(
        shiny::tags$dt(term), shiny::tags$dd(value))
      shiny::tags$dl(
        class = "row-dl",
        dl("Corsi di studio", nrow(sc$courses)),
        dl("Esami offerti", nrow(sc$exams)),
        dl("Studenti simulati", fmt_int(nrow(sc$students))),
        dl("Voto medio osservato",
           paste0(fmt_grade(md$observed_mean, 2), " (target ",
                  fmt_grade(sc$config$target_mean_grade, 1), ")")),
        dl("Affidabilita' generale",
           paste0(sc$config$reliability_level, " (attesa ",
                  fmt_num(md$reliability_value, 2), ")")),
        dl("Scelta opzionali", sc$config$choice_mode),
        dl("Metodo percentile",
           ifelse(sc$config$percentile_method == "rango_medio",
                  "rango medio", "conservativo")),
        dl("Seed", md$seed),
        dl("Simulato il", format(md$created_at, "%d/%m/%Y %H:%M"))
      )
    })

    output$scenario_warnings <- shiny::renderUI({
      sc <- scenario()
      warnings <- sc$metadata$warnings
      if (length(warnings) == 0) {
        return(shiny::p(class = "text-muted",
                        "Nessun avviso per lo scenario corrente."))
      }
      shiny::tags$ul(lapply(warnings, shiny::tags$li))
    })

    # ---- Download -----------------------------------------------------------
    export_config <- function(sc) {
      c(list(
        disclaimer = "Dati interamente simulati - nessun dato reale.",
        generated_at = format(sc$metadata$created_at, "%Y-%m-%d %H:%M:%S"),
        seed = sc$metadata$seed
      ), unclass(sc$config))
    }
    output$dl_config_yaml <- shiny::downloadHandler(
      filename = function() sprintf("scenario-config-seed%s.yml", input$seed),
      content = function(file) yaml::write_yaml(export_config(scenario()), file)
    )
    output$dl_config_json <- shiny::downloadHandler(
      filename = function() sprintf("scenario-config-seed%s.json", input$seed),
      content = function(file) jsonlite::write_json(
        export_config(scenario()), file, auto_unbox = TRUE, pretty = TRUE)
    )
    output$dl_exams_csv <- shiny::downloadHandler(
      filename = function() sprintf("esami-simulati-seed%s.csv", input$seed),
      content = function(file) {
        sc <- scenario()
        df <- dplyr::left_join(sc$exam_statistics,
                               sc$courses[, c("course_id", "course_name")],
                               by = "course_id")
        write_csv_with_header(df, file, sc)
      }
    )
    output$dl_students_csv <- shiny::downloadHandler(
      filename = function() sprintf("studenti-simulati-seed%s.csv", input$seed),
      content = function(file) {
        sc <- scenario()
        df <- dplyr::inner_join(sc$students, sc$student_scores, by = "student_id")
        write_csv_with_header(df, file, sc)
      }
    )
    output$dl_recovery_csv <- shiny::downloadHandler(
      filename = function() sprintf("recovery-seed%s.csv", input$seed),
      content = function(file) write_csv_with_header(
        scenario()$recovery$overall, file, scenario())
    )

    list(scenario = scenario)
  })
}
