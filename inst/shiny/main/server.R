source("functions.R")
source("interface.R")
library(dplyr)
library(Hmisc)
library(heemod)
library(rgho)


shinyServer(function(input, output, session) {
  values <- reactiveValues(nGlobalParameters = 1, nEquation = 0, nRgho = 0, nSurvival = 0, nTimedep = 0, nDeterministic = 0)
  localValues <- reactiveValues(loaded = FALSE, updatedState = FALSE, updatedTM = FALSE, updatedSP = FALSE)

  onBookmark(function(state) {
    nameValues <- names(reactiveValuesToList(values))
    sapply(nameValues, function(x){
      state$values[[x]] <- values[[x]]
    })
  })
  
  onRestore(function(state) {
    nameValues <- ls(state$values)
    sapply(nameValues, function(x){
      values[[x]] <- state$values[[x]]
    })
    localValues$currentTab <- input$main
    updateTabsetPanel(session, "main", "States" )
    delay(1, localValues$updatedState <- TRUE)
  })
  
  observe({
    req(localValues$updatedState)
    updateTabsetPanel(session, "main", "Transition Matrix" )
    delay(1, localValues$updatedTM <- TRUE)
  })
  observe({
    req(localValues$updatedTM)
    updateTabsetPanel(session, "main", "State Parameters" )
    delay(1, localValues$updatedSP <- TRUE)
  })
  observe({
    req(localValues$updatedSP)
    updateTabsetPanel(session, "main", localValues$currentTab)
  })
  
  setBookmarkExclude(
    c(
      paste0(MODULES, "OK"),
      "newParam",
      unname(MODULES)
    )
  )
  
  
  observe_timedepNew <- list()
  
  observe_nTimedep <<- observe({
    lapply(0:values$nTimedep, function(n){
      isolate({
        if (sapply(observe_timedepNew[n + 1], is.null)){ #I would have prefered : if (is.null(observe_timedepNew[[n+1]]))
          observe_timedepNew[[n + 1]] <<- observeEvent(input[[paste0("timedepNew", n)]], {
            if (is.null(values[[paste0("nTimedepNC", n)]]))
              values[[paste0("nTimedepNC", n)]] <- 1
            else 
              values[[paste0("nTimedepNC", n)]] <- values[[paste0("nTimedepNC", n)]] + 1
          })
        }
      })
    })
  }) 
  
  
  observe({
    req(localValues$loaded)
        lapply(0:values$nTimedep, function(n) {
          if (!is.null(values[[paste0("nTimedepNC", n)]]) && values[[paste0("nTimedepNC", n)]] > 0){
            isolate({
            lapply(seq_len(values[[paste0("nTimedepNC", n)]]), function(i){
                removeUI(paste0("#timedepValueNCLine", n, i), multiple=TRUE) #Remove then insert is suboptimal, but works. Reactivity...
                insertUI(selector = paste0("#timedepNC", n), ui=
                           fluidRow(id = paste0("timedepValueNCLine", n, i),
                                    column(4, textInput(paste0("timedepValueNC", n, i), NULL, ifelse (!is.null(input[[paste0("timedepValueNC", n, i)]]), input[[paste0("timedepValueNC", n, i)]], ""))),
                                    column(4, numericInput(paste0("timedepStart", n, i), NULL, ifelse (!is.null(input[[paste0("timedepStart", n, i)]]), input[[paste0("timedepStart", n, i)]], ""))),
                                    column(4, numericInput(paste0("timedepEnd", n, i), NULL, ifelse (!is.null(input[[paste0("timedepEnd", n, i)]]), input[[paste0("timedepEnd", n, i)]], "")))
                           )
                )
            })
            })
          }
        })
      }, priority= - 1)

  observe({
    inFile <- input$loadButton
    if (is.null(inFile))
      return(NULL)
    else {
      load(inFile$datapath)
      updateNumericInput(session, "nbStates", value = input$nbStates)
      updateNumericInput(session, "nbStateVariables", value = input$nbStateVariables)
      updateNumericInput(session, "nbStrategies", value = input$nbStrategies)
      updateCheckboxInput(session, "use_morta", value = input$use_morta)
      updateNumericInput(session, "startAge", value = input$startAge)
      updateNumericInput(session, "cycleLength", value = input$cycleLength)
      updateRadioButtons(session, "gender", selected = input$gender)
      updateSelectInput(session,"countMethod", selected = input$countMethod)
      updateNumericInput(session, "cycles", value = input$cycles)
    }
  })
  
  output$saveButton <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.RData')
    },
    content = function(file) {
      save(input, values, file=file)
    }
  )
  
  output$nameStates <- renderUI({
    req(input$nbStates)
    lapply(
      seq_len(input$nbStates),
      function(i) {
        isolate({
          textInput(
            paste0("stateName", i),
            paste("State Name", i),
            value = ifelse(
              !is.null(input[[paste0("stateName",i)]]),
              input[[paste0("stateName",i)]],
              LETTERS[i]))
        })
      })
  })
  
  output$nameStateVariables <- renderUI({
    req(input$nbStateVariables)

    lapply(
      seq_len(input$nbStateVariables),
      function(i) {
        isolate({
          textInput(
            paste0("variableStateName", i),
            paste("Variable Name", i),
            value = ifelse(
              !is.null(input[[paste0("variableStateName",i)]]),
              input[[paste0("variableStateName",i)]],
              if (i == 1)
                "cost"
              else if (i==2)
                "outcome"
              else
                paste0("variable_",i)))
        })
      })
  })
  
  output$nameStrategies <- renderUI({
    req(input$nbStrategies)
    
    lapply(
      seq_len(input$nbStrategies),
      function(i) {
        isolate({
          textInput(
            paste0("strategyName", i),
            paste("Strategy Name", i),
            value = ifelse(
              !is.null(input[[paste0("strategyName",i)]]),
              input[[paste0("strategyName",i)]],
              as.character(as.roman(i))))
        })
      })
  })
  
  
  output$transMatrix1 <- renderUI({
    show_first(val = "TM1", FUN = showTransMatrix, input)    
  })
  
  output$transMatrix2 <- renderUI({
    show_next(val = "TM2", trigger = "copyValuesParametersTM", input, values, showTransMatrix)
    
  })  
  
  output$stateParameters1 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_first(val = "SP1", FUN = showStateParam, input)
  })
  
  output$stateParameters2 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_next(val = "SP2", trigger = "copyValuesParametersSP", input, values, showStateParam)
  })
  
  
  
  output$costVariable <- renderUI({
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = input$variableStateName1
    )
  })
  output$effectVariable <- renderUI({
    textInput(
      "effectVariable",
      label = "Effect Variable",
      value = input$variableStateName2
    )
  })
  
  observeEvent(input$addParametersGP, {
    isolate(values$nGlobalParameters <- values$nGlobalParameters + 1)
  })
  
  
  add_deterministic <- NULL
  
  output$DSA <- renderUI({
    choices <- get_names_SA(input, values)
    req(length(choices) > 0)
    
    ## Observer needed for bookmark and insertUI
    observe({
      req(values$nDeterministic > 0)
      isolate({
        if (!is.null(add_deterministic)) add_deterministic$destroy()
        add_deterministic <<- observe({
          values$nDeterministic
          choices <- get_names_SA(input, values)
          isolate({
            lapply(seq_len(values$nDeterministic), function(i){
              removeUI(paste0("#DSA_div",i))
            })
            lapply(seq_len(values$nDeterministic), function(i){
              insertUI("#DSAtable",
                       ui = show_DSA_div(input, values, choices, i)
              )
            })
          })
        })
      })
    })
    ## End
    
    i = 0
    tagList(
    column(id = "DSAtable", 12,
           isolate(show_DSA_div(input, values, choices, i))
      ),
    column(12,
    div(class="centerdiv",
        actionButton("addDeterministic", "Add a deterministic value")
    ))
    )
  })
  

  

  
  
  observeEvent(input$addDeterministic, {
    values$nDeterministic <- values$nDeterministic + 1

  })
  
  output$outInit <- renderUI({
    #####
    req(
      nbState <- ux_nb_states(input),
      stateNames <- ux_state_names(input)
    )
    tagList(
      tags$h3("Initial counts per state"),
      tags$table(
        tagList(
          list(
            tags$th(""),
            tags$th(style='text-align:center', "Count")
          ),
          lapply(
            seq_len(nbState),
            function(i) {
              tags$tr(
                list(
                  tags$td(
                    strong(stateNames[i])),
                  tags$td(
                    if (i == 1) {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 1000,
                        width="100%"
                      )
                    } else {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = 1000,
                        width="100%"
                      )
                    }
                  )
                )
              )
            }
          )
        )
      )
    )
  })
  
  output$outModel <- renderUI({
    #####
    values$model <- ux_run_models(input = input, values = values)
    values$summary_model <- summary(values$model)
    
    if (is.null(values$model)) {
      tagList(tags$h3("Model specification incomplete"))
    } else {
      tagList(
        tags$h1("Model results"),
        tags$h3("Total values")
      )
    }
  })
  
  output$tableResults <- DT::renderDataTable({
    #####
    req(values$model)
    req(values$summary_model$res)
    
    DT::datatable(
      values$summary_model$res,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$titleICER <- renderUI({
    #####
    req(values$model)
    req(values$summary_model$res_comp)
    
    tagList(
      tags$h3("Efficiency frontier"),
      tags$p(paste(values$summary_model$frontier, collapse = " -> ")),
      tags$h3("Model comparison")
    )
  })
  
  output$tableICER <- DT::renderDataTable({
    #####
    req(values$model)
    req(values$summary_model$res_comp)
    
    DT::datatable(
      values$summary_model$res_comp,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$outCounts <- renderUI({
    #####
    
    req(values$model)
    
    tagList(
      tags$h3("Plot state membership count"),
      selectInput(
        inputId = "modelPlotCounts",
        label = "Model",
        choices = as.vector(ux_model_names(input))
      )
    )
  })
  
  
  output$plotCounts <- renderPlot({
    #####
    req(values$model)
    model <- input$modelPlotCounts
    req(model)
    plot(
      values$model,
      type = "counts",
      model = model
    ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_colour_brewer(
        name = "State",
        palette = "Set1"
      )
  },
  width = 600)
  
  output$debugParams <- renderUI({
    req(ux_nb_models(input))
    tagList(
      lapply(
        seq_len(ux_nb_models(input)),
        function(x) {
          renderPrint(ux_parameters(input, values, x))
        }
      )
    )
  })
  
  output$debugModels <- renderUI({
    req(ux_nb_models(input))
    tagList(
      lapply(
        seq_len(ux_nb_models(input)),
        function(x) {
          renderPrint(ux_model(
            input = input,
            values = values,
            model_number = x
          ))
        })
    )
  })
  
  output$debugRunModels <- renderPrint({
    ux_run_models_raw(input, values)
  })
  
  output$globalParameters <- renderUI({
    fluidRow(
      column(
        1,
        actionLink("newParam", "", icon("plus-circle", class="fa-3x rotateIcon")), style="margin-bottom:40px")
      ,
      column(
        11,
        hidden(
          fluidRow(
            id = "tabnewParam",
            lapply(seq_along(MODULES), function(i){
              fluidRow(
                actionLink(MODULES[i], names(MODULES)[i], class = "btn btn-link")
                )
            })
          )
        )
      )
    )
  })
  
  onevent(
    "mouseenter",
    "newParam",
    show(
      "tabnewParam",
      anim = TRUE,
      animType = "fade"
    ))
  onevent(
    "mouseleave",
    "globalParameters",
    hide(
      "tabnewParam",
      anim = TRUE,
      animType = "fade"
    ))
  
  lapply(MODULES, function(module) {
    observeEvent(input[[module]], {
      for (mod in MODULES){
          removeUI(paste0("#editing", upFirst(mod)))
      }
      if (module ==  "rgho" && (is.null(REGION) | is.null(COUNTRY))){
        try({
          REGION <<- get_gho_codes(dimension = "REGION")
          COUNTRY <<- get_gho_codes(dimension="COUNTRY")
        })
        if (is.null(REGION) | is.null(COUNTRY)){
          showNotification("GHO server is not reacheable for the moment. Please try again later.", duration = 5, type = "warning")
          #div(class = "alert alert-warning", "WHO is not reacheable for the moment")
        }
      }
      else
      insertUI("#addModule", ui=show_module(module, edit = TRUE, n = values[[paste0("n", upFirst(module))]], input, values, localValues))
    })
  })
  
  observe({
    insert_module_line("equation", input, values, localValues)
  })

  observeEvent(input$equationOK, {
    removeUI("#editingEquation")
    values$nEquation <- values$nEquation + 1
  })
  observe({
    insert_module_line("rgho", input, values, localValues)
  })
  observeEvent(input$rghoOK, {
    removeUI("#editingRgho")
    values$nRgho <- values$nRgho + 1
  })
  observe({
    insert_module_line("survival", input, values, localValues)
  })
  observeEvent(input$survivalOK, {
    removeUI("#editingSurvival")
    values$nSurvival <- values$nSurvival + 1
  })
  observe({
    values[["nTimedep"]]
    isolate(insert_module_line("timedep", input, values, localValues))
  })
  
  observeEvent(input$timedepOK, {
    removeUI("#editingTimedep")
    n <- values$nTimedep
    values$nTimedep <- n + 1
  }) 
})

