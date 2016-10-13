source("interface.R")
library(dplyr)
library(Hmisc)
library(heemod)
library(rgho)

REGION <- NULL
COUNTRY <- NULL

try({
  REGION <- get_gho_codes(dimension = "REGION")
  COUNTRY <- get_gho_codes(dimension="COUNTRY")
})

MODULES <- c(
  "Simple equation" = "equation",
  "WHO mortality rate" = "rgho",
  "Survival modeling" = "survival",
  "Time-dependant variable" = "timedep"
)

searchRegion <- function(n, input){
  regionNames <- REGION %>%
    attr("labels")
  
  regionNames <- ifelse(
    regionNames == "NA" | grepl("^Not ", regionNames),
    "------",
    regionNames
  )
  
  vRegionCodes <- as.vector(REGION)
  names(vRegionCodes) <- regionNames
  selectizeInput(
    paste0("rghoRegion", n),
    NULL,
    selected = ifelse(!is.null(input[[paste0("rghoRegion", n)]]), input[[paste0("rghoRegion", n)]], "GLOBAL"),
    choices = vRegionCodes
  )
}

searchCountry <- function(n, input){
     req(input[[paste0("rghoRegion", n)]])
  countryCodes <- filter_attrs(
    COUNTRY,
    WHO_REGION_CODE == input[[paste0("rghoRegion", n)]]
  )
  countryNames <- countryCodes %>%
    attr("labels")
  
  vCountryCodes <- as.vector(c("Global", countryCodes))
  names(vCountryCodes) <- c("Global", countryNames)
  
  selectizeInput(
    paste0("rghoCountry", n),
    NULL,
    choices = vCountryCodes,
    selected = ifelse(!is.null(input[[paste0("rghoCountry", n)]]), input[[paste0("rghoCountry", n)]], "GLOBAL")
  )
}

show_module <- function(module, edit, n, input, values, localValues){
  if (edit)
    values$moduleEdit <- TRUE
  else
    values$moduleEdit <- FALSE
  if (module == "equation") {
    tableTitle <- tagList(
                    column(2, strong("Name")),
                    column(2, strong("Value"))
    )
    tableBody <- tagList(
      column(2, textInput(paste0("equationName", n), NULL, ifelse(!is.null(input[[paste0("equationName", n)]]), input[[paste0("equationName", n)]], ""))),
      column(2, textInput(paste0("equationValue", n), NULL, ifelse(!is.null(input[[paste0("equationValue", n)]]), input[[paste0("equationValue", n)]], "")))
    )
  } else if (module == "rgho"){
    tableBody <- tagList(
      column(2, textInput(paste0("rghoName", n), NULL, ifelse(!is.null(input[[paste0("rghoName", n)]]), input[[paste0("rghoName", n)]], ""))),
      column(2, textInput(paste0("rghoStartAge", n), NULL, ifelse(!is.null(input[[paste0("rghoStartAge", n)]]), input[[paste0("rghoStartAge", n)]], ""))),
      column(2, selectInput(paste0("rghoGender", n), NULL, choices = c(Female = "FMLE", Male = "MLE"), selected = input[[paste0("rghoGender", n)]])),
      column(2, searchRegion(n, input)),
      column(2, renderUI(searchCountry(n, input)))
    )
    tableTitle <- tagList(
      column(2, strong("Name")),
      column(2, strong("Age at beginning")),
      column(2, strong("Gender")),
      column(2, strong("Region")),
      column(2, strong("Country"))
    )
  } else if (module == "survival"){
    if (!sapply(observe_show_module_survival[n + 1], is.null)){
      observe_show_module_survival[[n + 1]]$destroy()
    }
    observe_show_module_survival[[n + 1]] <<- observe({
        if (edit){
          prefix <- "#editingSurvival"
          suffix <- ""
          whereBody <- "beforeEnd"
        }
        else {
          prefix <- "#survival"
          suffix <- " div:last"
          whereBody <- "beforeBegin"
        }
        selectorBody <- paste0(prefix, "Body", n)
        selectorTitle <- paste0(prefix, "Title", n)
        
        if (!is.null(input[[paste0("survivalDistribution", n)]])){
          if (input[[paste0("survivalDistribution", n)]] == "Weibull"){
            removeUI(paste0(selectorBody, " .new"))
            removeUI(paste0(selectorTitle, " .new"))
            insertUI(paste0(selectorBody, suffix), ui =
                       isolate({
                         column(2, class = "new", numericInput(paste0("survivalK", n), NULL, ifelse(!is.null(input[[paste0("survivalK", n)]]), input[[paste0("survivalK", n)]],""))
                         )
                       }),
                     where = whereBody
            )
            insertUI(selectorTitle, ui = 
                       column(2, class = "new", strong("k")
                       )
            )
          } else {
            removeUI(paste0(selectorBody, " .new"))
            removeUI(paste0(selectorTitle, " .new"))
          }
        }
        
      })
        tableBody <- tagList(
                              column(2, textInput(paste0("survivalName", n), NULL, ifelse(!is.null(input[[paste0("survivalName", n)]]), input[[paste0("survivalName", n)]], ""))),
                              column(2, selectInput(paste0("survivalDistribution", n), NULL, choices = c("Exponential", "Weibull"), selected = ifelse (!is.null(input[[paste0("survivalDistribution", n)]]), input[[paste0("survivalDistribution", n)]], ""))),
                              column(2, numericInput(paste0("survivalLambda", n), NULL, ifelse(!is.null(input[[paste0("survivalLambda", n)]]), input[[paste0("survivalLambda", n)]], "")))
        )
        tableTitle <- tagList(
          column(2, strong("Name")), 
          column(2, strong("Distribution")), 
          column(2, strong("Lambda"))
        )
  } else if (module == "timedep"){
    if (!sapply(observe_show_module_timedep[n + 1], is.null)){
      observe_show_module_timedep[[n + 1]]$destroy()
    }
    observe_show_module_timedep[[n + 1]] <<- observe({
        if (edit){
          prefix <- "#editingTimedep"
          suffix <- ""
          whereBody <- "beforeEnd"
        }
        else {
          prefix <- "#timedep"
          suffix <- " div:last"
          whereBody <- "beforeBegin"
        }
        selectorBody <- paste0(prefix, "Body", n)
        selectorTitle <- paste0(prefix, "Title", n)
        
        if (!is.null(input[[paste0("timedepType", n)]])){
          if (input[[paste0("timedepType", n)]] == "nonConstant"){
            removeUI(paste0(selectorBody, " .C"))
            removeUI(paste0(selectorTitle, " .C"))
            removeUI(paste0(selectorBody, " .NC"), multiple = TRUE)
            removeUI(paste0(selectorTitle, " .NC"))
            insertUI(paste0(selectorBody, suffix), ui =
                       isolate({
                       tagList(
                         column(id = paste0("timedepNC",n), 6, class = "NC",
                         fluidRow(
                           column(4, textInput(paste0("timedepValueNC", n, 0), NULL,  ifelse(!is.null(input[[paste0("timedepValueNC", n, 0)]]), input[[paste0("timedepValueNC", n, 0)]], ""))),
                           column(4, numericInput(paste0("timedepStart", n, 0), NULL, ifelse(!is.null(input[[paste0("timedepStart", n, 0)]]), input[[paste0("timedepStart", n, 0)]],""))),
                           column(4, numericInput(paste0("timedepEnd", n, 0), NULL, ifelse(!is.null(input[[paste0("timedepEnd", n, 0)]]), input[[paste0("timedepEnd", n, 0)]], "")))
                         )
                         
                         ),
                         column(1, class = "NC", actionButton(paste0("timedepNew", n), icon("plus")))

                       )
                       })
                        , where = whereBody
            )
                     
            insertUI(selectorTitle, ui =
                       column(6, class = "NC",
                              fluidRow(
                       column(4, strong("Value")),
                       column(4, strong("Cycle Start")),
                       column(4, strong("Cycle End"))
                       )
                       )
            )
            localValues$loaded <- TRUE
          } else {
            removeUI(paste0(selectorBody, " .C"))
            removeUI(paste0(selectorTitle, " .C"))
            removeUI(paste0(selectorBody, " .NC"), multiple = TRUE)
            removeUI(paste0(selectorTitle, " .NC"))
            insertUI(paste0(selectorBody, suffix), ui =
                       isolate({
                                  column(2, class = "C",
                                         textInput(paste0("timedepValueC", n), NULL,  ifelse(!is.null(input[[paste0("timedepValueC", n)]]), input[[paste0("timedepValueC", n)]],"")))
                       }),
                     where = whereBody
            )
            insertUI(selectorTitle, ui =
                                column(2, class = "C", strong("Value"))
            )
          }
        }

      })
    tableBody <- tagList(
      column(2, textInput(paste0("timedepName", n), NULL, ifelse(!is.null(input[[paste0("timedepName", n)]]), input[[paste0("timedepName", n)]], ""))),
      column(2, selectInput(paste0("timedepType", n), NULL, choices = c("Constant variation with the number of cycles" = "constant", "Non-constant variation with the number of cycles" = "nonConstant"), selected = ifelse(!is.null(input[[paste0("timedepType", n)]]), input[[paste0("timedepType", n)]], character(0))))
    )
    tableTitle <- tagList(
      column(2, strong("Name")),
      column(2, strong("Type of time-dependent variable"))
    )
  }
  if (edit){
      wellPanel(id = paste0("editing", upFirst(module)),
                fluidRow(id = paste0("editing", upFirst(module), "Title", n), class = "row-eq-height",
                  tableTitle
                ),
                fluidRow(id = paste0("editing", upFirst(module), "Body", n), class = "row-eq-height",
                  tableBody
                ),
                actionButton(paste0(module, "OK"), "OK")
      )
  } else {
      tagList(
        if (n == 0){
          h4(id = paste0(module, "H4"), names(which(MODULES == module)))
        },
        if (n == 0 | !module %in% c("equation", "rgho"))
        fluidRow(id = paste0(module, "Title", n), class = "row-eq-height",
          tableTitle
        ),
        fluidRow(id = paste0(module, "Body", n), class = "row-eq-height",
          tableBody, 
          column(1, actionLink(paste0(module,"Delete", n), icon("trash-o", "fa-2x")))
        )
      )
    }
}


insert_module_line <-function(module, input, values, localValues){
  n <- values[[paste0("n", upFirst(module))]]
  req(n > 0)
  lapply(seq_len(n)-1, function(i){
    removeUI(paste0("#", module, "H4"), multiple=TRUE)
    removeUI(paste0("#", module, "Body",i), multiple=TRUE)
    removeUI(paste0("#", module, "Title",i), multiple=TRUE)
  })
  
  lapply(seq_len(n)-1, function(i){
    insertUI("#allModules", ui=
               show_module(module, FALSE, i, input, values, localValues)
    )

  })
}


insertTimedepNC <- function(input, values){
  nTimedep <- values$nTimedep - 1
  isolate({
  if (nTimedep >= 0){
    if (!is.null(values[[paste0("nTimedepNC", nTimedep)]])){
      lapply(seq_len(values[[paste0("nTimedepNC", nTimedep)]]), function(i){
        removeUI(paste0("#timedepValueNC", nTimedep, i), multiple=TRUE)
        removeUI(paste0("#timedepStart", nTimedep, i), multiple=TRUE)
        removeUI(paste0("#timedepEnd", nTimedep, i), multiple=TRUE)
      })
      lapply(0:nTimedep, function(n){
        lapply(seq_len(values[[paste0("nTimedepNC", n)]]), function(i){
          insertUI(selector = paste0("#newParam", n), ui =
                     fluidRow(
                       column(4, textInput(paste0("timedepValueNC", n, i), NULL, ifelse (!is.null(input[[paste0("timedepValueNC", n, i)]]), input[[paste0("timedepValueNC", n, i)]], ""))),
                       column(4, numericInput(paste0("timedepStart", n, i), NULL, ifelse (!is.null(input[[paste0("timedepStart", n, i)]]), input[[paste0("timedepStart", n, i)]], ""))),
                       column(4, numericInput(paste0("timedepEnd", n, i), NULL, ifelse (!is.null(input[[paste0("timedepEnd", n, i)]]), input[[paste0("timedepEnd", n, i)]], "")))
                     )
          )
      })

      })
        
    }
  }
  })
}


shinyServer(function(input, output, session) {
  values <- reactiveValues(nGlobalParameters = 1, nEquation = 0, nRgho = 0, nSurvival = 0, nTimedep = 0)
  loadedValues <- reactiveValues(loaded = 0, SP1 = 0, SP2 = 0, TM1 = 0, TM2 = 0, GP = 0, DSA = 0, nameStates = 0, nameStateVariables = 0, nameStrategies = 0)
  localValues <- reactiveValues(loaded = FALSE)


  onBookmark(function(state) {
    nameValues <- names(reactiveValuesToList(values))
    sapply(nameValues, function(x){
      state$values[[x]] <- values[[x]]
    })
  })
  

  # Restore extra values from state$values when we restore
  onRestore(function(state) {
    nameValues <- ls(state$values)
    sapply(nameValues, function(x){
      values[[x]] <- state$values[[x]]
    })
  })
  
  setBookmarkExclude(
    c(
      paste0(MODULES, "OK"),
      "newParam",
      unname(MODULES)
    )
  )
  
  
  #observe_nTimedep <- NULL
  observe_timedepNew <- list()
  
  # observe({
  #   values$nTimedep
  #   isolate({
  #   if(!is.null(observe_nTimedep)){
  #     observe_nTimedep$destroy()
  #   }

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
  #})
  #})
  
  
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
  
  showStateParam <- function(nbStrat, input, values, click) {
    nbStates <- input$nbStates
    nbStateVariables <- input$nbStateVariables
    
    req(nbStates)
    req(nbStateVariables)
    
    stateName <- ""
    variableStateName <- ""
    
    for (i in seq_len(nbStates)) {
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    
    for (i in seq_len(nbStateVariables)) {
      variableStateName[i] <- input[[paste0("variableStateName", i)]]
    }
    
    if (input$nbStates > 0) {
      start <- ifelse(nbStrat > 1, 2, 1)
      
      lapply(
        seq(from = start, to = nbStrat),
        function(x) {
          tagList(
            h3(paste("State Parameters for", input[[paste0("strategyName",x)]])),
            tags$table(
              class='stateVariables',
              tagList(
                tags$th(),
                lapply(
                  seq_len(nbStates),
                  function(i) {
                    tags$th(style='text-align:center', stateName[i])
                  }),
                tags$th(style='text-align:center', "Discounting Rate"),
                lapply(
                  seq_len(nbStateVariables),
                  function(i) {
                    tags$tr(
                      tags$td(variableStateName[i]),
                      lapply(
                        seq_len(nbStates),
                        function (j) {
                          isolate({
                            tags$td(textInput(
                              paste0("stateVariable",x,i,j),
                              value = ifelse(click == TRUE, 
                                             ifelse(!is.null(input[[paste0("stateVariable",1,i,j)]]), input[[paste0("stateVariable",1,i,j)]], 0),
                                             ifelse (!is.null(input[[paste0("stateVariable",x,i,j)]]), input[[paste0("stateVariable",x,i,j)]], 
                                                     ifelse(!is.null(input[[paste0("stateVariable",1,i,j)]]),input[[paste0("stateVariable",1,i,j)]],0)
                                             )),
                              label = NULL,
                              width="100%"))
                          })
                        }),
                      isolate({
                        tags$td(numericInput(
                          paste0("discountingRate",x,i),
                          label = NULL,
                          step=1,
                          value = ifelse(click == TRUE, 
                                         ifelse(!is.null(input[[paste0("discountingRate",1,i)]]), input[[paste0("discountingRate",1,i)]], 0),
                                         ifelse(!is.null(input[[paste0("discountingRate",x,i)]]), input[[paste0("discountingRate",x,i)]], 
                                                ifelse(!is.null(input[[paste0("discountingRate",1,i)]]), input[[paste0("discountingRate",1,i)]],0)
                                         )),
                          width="100%"))
                      })
                    )
                  })
              )
            )
          )
        })
    }
  }
  
  showTransMatrix <- function(nbStrat, input, values, click) {
    nbStates <- input$nbStates
    
    req(nbStates)
    
    stateName <- ""
    start <- ifelse(nbStrat > 1, 2, 1)
    
    for (i in seq_len(nbStates)) {
      stateName[i] <- input[[paste0("stateName", i)]]
    }
    
    if (input$nbStates > 0) {
      tagList(
        lapply(
          seq(from = start, to = nbStrat),
          function(x) {
            tagList(
              h3(paste("Transition Matrix for", input[[paste0("strategyName",x)]])),
              tags$div(
                renderPlot({
                  tm <- ux_matrix(input, x)
                  if (is.null(tm)) {
                    plot.new()
                    text(.5, .5, "Incorrect\ninput")
                  } else {
                    plot(tm)
                  }
                },
                width = 200,
                height = 200
                ), style="text-align: center"
              ),
              tags$table(
                style="margin:0 auto;",
                class='transmatrix',
                tagList(
                  tags$th(),
                  lapply(
                    seq_len(nbStates),
                    function(i) {
                      tags$th(style='text-align:center', stateName[i])
                    }),
                  lapply(
                    seq_len(nbStates),
                    function(i) {
                      tags$tr(
                        tags$td(stateName[i]),
                        lapply(
                          seq_len(nbStates),
                          function(j) {
                            isolate(
                              tags$td(textInput(
                                paste0("transmatrix",x,i,j),
                                value = ifelse(click == TRUE, 
                                               ifelse(!is.null(input[[paste0("transmatrix",1,i,j)]]), input[[paste0("transmatrix",1,i,j)]], "0"),
                                               ifelse(!is.null(input[[paste0("transmatrix",x,i,j)]]),input[[paste0("transmatrix",x,i,j)]],
                                                      ifelse(!is.null(input[[paste0("transmatrix",1,i,j)]]), input[[paste0("transmatrix",1,i,j)]], "0")
                                               )),
                                label=NULL,
                                width="100%")))
                          })
                      )
                    })
                )
              ),
              hr()
            )
          })
      )
    }
  }
  
  showGlobalParameters <- function(input, values) {
    n <- values$nGlobalParameters
    a <- tags$table(
      tags$tr(
        tags$th(style='text-align:center', "Variable name"),
        tags$th(style='text-align:center', "Value")
      ),
      lapply(
        seq_len(n),
        function(i) {
          tags$tr(
            isolate(tags$td(
              textInput(
                paste0("globalParamName",i),
                label = NULL,
                value = input[[paste0("globalParamName",i)]],
                width="100%"))),
            isolate(tags$td(
              textInput(
                paste0("globalParamValue",i),
                label = NULL,
                value =  ifelse(!is.null(input[[paste0("globalParamValue", i)]]), input[[paste0("globalParamValue",i)]], ""),
                width="100%")))
          )
          
        })
    )
    
    tagList(
      a,
      actionButton("addParametersGP", "Add a new variable")
    )
  }
  
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
    loadedValues[["input"]] <- input
    loadedValues[["values"]] <- values
  })
  
  output$saveButton <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.RData')
    },
    content = function(file) {
      save(input, values, file=file)
    }
  )
  
  observeEvent(input$loadButton, {
    loadedValues$loaded <- loadedValues$loaded + 1
  })
  
  output$nameStates <- renderUI({
    req(input$nbStates)
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStates < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStates <- loadedValues$loaded
    }
    
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
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStateVariables < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStateVariables <- loadedValues$loaded
    }
    
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
    
    if(loadedValues$loaded > 0 & isolate(loadedValues$nameStrategies < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$nameStrategies <- loadedValues$loaded
    }
    
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
  
  
  show_first <- function(val, FUN, loadedValues, input){
    req(input$nbStates, input$nbStrategies)
    if (val == "SP1")
      req(input$nbStateVariables)
    for (i in 1:input$nbStates){
      req(input[[paste0("stateName", i)]])
    }
    req(input$strategyName1)
    if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
    }
    FUN(1, input, values, click = FALSE)
  }
  
  copyValues <- function(trigger, input, values, FUN) {
    a <- eventReactive(input[[trigger]],{
      FUN(input$nbStrategies, input, values, TRUE)
    })
    
    return(a())
  }
  
  show_next <- function(val, trigger, input, values, FUN, loadedValues){
    req(input$nbStates, input$nbStrategies > 1)
    if (val == "SP2")
      req(input$nbStateVariables)
    
    for (i in 1:input$nbStates){
      req(input[[paste0("stateName", i)]])
    }
    for (i in 1:input$nbStrategies){
      req(input[[paste0("strategyName", i)]])
    }
    input[[trigger]]
    if(loadedValues$loaded > 0 & isolate(loadedValues[[val]] < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues[[val]] <- loadedValues$loaded
      FUN(input$nbStrategies, input, values, click = FALSE)
    } 
    else if (input[[trigger]]){
      copyValues(trigger, input = input, values = values, FUN)
    } else {
      FUN(input$nbStrategies, input, values, click = FALSE)
    }
  }
  
  output$transMatrix1 <- renderUI({
    show_first(val = "TM1", FUN = showTransMatrix, loadedValues, input)    
  })
  
  output$transMatrix2 <- renderUI({
    show_next(val = "TM2", trigger = "copyValuesParametersTM", input, values, showTransMatrix, loadedValues)
    
  })  
  
  output$stateParameters1 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_first(val = "SP1", FUN = showStateParam, loadedValues = loadedValues, input)
  })
  
  output$stateParameters2 <- renderUI({
    req(input[[paste0("variableStateName", input$nbStateVariables)]])
    show_next(val = "SP2", trigger = "copyValuesParametersSP", input, values, showStateParam, loadedValues)
  })
  
  
  
  output$costVariable <- renderUI({
    textInput(
      "costVariable",
      label = "Cost Variable",
      value = ifelse(loadedValues$loaded == 0, input$variableStateName1, loadedValues$input$costVariable)
    )
  })
  output$effectVariable <- renderUI({
    textInput(
      "effectVariable",
      label = "Effect Variable",
      value = ifelse(loadedValues$loaded == 0, input$variableStateName2, loadedValues$input$effectVariable)
    )
  })
  

  
  observe({
    if (loadedValues$loaded > 0){
      values$nGlobalParameters <- loadedValues$values$nGlobalParameters
    }
  })
  
  observeEvent(input$addParametersGP, {
    isolate(values$nGlobalParameters <- values$nGlobalParameters + 1)
  })
  
  output$globalParameters <- renderUI({
    req(input$nbStrategies)
    values$nGlobalParameters
    if(loadedValues$loaded > 0 & isolate(loadedValues$GP < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$GP <- loadedValues$loaded
    }
    showGlobalParameters(input, values)
  })
  
  #   observeEvent(input$addDeterministic,{
  #     isolate(values$nbDeterministic <- values$nbDeterministic + 1)
  #   })
  
  output$DSA <- renderUI({
    req(input$nbStates, input$nbStrategies)
    if(loadedValues$loaded > 0 & isolate(loadedValues$DSA < loadedValues$loaded)){
      input <- loadedValues$input
      values <- loadedValues$values
      loadedValues$DSA <- loadedValues$loaded
    }
    dsa <- lapply(seq_len(values$nGlobalParameters), function(i){
      isolate({
        tags$tr(
          tags$td(disabled(textInput(paste0("recGlobalParamName", i), NULL, ifelse(!is.null(input[[paste0("globalParamName", i)]]), input[[paste0("globalParamName", i)]], ""), width="100%"))),
          tags$td(numericInput(paste0("minValue", i), NULL, ifelse(!is.null(input[[paste0("minValue", i)]]), input[[paste0("minValue", i)]], ""), width="100%")),
          tags$td(numericInput(paste0("maxValue", i), NULL, ifelse(!is.null(input[[paste0("maxValue", i)]]), input[[paste0("maxValue", i)]], ""), width="100%"))
        )
      })
    })
    
    tagList(
      tags$table(style="margin:0 auto;", tags$th("Variable name", style='text-align:center'), tags$th("Minimum value", style='text-align:center'), tags$th("Maximum value", style='text-align:center'), dsa),
      fluidRow(column(3, offset = 5, actionButton("addDeterministic", "Add a deterministic value")))
    )
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
                        value = ifelse (loadedValues$loaded == 0, 1000, ifelse(!is.null(loadedValues$input[[paste0("init",i)]]), loadedValues$input[[paste0("init",i)]], 1000)),
                        width="100%"
                      )
                    } else {
                      numericInput(
                        paste0("init", i),
                        label = NULL,
                        value = ifelse (loadedValues$loaded == 0, 0, ifelse(!is.null(loadedValues$input[[paste0("init",i)]]), loadedValues$input[[paste0("init",i)]], 0)),
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
                actionLink(MODULES[i], names(MODULES)[i])
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

