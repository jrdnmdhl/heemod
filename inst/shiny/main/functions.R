get_names_SA <- function(input, values){
  equation <- sapply(seq_len(values$nEquation)-1, function(i){
    input[[paste0("equationName", i)]]
  })
  rgho <- sapply(seq_len(values$nRgho)-1, function(i){
    input[[paste0("rghoName", i)]]
  })
  survival <- sapply(seq_len(values$nSurvival)-1, function(i){
    c(
      paste(input[[paste0("survivalName", i)]], "lambda"),
      if (input[[paste0("survivalDistribution", i)]] == "Weibull") paste(input[[paste0("survivalName", i)]], "k")
    )
  }) %>% unlist %>% as.vector
  timedep <- sapply(seq_len(values$nTimedep)-1, function(i){
    if (input[[paste0("timedepType", i)]] == "constant") {
      input[[paste0("timedepName", i)]]
    } else {
        sapply(0:values[[paste0("nTimedepNC", i)]], function(j){
          sprintf("%s (%s-%s)", input[[paste0("timedepName", i)]], input[[paste0("timedepStart", i, j)]], input[[paste0("timedepEnd", i, j)]])
        }) %>% unlist %>% as.vector
      }
  }) %>% unlist %>% as.vector
  return(c(equation, rgho, survival, timedep))
}

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


show_first <- function(val, FUN, input){
  req(input$nbStates, input$nbStrategies)
  if (val == "SP1")
    req(input$nbStateVariables)
  for (i in 1:input$nbStates){
    req(input[[paste0("stateName", i)]])
  }
  req(input$strategyName1)
  FUN(1, input, values, click = FALSE)
}

copyValues <- function(trigger, input, values, FUN) {
  a <- eventReactive(input[[trigger]],{
    FUN(input$nbStrategies, input, values, TRUE)
  })
  
  return(a())
}

show_next <- function(val, trigger, input, values, FUN){
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
  if (input[[trigger]]){
    copyValues(trigger, input = input, values = values, FUN)
  } else {
    FUN(input$nbStrategies, input, values, click = FALSE)
  }
}
