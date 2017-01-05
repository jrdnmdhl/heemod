show_PSA_div <- function(input, values, choices, n){
  if (!is.null(input[[paste0("PSADistrib", n)]])){
    psa_param1 <- switch (input[[paste0("PSADistrib", n)]],
                          "Normal" = "Mean",
                          "Lognormal" = "Mean",
                          "Binomial" = "Prop",
                          "Gamma" = "Mean",
                          "Logitnormal" = "Mu",
                          "Multinomial" = "Nb parameters"
                          ) 
    psa_param2 <- switch (input[[paste0("PSADistrib", n)]],
                          "Normal" = "SD",
                          "Lognormal" = "SD",
                          "Binomial" = "Size",
                          "Gamma" = "SD",
                          "Logitnormal" = "Sigma"
    ) 
  }
  fluidRow(id = paste0("PSA_div", n),
      column(2, selectizeInput(paste0("PSAGlobalParamName", n), "Variable name" , choices = choices, selected = ifelse(!is.null(input[[paste0("PSAGlobalParamName", n)]]), input[[paste0("PSAGlobalParamName", n)]], ""))),
      column(2, selectizeInput(paste0("PSADistrib", n), "Distribution", choices = c("Normal", "Lognormal", "Binomial", "Gamma", "Logitnormal", "Multinomial"), selected = ifelse(!is.null(input[[paste0("PSADistrib", n)]]), input[[paste0("PSADistrib", n)]], character(0)))),
      if (!is.null(input[[paste0("PSADistrib", n)]]) && length(input[[paste0("PSADistrib", n)]]) > 0){
        isolate(
        tagList(
          column(2, numericInput(paste0("PSAParam1", n), psa_param1, ifelse(!is.null(input[[paste0("PSAParam1", n)]]), input[[paste0("PSAParam1", n)]], ""))),
          if (input[[paste0("PSADistrib", n)]] != "Multinomial"){
            column(2, numericInput(paste0("PSAParam2", n), psa_param2, ifelse(!is.null(input[[paste0("PSAParam2", n)]]), input[[paste0("PSAParam2", n)]], "")))
          } else column(6, uiOutput("addMultinomial"))
        )
        )
      },
      if (!is.null(input[[paste0("PSADistrib", n)]]) && input[[paste0("PSADistrib", n)]] == "Lognormal"){
        column(4, style = "margin-top:20px", checkboxInput(paste0("PSALogscale", n), "Check if mean and sd are on the log scale", value = ifelse(!is.null(input[[paste0("PSALogscale", n)]]), input[[paste0("PSALogscale", n)]], FALSE)))
      }
  )
}

show_DSA_div <- function(input, values, choices, n){
  var_name <- if (n == 0) "Variable name" else NULL
  max_val <- if (n == 0) "Minimum value" else NULL
  min_val <- if (n == 0) "Maximum value" else NULL
  isolate({
    print(choices)
    div(id = paste0("DSA_div", n), class="centerdiv",
        selectizeInput(paste0("DSAGlobalParamName", n), var_name, choices = choices, selected = ifelse(!is.null(input[[paste0("DSAGlobalParamName", n)]]), input[[paste0("DSAGlobalParamName", n)]], "")),
        numericInput(paste0("minDSAValue", n), max_val, ifelse(!is.null(input[[paste0("minDSAValue", n)]]), input[[paste0("minDSAValue", n)]], "")),
        numericInput(paste0("maxDSAValue", n), min_val, ifelse(!is.null(input[[paste0("maxDSAValue", n)]]), input[[paste0("maxDSAValue", n)]], ""))
    )
  })
}


get_names_SA <- function(input, values){
  equation <- purrr::map(seq_len(values$nEquation)-1, function(i){
    input[[paste0("equationName", i)]]
  }) 
  rgho <- purrr::map(seq_len(values$nRgho)-1, function(i){
    input[[paste0("rghoName", i)]]
  }) 
  survival <- purrr::map(seq_len(values$nSurvival)-1, function(i){
    c(
      if(!is.null(input[[paste0("survivalName", i)]])) paste(input[[paste0("survivalName", i)]], "lambda"),
      if (!is.null(input[[paste0("survivalDistribution", i)]]) && input[[paste0("survivalDistribution", i)]] == "Weibull") paste(input[[paste0("survivalName", i)]], "k")
    )
  })
  timedep <- purrr::map(0:(values$nTimedep-1), function(i){
    if (!is.null(input[[paste0("timedepType", i)]])){
      if (input[[paste0("timedepType", i)]] == "constant") {
        input[[paste0("timedepName", i)]]
      }
      else {
        if(!is.null(values[[paste0("nTimedepNC", i)]])){
          map::map(0:values[[paste0("nTimedepNC", i)]], function(j){
            sprintf("%s (%s-%s)", input[[paste0("timedepName", i)]], input[[paste0("timedepStart", i, j)]], input[[paste0("timedepEnd", i, j)]])
          }) %>% compact %>% flatten_chr
        }
      }
    }
      
  })
  return(
    c(equation, rgho, survival, timedep) %>% 
      purrr::compact() %>%
      purrr::flatten_chr() %>% 
      sort()
    )
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
  countryCodes <- filter_gho(
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
    selected = isolate({ifelse(!is.null(input[[paste0("rghoCountry", n)]]), input[[paste0("rghoCountry", n)]], "GLOBAL")})
  )
}

show_module <- function(module, edit, n, table_title, table_body)  {
  if (edit){
    wellPanel(id = paste0("editing", Hmisc::upFirst(module)),
              fluidRow(id = paste0("editing", Hmisc::upFirst(module), "Title", n), class = "row-eq-height",
                       table_title
              ),
              fluidRow(id = paste0("editing", Hmisc::upFirst(module), "Body", n), class = "row-eq-height",
                       table_body
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
                 table_title
        ),
      fluidRow(id = paste0(module, "Body", n), class = "row-eq-height",
               table_body#, 
               #actionLink(paste0(module,"Delete", n), icon("trash-o", "fa-2x"))
      )
    )
  }
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
