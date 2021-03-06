context("Partitioned Survival Model")

surv_dist1 <- define_survival(
  distribution = "exp",
  rate = .003
)
surv_dist2 <- define_survival(
  distribution = "exp",
  rate = .002
)
surv_dist3 <- define_survival(
  distribution = "exp",
  rate = .005
)
surv_dist4 <- define_survival(
  distribution = "exp",
  rate = .004
)


suppressMessages(
  ps <- define_part_surv(
    pfs = join(surv_dist3, surv_dist4, at=365),
    os = join(surv_dist1, surv_dist2, at=365),
    cycle_length = c(365, 365)
  )
)
suppressMessages(
  ps1 <- define_part_surv(
    pfs = join(surv_dist3, surv_dist4, at=365) %>% apply_hr(0.8),
    os = join(surv_dist1, surv_dist2, at=365) %>% apply_hr(0.8),
    cycle_length = c(365, 365)
  )
)

sA <-  define_state(
  cost = 10000, ut = 1
)
sB <-  define_state(
  cost = 20000, ut = 1
)
sC <-  define_state(
  cost = 0, ut = 0
)

stratPS <- define_strategy(
  transition = ps,
  A = sA, B = sB, C = sC
)

stratPS1 <- define_strategy(
  transition = ps1,
  A = sA, B = sB, C = sC
)

suppressMessages(
  resPS <- run_model(
  Strat1 = stratPS,
  Strat2 = stratPS1,
  cycles = 10,
  cost = cost,
  effect = ut,
  method = "end"
))

test_that(
  "part surv works", {
    
    surv_dist_1 <- define_survival(
      distribution = "exp", rate = 0.5)
    fit_cov <- flexsurv::flexsurvreg(
      formula = survival::Surv(rectime, censrec) ~ group,
      dist = "weibull", 
      data = flexsurv::bc)
    fitcov_medium <- set_covariates(fit_cov, group = "Medium")
    km_cov <- survival::survfit(
      formula = survival::Surv(rectime, censrec) ~ group,
      data = flexsurv::bc)
    km_medium <- set_covariates(km_cov, group = "Medium")
    
    suppressMessages({
      ps <- define_part_surv(
        pfs = surv_dist_1,
        os = km_medium %>%
          join(fitcov_medium, 
                  at = 730),
        cycle_length = c(1, 365)  # 1 for pfs, 365 for os
      )})
    
    sA <- define_state(cost = 10, ut = 1)
    sB <- define_state(cost = 20, ut = 0.5)
    sC <- define_state(cost = 0, ut = 0)
    
    stratPS <- define_strategy(transition = ps, A = sA, B = sB, C = sC)
    
    param <- define_parameters(
      p1 = compute_surv(
        surv_dist_1,
        time = model_time  # can also be state_time
      ),
      p2 = compute_surv(
        km_medium %>%
          join(fitcov_medium, 
                  at = 730),
        time = model_time, cycle_length = 365  # time is in days in km_medium, in years in model_time
      ))
    
    tm <- define_transition(
      1 - p1, C, p2,
      0, C, p2,
      0, 0, 1)
    #> No named state -> generating names.
    
    stratTM <- define_strategy(
      transition = tm, A = sA, B = sB, C = sC)
    
    suppressWarnings({
      resPS <- run_model(
        stratPS,
        cycles = 10)
    })
    suppressWarnings({
      resTM <- run_model(
        parameters = param,
        stratTM,
        cycles = 10)
    })
    
    expect_equal(
      get_counts(resPS)$count,
      get_counts(resTM)$count
    )
    
    suppressWarnings({
      resPS <- run_model(
        stratPS,
        cycles = 10,
        method = "end")
    })
    suppressWarnings({
      resTM <- run_model(
        parameters = param,
        stratTM,
        cycles = 10,
        method = "end")
    })
    
    expect_equal(
      get_counts(resPS)$count,
      get_counts(resTM)$count
    )
  }
)

suppressMessages({
  ps <- define_part_surv(
    pfs = join(surv_dist3),
    os = join(surv_dist1),
    state_names = c("ProgressionFree", "Progressive", "Death"),
    cycle_length = c(365, 365)
  )
})
test_that(
  "errors with inappropriate state names", {
    expect_error(
      define_part_surv(
        pfs = join(surv_dist3),
        os = join(surv_dist1),
        state_names = c("NoDisease", "Progressive", "Death"),
        cycle_length = c(365, 365)
      ),
      "Progression free state (only) must have 'free' in its name",
      fixed = TRUE
    )
    expect_error(
      define_part_surv(
        pfs = join(surv_dist3),
        os = join(surv_dist1),
        state_names = c("ProgressionFree", "Progressive", "Kaput"),
        cycle_length = c(365, 365)
      ),
      "State name representing death"
    )
    expect_error(
      define_part_surv(
        pfs = join(surv_dist3),
        os = join(surv_dist1),
        state_names = c("ProgressionFree", "Progressive",
                        "uh-oh", "Death"),
        cycle_length = c(365, 365)
      ),
      "If there are 4 states, a state must be called 'terminal'",
      fixed = TRUE
    )
    expect_error(
      define_part_surv(
        pfs = join(surv_dist3),
        os = join(surv_dist1),
        state_names = c(
          "ProgressionFree",
          "Progressivebutfree",
          "terminal",
          "Death"
        ),
        cycle_length = c(365, 365)
      ),
      "Progression free state (only) must have 'free' in its name",
      fixed = TRUE
    )
  })

test_that(
  "construct_part_surv_tib works", {
    
    surv_def <- read_file(system.file("tabular/surv", 
                                      "use_fits.csv", 
                                      package = "heemod"))
    surv_def$.subset <- "all"
    fake_fit_tib <- read_file(system.file("tabular/surv",
                                          "fake_fit_tib.csv", 
                                          package = "heemod"))
    state_names <- c("ProgressionFree", "ProgressiveDisease", 
                     "Terminal", "Death")
    ## basically just make sure it runs, since we're using fake fits
    zz <- construct_part_surv_tib(surv_def, fake_fit_tib, state_names)
    
    expect_identical(names(zz), c(".strategy", ".subset", "part_surv"))
    expect_identical(class(zz[[1, 3]]), c("part_surv"))
    surv_def_join <- read_file(system.file("tabular/surv", 
                                           "use_fits_with_join.csv", 
                                           package = "heemod"))
    zz <- construct_part_surv_tib(surv_def_join, fake_fit_tib, state_names)
    surv_def_join <- surv_def_join[, 1:3]
    expect_error(capture.output(construct_part_surv_tib(
      surv_def_join, 
      fake_fit_tib, 
      state_names)),
      "unless 'until' is also specified", fixed = TRUE)
    bad_surv_def <- surv_def_join
    bad_surv_def[[1, "dist"]] <- "fit('bad')"
    expect_error(capture.output(construct_part_surv_tib(
      bad_surv_def, 
      fake_fit_tib, 
      state_names)),
      "fit not found")
    names(surv_def)[1] <- "strategy"
    expect_error(construct_part_surv_tib(surv_def, fake_fit_tib, state_names),
                 "missing required names in 'surv_def':", fixed = TRUE)
    names(surv_def)[1] <- ".strategy"
    names(fake_fit_tib)[1] <- ".strategy"
    expect_error(construct_part_surv_tib(surv_def, fake_fit_tib, state_names),
                 "missing required names in 'fit_tibble':", fixed = TRUE)
  }
)

