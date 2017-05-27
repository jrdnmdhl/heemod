#' Evaluate Markov model parameters
#' 
#' Evaluate parameters specified through 
#' `define_parameters`, for a given number of cycles.
#' 
#' @param x an `uneval_parameters` object.
#' @param cycles integer. Number of cycles to simulate.
#'   
#' @return An object of class `eval_parameters` 
#'   (actually a data.frame with one column per parameter 
#'   and one row per cycle).
#'   
#' @example inst/examples/example_eval_parameters.R
#'   
#' @keywords internal
eval_parameters <- function(x, cycles = 1,
                            strategy_name = NA, max_state_time = cycles) {
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  # Get number of paramters
  n_par <- length(x)
  
  # Get parameter names
  par_names <- names(x)
  
  # Extract parent environment
  par_env <- x[[1]]$env
  
  # Set up the results list
  res <- list(
    model_time = rep(seq_len(cycles), max_state_time),
    markov_cycle = rep(seq_len(cycles), max_state_time),
    state_time = rep(seq_len(max_state_time), each=cycles),
    strategy = strategy_name
  )
  n_row = nrow(res)
  
  # Evalute each parameter and add it to the list
  for(i in seq_len(n_par)) {
    res[[par_names[i]]] <- lazyeval::lazy_eval(x[[i]], data = res)
  }
  
  # Return evaluated parameters object
  structure(
    res,
    original = x,
    class = c("eval_parameters", class(res))
  )
}

eval_init <- function(x, parameters, expand) {
  
  # Get number of states and state names
  n_state <- nrow(expand)
  state_names <- names(x)
  
  # Start a blank df
  inits_df <- tibble::tibble()
  
  # Evaluate initial values
  for(i in seq_len(n_state)) {
    inits_df <- rbind(
      inits_df,
      tibble::tibble(
        .name = state_names[i],
        model_time = parameters$model_time,
        state_time = parameters$state_time,
        .value = lazyeval::lazy_eval(x[[i]],data = parameters)
      )
    )
  }
  
  # Expand and Transpose
  inits_df <- inits_df %>%
    left_join(expand, by = c(".name" = "state")) %>%
    dplyr::filter(state_time <= limit, model_time == min(model_time)) %>%
    dplyr::mutate(
      .full_name = ifelse(
        expand,
        paste0(".", .name, "_", state_time),
        .name
      ),
      .value = ifelse(state_time == 1, .value, 0)
    ) %>%
    reshape2::acast(1~factor(.full_name, levels=unique(.full_name)), value.var = ".value")
}

eval_inflow <- function(x, parameters, expand) {
  
  # Get number of states and state names
  n_state <- nrow(expand)
  state_names <- names(x)
  
  # Start a blank df
  inflow_df <- tibble::tibble()
  
  # Evaluate initial values
  for(i in seq_len(n_state)) {
    inflow_df <- rbind(
      inflow_df,
      tibble::tibble(
        .name = state_names[i],
        model_time = parameters$model_time,
        state_time = parameters$state_time,
        .value = lazyeval::lazy_eval(x[[i]],data = parameters)
      )
    )
  }
  
  # Expand and Transpose
  inflow_df <- inflow_df %>%
    left_join(expand, by = c(".name" = "state")) %>%
    dplyr::filter(state_time <= limit) %>%
    dplyr::mutate(
      .full_name = ifelse(
        expand,
        paste0(".", .name, "_", state_time),
        .name
      ),
      .value = ifelse(state_time == 1, .value, 0)
    ) %>%
    reshape2::acast(model_time~factor(.full_name, levels=unique(.full_name)), value.var = ".value")
    
}
