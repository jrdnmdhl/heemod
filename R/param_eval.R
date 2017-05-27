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

eval_init <- function(x, parameters) {
  to_keep <- names(x)
  init_df <- tibble::tibble(.rowid=1)
  if (length(to_keep)) {
    with(parameters, dplyr::mutate_(init_df, .dots = x))[to_keep]
  } else {
    tibble::tibble()
  }
}

eval_starting_values <- eval_inflow <- eval_init
