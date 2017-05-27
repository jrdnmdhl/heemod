#' Check Markov Model Transition Matrix
#' 
#' Check whether a matrix fullfills the conditions to be a 
#' transition matrix.
#' 
#' This function is called by [eval_transition()]
#' and should not be used directly.
#' 
#' Checks whether all rows sum to 1 and all probabilities 
#' are between 0 and 1.
#' 
#' @param x a matrix.
#'   
#' @return `NULL`
#'   
#' @keywords internal
check_matrix <- function(x) {
  stopifnot(inherits(x, "array"))
  stopifnot(length(dim(x)) == 3)
  
  if (! isTRUE(all.equal(
    range(rowSums(x, dims = 2)),
    c(1, 1)))) {
    problem_rows <- which(rowSums(x, dims = 2) != 1, arr.ind = TRUE)
    problem_rows <- data.frame(
      cycle = problem_rows[,1], 
                               state = get_state_names(x)[problem_rows[,2]])
    problem_rows <- format.data.frame(problem_rows, justify = "left")
    
    stop(sprintf(
      "Not all transition matrix rows sum to 1:\n%s",
      paste(sprintf(
        "cycle: %s, state: %s",
        problem_rows[,1],
        problem_rows[,2]),
        collapse = "\n")
    ))
    
    
  }
  
  if (! all(x >= 0 & x <= 1)) {
    problem <- which(x < 0 | x > 1, arr.ind = TRUE)
    problem <- data.frame(problem)
    names(problem) <- c("cycle", "from", "to")
    states <- get_state_names(x)
    problem$from <- states[problem$from]
    problem$to <- states[problem$to]
    problem <- format.data.frame(problem, justify = "left")
    
    stop(sprintf(
      "Some transition probabilities are outside the interval [0 - 1]:\n%s",
      paste(sprintf(
        "cycle: %s, from: %s, to: %s",
        problem$cycle, problem$from, problem$to),
        collapse = "\n")
    ))
    
  }
}

#' Evaluate Markov Model Transition Matrix
#' 
#' Evaluate a transition matrix using evaluated parameters.
#' 
#' Runs checks on the transition matrix during evaluation.
#' 
#' This functions has been heavily optimized, and thus can
#' be difficult to read. Good luck...
#' 
#' @param x an `uneval_matrix` object.
#' @param parameters an `eval_parameters` object.
#'   
#' @return An `eval_matrix` object (actually a list of 
#'   transition matrices, one per cycle).
#'   
#' @keywords internal
eval_transition <- function(x, ...) {
  UseMethod("eval_transition")
}

eval_transition.uneval_matrix <- function(x, parameters, expand) {
  
  # update calls to dispatch_strategy()
  x <- dispatch_strategy_hack(x)
  
  # Set up time values for which transition probabilities
  # will be evaluated
  time_values <- list(
    state_time = parameters$state_time,
    model_time = parameters$model_time
  )
  
  # Replace complement with negative pi
  parameters$C <- -pi
  
  # Get number of states + state names
  n_state <- sqrt(length(x))
  state_names <- attr(x, "state_names")
  
  # Start an empty df to which transitions will
  # be appended
  trans_table <- tibble::tibble()
  
  # Loop through each cell of unexpanded transition matrix and
  # fill out long-form transition table
  for(from in seq_len(n_state)) {
    for(to in seq_len(n_state)) {
      trans_values <- time_values
      trans_values$.from <- state_names[from]
      trans_values$.to <- state_names[to]
      trans_values$.value <- lazyeval::lazy_eval(
        x[[(from - 1) * n_state + to]],
        data = parameters
      )
      trans_table <- rbind(
        trans_table,
        do.call(tibble::tibble, trans_values)
      )
    }
  }
  
  # Join transitions w/ expansion table so that to/from states
  # requiring expansion are identified
  trans_table <- trans_table %>%
    dplyr::left_join(
      expand %>% dplyr::transmute(state=state, .expand_from = expand, limit = limit),
      by = c(".from" = "state")
    ) %>%
    dplyr::left_join(
      expand %>% dplyr::transmute(state=state, .expand_to = expand),
      by = c(".to" = "state")
    )
  
  # Handle transitions for expanded states
  trans_table <- trans_table %>%
    dplyr::filter(state_time <= limit) %>%
    dplyr::group_by(.from, .to, model_time) %>%
    dplyr::mutate(
      .from_expanded = ifelse(
        .expand_from,
        paste0(".", .from, "_", state_time),
        .from
      ),
      .to_expanded = ifelse(
        .expand_to & .from == .to,
        paste0(".", .to, "_", pmin(max(state_time), state_time + 1)),
        ifelse(
          .expand_to,
          paste0(".", .to, "_", 1),
          .to
        )
      )
    )
  
  e_state_names <- unique(trans_table$.from_expanded)
  
  # Reshape into 3d matrix and calculate complements
  trans_matrix <- trans_table %>%
    reshape2::acast(
      model_time ~
        factor(.from_expanded, levels = e_state_names) ~
        factor(.to_expanded, levels = e_state_names),
      value.var = ".value",
      fill = 0
    ) %>%
    replace_C
  
  array_res <- structure(
    trans_matrix,
    state_names = e_state_names
  )
  
  check_matrix(array_res)
  
  structure(
    split_along_dim(array_res, 1),
    class = c("eval_matrix", "list"),
    state_names = get_state_names(x)
  )
}

split_along_dim <- function(a, n) {
  # could be maybe optimized?
  setNames(lapply(
    split(a, arrayInd(seq_along(a), dim(a))[, n]),
    array, dim = dim(a)[-n], dimnames(a)[-n]),
    dimnames(a)[[n]])
}

replace_C <- function(x) {
  posC <- x == -pi
  
  if (! all(rowSums(posC, dims = 2) <= 1)) {
    stop("Only one 'C' is allowed per matrix row.")
  }
  
  x[posC] <- 0
  
  valC <- 1 - rowSums(x, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  x[posC] <- valC
  x
}

get_state_names.eval_matrix <- function(x, ...){
  attr(x, "state_names")
}
get_state_names.array <- function(x, ...){
  attr(x, "state_names")
}

get_matrix_order.eval_matrix <- function(x){
  ncol(x[[1]])
}
