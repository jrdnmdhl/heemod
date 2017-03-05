#' Define Partitioned Survival
#' 
#' Define a partitioned survival model with progression-free
#' survival and overall survival.
#' 
#' @param ... Numeric vectors or survival distribution corresponding
#' to the marginal distributions which partition the model states.  State names
#' are passed using argument names
#' @param absorbing_state The name of the absorbing health state
#' @param cycle_length The value of a Markov cycle in
#'   absolute time units.
#'   
#' @return A `part_surv` object.
#' @export
#' 
#' @examples
#' dist_pfs <- define_survival("exp", rate = 1)
#' dist_os <- define_survival("exp", rate = .5)
#' 
#' define_part_surv(
#'   pfs = dist_pfs,
#'   os = dist_os
#' )
#' 
define_part_surv <- function(..., absorbing_state = "dead", cycle_length = 1) {
  dots <- lazyeval::lazy_dots(...)
  define_part_surv_(
    dots,
    absorbing_state = absorbing_state,
    cycle_length = cycle_length
  )
}

#' @export
#' @rdname define_part_surv
define_part_surv_ <- function(dots, absorbing_state = "dead", cycle_length = 1) {
  
  # Handle state names, fill in any missing ones
  n_state <- length(dots)
  states = names(dots)
  ind <- states == ""
  states[ind] <- LETTERS[which(ind)]
  state_names <- LETTERS[seq_len(n_state + 1)]
  states <- c(states, absorbing_state)
  names(state_names) <- states

  if (length(cycle_length) == 1) {
    cycle_length <- rep(cycle_length, 2)
  }
  
  res <- list(
    marginals = dots,
    state_names = state_names,
    cycle_length = cycle_length
  )
  
  structure(
    res,
    class = "part_surv"
  )
}

get_state_names.part_surv <- function(x) {
  x$state_names
}

eval_transition.part_surv <- function(x, parameters) {
  
  n_marginal = length(x$marginals)
  
  # Now handles two cases for how survival distributions can be passed
  # 1 User passes a surival distribution object -> run compute_surv
  # 2 User passes a numeric vector -> assume that vector is survival
  marginals <- lapply(
    seq_len(n_marginal),
    function(i){
      mar_eval <- lazyeval::lazy_eval(
        x$marginals[[i]],
        data=dplyr::slice(parameters, 1)
      )
      if(!is.numeric(mar_eval)) {
          # If a survival distribution is given, compute survival
          ret <- compute_surv(
            mar_eval,
            time = parameters$markov_cycle - 1,
            cycle_length =  x$cycle_length[i],
            type = "surv"
          )
      } else {
        # If a numeric is given, treat it as pre-computed survival
        # Have to re-evaluate it again but using all cycles
        mar_eval <- lazyeval::lazy_eval(
          x$marginals[[i]],
          data=parameters
        )
        ret <- mar_eval
      }
    }
  )
  
  # Ensure that OS >= PFS
  adj_marginals = rev(
    Reduce(
      function(a, b) pmin(a, b),
      rev(marginals),
      accumulate = TRUE
    )
  )
  
  structure(
    list(
      marginals = adj_marginals,
      state_names = x$state_names
    ),
    class = "eval_part_surv")
}

compute_counts.eval_part_surv <- function(x, init,
                                          method, inflow) {
  
  # Compute trace (PPS = OS - PFS, etc...)
  res <- Reduce(
    function(a, b) {
      aMat <- as.matrix(a)
      nColA <- ncol(aMat)
      cbind(aMat, b - rowSums(aMat))
    },
    x$marginals
  )
  
  res <- as.data.frame(cbind(res, 1 - rowSums(res)))
  
  colnames(res) <- x$state_names
  
  if (any(res < 0)) {
    stop("Negative counts in partitioned model.")
  }
  
  res <- res * sum(init)
  
  structure(res, class = c("cycle_counts", class(res)))
}
