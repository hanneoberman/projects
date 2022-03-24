#' Computes convergence diagnostics for a \code{mids} object
#'
#' Takes an object of class \code{mids}, computes the autocorrelation
#' and/or potential scale reduction factor, and returns a \code{data.frame}
#' with the specified diagnostic(s) per iteration.
#'
#' @aliases convergence
#' @param data An object of class \code{mids} as created by the function
#' \code{mice()}.
#' @param diagnostic A keyword. One of the following keywords: \code{"ac"},
#' \code{"all"}, \code{"gr"} and \code{"psrf"}. See the Details section
#' for the interpretation.
#' The default is \code{diagnostic = "all"} which returns both the
#' autocorrelation and potential scale reduction factor per iteration.
#' @param \dots Additional arguments. Not used.
#' @return A \code{data.frame} with the autocorrelation and/or potential
#' scale reduction factor per iteration of the MICE algorithm.
#' @details
#' The argument \code{diagnostic} can be length-1 character, which is
#' matched to one of the following keywords:
#' \describe{
#' \item{\code{"all"}}{computes both the autocorrelation as well as the
#' potential scale reduction factor per iteration;}
#' \item{\code{"ac"}}{computes only the autocorrelation per iteration;}
#' \item{\code{"psrf"}}{computes only the potential scale reduction factor
#' per iteration;}
#' \item{\code{"gr"}}{same as \code{psrf}, the potential scale reduction
#' factor is colloquially called the Gelman-Rubin diagnostic.}
#' }
#' @seealso \code{\link{mice}}, \code{\link[=mids-class]{mids}}
#' @keywords none
#' @examples
#'
#' # obtain imputed data set
#' imp <- mice(nhanes2, maxit = 6, print = FALSE)
#' # compute convergence diagnostics
#' convergence(imp)
#' @export
convergence.mids <- function(data, diagnostic = "all", ...) {
  mice:::install.on.demand("rstan", ...)
  mice:::install.on.demand("purrr", ...)
  if (!is.mids(data))
    stop("'data' not of class 'mids'")
  if (diagnostic == "gr") {
    diagnostic <- "psrf"
  }
  
  m <- as.integer(data$m)
  t <- as.integer(data$iteration)
  vars <- names(data$data)
  out <- data.frame(it = 1:t)
  
  # reshape into list per imputation
  per_m <-
    lapply(1:m, function(x)
      data$chainMean[, , x] %>%
        t() %>%
        as.data.frame())
  
  # reshape into list per variable
  per_v <- purrr::map(vars, ~ {
    per_m %>%
      purrr::map(.x) %>%
      do.call(base::cbind, .)
  })
  
  # compute autocorrelation
  if (diagnostic == "all" | diagnostic == "ac") {
    ac <-
      purrr::map(per_v, function(.x) {
        purrr::map_dbl(3:t, function(.y) {
          purrr::map_dbl(1:m, function(.z) {
            suppressWarnings(cor(.x[1:.y - 1, .z], .x[2:.y, .z]))
          }) %>% max()
        })
      }) %>%
      stats::setNames(., paste0("ac_", vars)) %>%
      as.data.frame() %>%
      base::rbind(NA, NA, .)
    out <- base::cbind(out, ac)
  }
  
  # compute potential scale reduction factor
  if (diagnostic == "all" | diagnostic == "psrf") {
    psrf <- per_v %>%
      stats::setNames(., paste0("psrf_", vars)) %>%
      purrr::map_dfc(., ~ purrr::map_dbl(1:t, function(it) {
        rstan::Rhat(.[1:it, ])
      }))
    out <- base::cbind(out, psrf)
  }
  # make this a list with 2 dataframes and make it an S3 object for plotting
  return(out)
}

psrf <- function(theta, ...){
  chain_mean <- apply(theta[[2]], 2, mean)
  chain_var <- apply(theta[[2]], 2, var)
  sqrt(((t * var(chain_mean)) / mean(chain_var) + t - 1) / t)
}

splits <- cbind(theta[[2]][1:floor(t/2), ], theta[[2]][ceiling(t/2 + 1):t, ])
ranks <- rank(splits, ties.method = 'average')
norms <- qnorm((ranks - 1 / 2) / (t*m))
# bulk_rhat <- rhat_rfun(z_scale(split_chains(sims)))
# sims_folded <- abs(sims - median(sims))
# tail_rhat <- rhat_rfun(z_scale(split_chains(sims_folded)))
# max(bulk_rhat, tail_rhat)