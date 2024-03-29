#' @title Sample Data: A Multiple-Group CFA Model with an Influential Case
#'
#' @description A six-variable dataset with 100 cases.
#'
#' @format A data frame with 100 rows
#' and 6 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#'   \item{gp}{Group variable. Character. Either "GroupA" or "GroupB".}
#' }
#'
#' @examples
#' library(lavaan)
#' data(cfa_dat_mg)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' "
#' fit1 <- cfa(mod, cfa_dat_mg, group = "gp")
#' fit2 <- cfa(mod, cfa_dat_mg, group = "gp",
#'             group.equal = "loadings")
#' fit3 <- cfa(mod, cfa_dat_mg, group = "gp",
#'             group.equal = c("loadings", "intercepts"))
#' lavTestLRT(fit1, fit2, fit3)
#' lavTestLRT(fit1, fit3)
#'
#' # Drop the first case
#' cfa_dat_mgb <- cfa_dat_mg[-1, ]
#' fit1b <- cfa(mod, cfa_dat_mgb, group = "gp")
#' fit2b <- cfa(mod, cfa_dat_mgb, group = "gp",
#'              group.equal = "loadings")
#' fit3b <- cfa(mod, cfa_dat_mgb, group = "gp",
#'              group.equal = c("loadings", "intercepts"))
#' lavTestLRT(fit1b, fit2b, fit3b)
#' lavTestLRT(fit1b, fit3b)
#'
"cfa_dat_mg"
