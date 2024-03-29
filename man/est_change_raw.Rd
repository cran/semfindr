% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_change_raw.R
\name{est_change_raw}
\alias{est_change_raw}
\title{Case Influence on Parameter Estimates}
\usage{
est_change_raw(
  rerun_out,
  parameters = NULL,
  standardized = FALSE,
  user_defined_label_full = FALSE
)
}
\arguments{
\item{rerun_out}{The output from \code{\link[=lavaan_rerun]{lavaan_rerun()}}.}

\item{parameters}{A character vector to specify the selected
parameters. Each parameter is named as in \code{lavaan} syntax, e.g.,
\code{x ~ y} or \code{x ~~ y}, as appeared in the columns \code{lhs}, \code{op}, and \code{rhs}
in the output of \code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}}.
Supports specifying an operator to select all parameters with these
operators: \code{~}, \verb{~~}, \verb{=~}, and \code{~1}. This vector can contain
both parameter names and operators. More details can be found
in the help of \code{\link[=pars_id]{pars_id()}}.
If omitted or \code{NULL}, the
default, changes on all free parameters will be computed.}

\item{standardized}{If \code{TRUE}, the changes in the full standardized
solution is returned (\code{type} = \code{std.all} in
\code{\link[lavaan:standardizedSolution]{lavaan::standardizedSolution()}}). Otherwise, the changes in the
unstandardized solution are returned. Default is \code{FALSE}.}

\item{user_defined_label_full}{Logical. If \code{TRUE}, use the full
labels for user-defined parameters (parameters created by
\verb{:=}), which include the definition. If \code{FALSE}, then only
the label on the right-hand side of \verb{:=} will be used.
Default is \code{FALSE}. In previous version, the full labels
were used. Set to \code{TRUE} if backward compatibility
is needed.}
}
\value{
An \code{est_change}-class object, which is
matrix with the number of columns equals to the number of
requested parameters, and the number of rows equals to the number
of cases. The row names are the case identification values used in
\code{\link[=lavaan_rerun]{lavaan_rerun()}}. The elements are the raw differences.
A print method is available for user-friendly output.
}
\description{
Gets a \code{\link[=lavaan_rerun]{lavaan_rerun()}} output and computes the
changes in selected parameters for each case if included.
}
\details{
For each case, \code{\link[=est_change_raw]{est_change_raw()}} computes the differences
in the estimates of selected parameters with and without this
case:

(Estimate with all case) - (Estimate without this case).

The
change is the raw change, either for the standardized or
unstandardized solution. The change is \emph{not} divided by standard
error.
This is a measure of the influence of a case on the parameter
estimates if it is included.

If the value of a case is positive, including the case increases an estimate.

If the value of a case is negative, including the case decreases an estimate.

If the analysis is not admissible or did not converge when a case
is deleted, \code{NA}s will be returned for this case on the
differences.

Supports both single-group and multiple-group models.
(Support for multiple-group models available in 0.1.4.8 and later version).
}
\examples{
library(lavaan)

# A path model

dat <- pa_dat
mod <-
"
m1 ~ a1 * iv1 + a2 * iv2
dv ~ b * m1
a1b := a1 * b
a2b := a2 * b
"
# Fit the model
fit <- lavaan::sem(mod, dat)

# Fit the model n times. Each time with one case is removed.
# For illustration, do this only for four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(3, 5, 7, 8))
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates.
out <- est_change_raw(fit_rerun)
# Results excluding a case
out
# Note that these are the differences in parameter estimates.

# The parameter estimates from all cases
(coef_all <- coef(fit))
# The parameter estimates from manually deleting the third case
fit_no_3 <- lavaan::sem(mod, dat[-3, ])
(coef_no_3 <- coef(fit_no_3))
# The differences
coef_all - coef_no_3
# The first row of `est_change_raw(fit_rerun)`
round(out[1, ], 3)

# Compute only the changes of the paths from iv1 and iv2 to m1
out2 <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
# Results excluding a case
out2
# Note that only the changes in the selected paths are included.

# Use standardized = TRUE to compare the differences in standardized solution
out2_std <- est_change_raw(fit_rerun,
                           c("m1 ~ iv1", "m1 ~ iv2"),
                           standardized = TRUE)
out2_std
(est_std_all <- parameterEstimates(fit,
                 standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
(est_std_no_1 <- parameterEstimates(fit_no_3,
                 standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
# The differences
est_std_all$std.all - est_std_no_1$std.all
# The first row of `out2_std`
out2_std[1, ]

# A CFA model

dat <- cfa_dat
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f1 ~~ f2
"
# Fit the model
fit <- lavaan::cfa(mod, dat)

# Examine four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 3, 5, 7))
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates.
# For free loadings only
out <- est_change_raw(fit_rerun, parameters = "=~")
out
# For standardized loadings only
out_std <- est_change_raw(fit_rerun, parameters = "=~",
                          standardized = TRUE)
out_std

# A latent variable model

dat <- sem_dat
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   a * f1
f3 ~   b * f2
ab := a * b
"
# Fit the model
fit <- lavaan::sem(mod, dat)

# Examine four selected cases
fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
                          to_rerun = c(2, 3, 5, 7))
# Compute the changes in parameter estimates if a case is included
# vs. if this case is excluded.
# That is, case influence on parameter estimates.
# For structural paths only
out <- est_change_raw(fit_rerun, parameters = "~")
out
# For standardized paths only
out_std <- est_change_raw(fit_rerun, parameters = "~",
                          standardized = TRUE)
out_std


}
\references{
Pek, J., & MacCallum, R. (2011). Sensitivity analysis
in structural equation models: Cases and their influence.
\emph{Multivariate Behavioral Research, 46}(2), 202-228.
doi:10.1080/00273171.2011.561068
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}.
}
