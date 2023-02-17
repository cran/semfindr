## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dat----------------------------------------------------------------------
library(semfindr)
dat <- pa_dat

## ----fit----------------------------------------------------------------------
library(lavaan)
mod <-
"
m1 ~ iv1 + iv2
dv ~ m1
"
fit <- sem(mod, dat)

## ----fit_rerun----------------------------------------------------------------
if (file.exists("semfindr_fit_rerun.RDS")) {
    fit_rerun <- readRDS("semfindr_fit_rerun.RDS")
  } else {
    fit_rerun <- lavaan_rerun(fit)
    saveRDS(fit_rerun, "semfindr_fit_rerun.RDS")
  }

## ----lav-scores-head----------------------------------------------------------
head(lavScores(fit)[ , 1, drop = FALSE])

## ----fit_est_change_approx----------------------------------------------------
fit_est_change_approx <- est_change_raw_approx(fit)
head(fit_est_change_approx)

## ----compare-est-change-------------------------------------------------------
# From semfindr
fit_est_change_raw <- est_change_raw(fit_rerun)
# Plot the differences
library(ggplot2)
tmp1 <- as.vector(t(as.matrix(fit_est_change_raw)))
tmp2 <- as.vector(t(as.matrix(fit_est_change_approx)))
est_change_df <- data.frame(param = rep(colnames(fit_est_change_raw),
                                        nrow(fit_est_change_raw)),
                             est_change = tmp1,
                             est_change_approx = tmp2)
ggplot(est_change_df, aes(x = est_change, y = est_change_approx)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 0.8, alpha = 0.5) +
  facet_wrap(~ param) +
  coord_fixed()

## ----approx-gcd---------------------------------------------------------------
# Information matrix (Hessian)
information_fit <- lavInspect(fit, what = "information")
# Short cut for computing quadratic form (https://stackoverflow.com/questions/27157127/efficient-way-of-calculating-quadratic-forms-avoid-for-loops)
gcd_approx <- (nobs(fit) - 1) * rowSums(
  (fit_est_change_approx %*% information_fit) * fit_est_change_approx
)

## ----est_change_approx-gcd----------------------------------------------------
fit_est_change_approx <- est_change_approx(fit)
head(fit_est_change_approx)

## ----compare-approx-gcd-------------------------------------------------------
# Compare to exact computation
fit_est_change <- est_change(fit_rerun)
# Plot
gcd_df <- data.frame(
  gcd_exact = fit_est_change[ , "gcd"],
  gcd_approx = fit_est_change_approx[ , "gcd_approx"]
)
ggplot(gcd_df, aes(x = gcd_exact, y = gcd_approx)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  coord_fixed()

## ----cor-gcd------------------------------------------------------------------
cor(gcd_df, method = "spearman")

## ----lli----------------------------------------------------------------------
lli <- lavInspect(fit, what = "loglik.casewise")
head(lli)

## ----compare-lli--------------------------------------------------------------
# Predicted ll without observation 1
fit@loglik$loglik - lli[1]
# Actual ll without observation 1
fit_no1 <- sem(mod, dat[-1, ])
fit_no1@loglik$loglik

## ----chisq_i_approx-----------------------------------------------------------
chisq_i_approx <- fit_measures_change_approx(fit)
# Compare to the actual chisq when dropping observation 1
c(predict = chisq_i_approx[1, "chisq"] + fitmeasures(fit, "chisq"),
  actual = fitmeasures(fit_no1, "chisq"))

## ----plot-change-chisq--------------------------------------------------------
# Exact measure from semfindr
out <- fit_measures_change(fit_rerun)
# Plot
chisq_change_df <- data.frame(
  chisq_change = out[ , "chisq"],
  chisq_change_approx = chisq_i_approx[ , "chisq"]
)
ggplot(chisq_change_df, aes(x = chisq_change, y = chisq_change_approx)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  coord_fixed()

## ----plot-change-rmsea--------------------------------------------------------
# Plot
rmsea_change_df <- data.frame(
  rmsea_change = out[ , "rmsea"],
  rmsea_change_approx = chisq_i_approx[ , "rmsea"]
)
ggplot(rmsea_change_df, aes(x = rmsea_change, y = rmsea_change_approx)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  coord_fixed()

