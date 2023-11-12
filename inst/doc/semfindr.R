## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(semfindr)
dat <- pa_dat
head(dat)

## -----------------------------------------------------------------------------
mod <-
"
m1 ~ iv1 + iv2
dv ~ m1
"

## -----------------------------------------------------------------------------
library(lavaan)
fit <- sem(mod, dat)

## ----echo = FALSE, include = FALSE--------------------------------------------
if (file.exists("semfindr_fit_rerun.RDS")) {
    fit_rerun <- readRDS("semfindr_fit_rerun.RDS")
  } else {
    fit_rerun <- lavaan_rerun(fit)
    saveRDS(fit_rerun, "semfindr_fit_rerun.RDS")
  }

## ----echo = FALSE-------------------------------------------------------------
fit_no1 <- sem(mod, dat[-1, ])
fit_no43 <- sem(mod, dat[-43, ])

## ----eval = FALSE-------------------------------------------------------------
#  fit_rerun <- lavaan_rerun(fit)

## -----------------------------------------------------------------------------
fit_est_change <- est_change(fit_rerun)
fit_est_change

## ----echo = FALSE-------------------------------------------------------------
i <- order(fit_est_change[, "gcd"],
           decreasing = TRUE)
i_top5 <- i[1:5]
round(fit_est_change[i_top5, ], 3)
i_top5_gcd_overall <- i_top5

## -----------------------------------------------------------------------------
fit_est_change_paths_only <- est_change(fit_rerun,
                                        parameters = c("m1 ~ iv1",
                                                      "m1 ~ iv2",
                                                      "dv ~ m1"))
fit_est_change_paths_only

## ----echo = FALSE-------------------------------------------------------------
i <- order(fit_est_change_paths_only[, "gcd"],
           decreasing = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  fit_est_change_paths_only <- est_change(fit_rerun,
#                                          parameters = c("~"))

## ----echo = FALSE-------------------------------------------------------------
i_top5_gcd_paths <- i[1:5]

## -----------------------------------------------------------------------------
fit_est_change_raw <- est_change_raw(fit_rerun)
fit_est_change_raw

## -----------------------------------------------------------------------------
fit_est_change_raw_std <- est_change_raw(fit_rerun,
                                         standardized = TRUE)
fit_est_change_raw_std

## -----------------------------------------------------------------------------
standardizedSolution(fit, se = FALSE)[1, ]
standardizedSolution(sem(mod, dat[-43, ]), se = FALSE)[1, ]

## -----------------------------------------------------------------------------
fit_est_change_raw_std_paths <- est_change_raw(fit_rerun,
                                               standardized = TRUE,
                                               parameters = c("m1 ~ iv1",
                                                              "m1 ~ iv2",
                                                              "dv ~ m1"))
fit_est_change_raw_std_paths

## ----eval = FALSE-------------------------------------------------------------
#  fit_est_change_raw_std_paths <- est_change_raw(fit_rerun,
#                                                 standardized = TRUE,
#                                                 parameters = c("~"))

## -----------------------------------------------------------------------------
fit_md <- mahalanobis_rerun(fit_rerun)
fit_md

## -----------------------------------------------------------------------------
fit_mc <- fit_measures_change(fit_rerun,
            fit_measures = c("chisq", "cfi", "tli", "rmsea"))
fit_mc

## -----------------------------------------------------------------------------
print(fit_mc, sort_by = "chisq")

## ----eval = FALSE-------------------------------------------------------------
#  fit_mc <- fit_measures_change(fit_rerun)

## -----------------------------------------------------------------------------
fit_influence <- influence_stat(fit_rerun)
fit_influence

## -----------------------------------------------------------------------------
gcd_plot(fit_influence,
         largest_gcd = 3)

## -----------------------------------------------------------------------------
md_plot(fit_influence,
        largest_md = 3)

## -----------------------------------------------------------------------------
gcd_gof_plot(fit_influence,
             fit_measure = "rmsea",
             largest_gcd = 3,
             largest_fit_measure = 3)

## -----------------------------------------------------------------------------
gcd_gof_md_plot(fit_influence,
                fit_measure = "rmsea",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 15)

## ----fig.height = 7-----------------------------------------------------------
est_change_plot(fit_est_change,
                largest_change = 3)

## -----------------------------------------------------------------------------
est_change_plot(fit_est_change,
                parameters = "~",
                largest_change = 3)

## -----------------------------------------------------------------------------
est_change_plot(fit_est_change_raw,
                parameters = "~",
                largest_change = 3)

## ----eval = FALSE-------------------------------------------------------------
#  est_change_plot(fit_influence,
#                  parameters = "~",
#                  largest_change = 3)

## ----fig.height = 7-----------------------------------------------------------
est_change_gcd_plot(fit_est_change,
                    largest_gcd = 3)

## -----------------------------------------------------------------------------
est_change_gcd_plot(fit_est_change,
                    parameters = "~",
                    largest_gcd = 3)

## ----eval = FALSE-------------------------------------------------------------
#  est_change_gcd_plot(fit_influence,
#                      parameters = "~",
#                      largest_gcd = 3)

## -----------------------------------------------------------------------------
fit_est_change_approx <- est_change_approx(fit)
fit_est_change_approx

## -----------------------------------------------------------------------------
fit_est_change_approx_paths <- est_change_approx(fit,
                                                 parameters = "~")
fit_est_change_approx_paths

## -----------------------------------------------------------------------------
fit_est_change_raw_approx <- est_change_raw_approx(fit)
fit_est_change_raw_approx

## -----------------------------------------------------------------------------
fit_md <- mahalanobis_rerun(fit)
fit_md

## -----------------------------------------------------------------------------
fit_mc_approx <- fit_measures_change_approx(fit,
                   fit_measures = c("chisq", "cfi", "tli", "rmsea"))
fit_mc_approx

## -----------------------------------------------------------------------------
print(fit_mc_approx, sort_by = "chisq")

## ----eval = FALSE-------------------------------------------------------------
#  fit_mc_approx <- fit_measures_change_approx(fit)

## -----------------------------------------------------------------------------
fit_influence_approx <- influence_stat(fit)
fit_influence_approx

## -----------------------------------------------------------------------------
gcd_plot(fit_influence_approx,
         largest_gcd = 3)

## -----------------------------------------------------------------------------
md_plot(fit_influence_approx,
        largest_md = 3)

## -----------------------------------------------------------------------------
gcd_gof_plot(fit_influence_approx,
             fit_measure = "rmsea",
             largest_gcd = 3,
             largest_fit_measure = 3)

## -----------------------------------------------------------------------------
gcd_gof_md_plot(fit_influence_approx,
                fit_measure = "rmsea",
                largest_gcd = 3,
                largest_fit_measure = 3,
                largest_md = 3,
                circle_size = 15)

## ----fig.height = 7-----------------------------------------------------------
est_change_plot(fit_est_change_approx,
                largest_change = 3)

## -----------------------------------------------------------------------------
est_change_plot(fit_est_change_approx,
                parameters = "~",
                largest_change = 3)

## -----------------------------------------------------------------------------
est_change_plot(fit_est_change_raw_approx,
                parameters = "~",
                largest_change = 3)

## ----eval = FALSE-------------------------------------------------------------
#  est_change_plot(fit_influence_approx,
#                  parameters = "~",
#                  largest_change = 3)

## ----fig.height = 7-----------------------------------------------------------
est_change_gcd_plot(fit_est_change_approx,
                    largest_gcd = 3)

## -----------------------------------------------------------------------------
est_change_gcd_plot(fit_est_change_approx,
                    parameters = "~",
                    largest_gcd = 3)

## ----eval = FALSE-------------------------------------------------------------
#  est_change_gcd_plot(fit_influence_approx,
#                      parameters = "~",
#                      largest_gcd = 3)

