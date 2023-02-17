## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(semfindr)
dat <- pa_dat
# Add case id
dat <- cbind(id = paste0("case", seq_len(nrow(dat))), dat)
head(dat)

## -----------------------------------------------------------------------------
mod <-
"
m1 ~ iv1 + iv2
dv ~ m1
"
library(lavaan)
fit <- sem(mod, dat)

## ----echo = FALSE, include = FALSE--------------------------------------------
if (file.exists("user_id_fit_rerun.RDS")) {
    fit_rerun <- readRDS("user_id_fit_rerun.RDS")
  } else {
    fit_rerun <- lavaan_rerun(fit, case_id = dat$id)
    saveRDS(fit_rerun, "user_id_fit_rerun.RDS")
  }

## ----eval = FALSE-------------------------------------------------------------
#  fit_rerun <- lavaan_rerun(fit, case_id = dat$id)

## -----------------------------------------------------------------------------
head(fit_rerun$rerun[1:3])

## -----------------------------------------------------------------------------
fit_est_change <- est_change(fit_rerun)
round(head(fit_est_change), 3)

## -----------------------------------------------------------------------------
fit_est_change_paths_only <- est_change(fit_rerun,
                                parameters = c("m1 ~ iv1",
                                               "m1 ~ iv2",
                                               "dv ~ m1"))
round(fit_est_change_paths_only[1:5, ], 3)

## -----------------------------------------------------------------------------
fit_est_change_raw <- est_change_raw(fit_rerun)
round(fit_est_change_raw[1:5, ], 3)

## -----------------------------------------------------------------------------
fit_md <- mahalanobis_rerun(fit_rerun)
round(fit_md[1:5, , drop = FALSE], 3)

## -----------------------------------------------------------------------------
fit_mc <- fit_measures_change(fit_rerun,
            fit_measures = c("chisq", "cfi", "tli", "rmsea"))
round(fit_mc[1:5, ], 3)

## -----------------------------------------------------------------------------
fit_influence <- influence_stat(fit_rerun)
round(fit_influence[1:5, ], 3)

## -----------------------------------------------------------------------------
gcd_plot(fit_influence, largest_gcd = 3)

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

