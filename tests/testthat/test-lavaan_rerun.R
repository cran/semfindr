library(testthat)
library(lavaan)
library(semfindr)

#context("Test lavaan_rerun")

mod <- 
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

test_that("Compare parameter estimates of omitting an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        parameterEstimates(fit0_15), parameterEstimates(rerun_15)
      )
  })

