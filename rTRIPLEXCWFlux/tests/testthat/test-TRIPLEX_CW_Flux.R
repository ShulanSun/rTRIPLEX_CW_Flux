library(rTRIPLEXCWFlux)
library(testthat)

test_that("basic model run", {
  path <- system.file(onemonth_exam, package = "rTRIPLEXCWFlux")
  path <- system.file(Inputpara, package = "rTRIPLEXCWFlux")

  out <- TRIPLEX_CW_Flux(
    Input_variable=onemonth_exam,Input_parameter=Inputpara,
    overyear=FALSE)

  expect_type(out, "list")
})

