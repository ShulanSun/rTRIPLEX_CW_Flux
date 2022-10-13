library(rTRIPLEXCWFlux)
library(testthat)

test_that("basic model run", {
  path <- system.file(Inputvariable, package = "rTRIPLEXCWFlux")
  path <- system.file(Inputpara, package = "rTRIPLEXCWFlux")

  out <- TRIPLEX_CW_Flux(
    Input_variable=Inputvariable,Input_parameter=Inputpara)

  expect_type(out, "list")
})

