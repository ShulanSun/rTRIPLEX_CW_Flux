library(rTRIPLEXCWFlux)
library(testthat)

test_that("basic model run", {
  path <- system.file(Inputvariable, package = "rTRIPLEXCWFlux")
  path <- system.file(Inputpara, package = "rTRIPLEXCWFlux")

  examdata<-data.frame()
  for(i in 1:12){
    subdata<-subset(Inputvariable,Inputvariable$Month==i)
    mondata<-subset(subdata,subdata$Day<=2)
    examdata<-rbind(examdata,mondata)
  }

  out <- TRIPLEX_CW_Flux(
    Input_variable=examdata,Input_parameter=Inputpara,
    overyear=TRUE)

  expect_type(out, "list")
})
