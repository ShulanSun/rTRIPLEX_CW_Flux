## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=TRUE----------------------------------------------------------------
library(rTRIPLEXCWFlux)
out<-TRIPLEX_CW_Flux (Input_variable=Inputvariable,
                 Input_parameter=Inputpara)
head(out)

## ----echo=FALSE,out.width ="60%",fig.align = 'center'-------------------------
knitr::include_graphics("../man/Figures/ET_seasonal19.png")

## ----echo=FALSE,out.width="60%",fig.align = 'center'--------------------------
knitr::include_graphics("../man/Figures/NEP_seasonal19.png")

## ----echo=FALSE,out.width="60%",fig.align = 'center'--------------------------
knitr::include_graphics("../man/Figures/ET_NEP.png")

## ----echo=FALSE,out.width="80%",fig.align = 'center'--------------------------
knitr::include_graphics("../man/Figures/Diurnal_NEP.png")

## ----echo=FALSE,out.width="80%",fig.align = 'center'--------------------------
knitr::include_graphics("../man/Figures/Diurnal_ET.png")

## ----echo=FALSE,out.width="60%",fig.align = 'center'--------------------------
knitr::include_graphics("../man/Figures/Environmental_factors.png")

