#' @title Runs a TRIPLEX-CW-Flux model simulation
#' @description Runs the TRIPLEX-CW-Flux model. For more details on input variables and parameters and structure of input visit \code{\link{data}}.
#'
#' @param Input_variable A table as described in \code{\link{Inputpara}} containing the information about input variables.
#' @param Input_parameter A table as described in \code{\link{Inputvariable}} containing the information about input parameters.
#' @param overyear If overyear is 'TRUE', this means that the input data is more than one year. The outputs of the TRIPLEX_CW_Flux function are a long format dataframe and charts of simulated result for net ecosystem productivity (NEP) and evapotranspiration (ET) at 30 min scale, and monthly variation of the input environmental factors.
#'
#' @return A list with class "result" containing the simulated results and charts for NEP and ET at 30 min scale, and monthly variation of the input environmental factors
#' @export
#'
#' @examples
#' library(rTRIPLEXCWFlux)
#' TRIPLEX_CW_Flux (Input_variable=onemonth_exam,Input_parameter=Inputpara,overyear=FALSE)
#'
#' @references
#' Evaporation and Environment. Symposia of the Society for Experimental Biology, 19, 205-234. Available at the following web site: \url{https://www.semanticscholar.org/paper/Evaporation-and-environment.-Monteith/428f880c29b7af69e305a2bf73e425dfb9d14ec8}
#' Zhou, X.L., Peng, C.H., Dang, Q.L., Sun, J.F., Wu, H.B., &Hua, D. (2008). Simulating carbon exchange in Canadian Boreal forests: I. Model structure, validation, and sensitivity analysis. Ecological Modelling,219(3-4), 287-299. \doi{https://doi.org/10.1016/j.ecolmodel.2008.07.011}
#'
#' @importFrom grDevices dev.new
#' @importFrom graphics abline axis barplot box curve
#' @importFrom stats aggregate lm residuals
#' @importFrom graphics legend mtext par points text


TRIPLEX_CW_Flux<- function(Input_variable,Input_parameter,
                           overyear=FALSE) {

  ## To maintain user's original options
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))


  # Measured net ecosystem production and evapotranspiration
  Input_variable$ObserveNEE30<-Input_variable$NEE*1800/1000/44*12*(-1)
  Input_variable$OETS<-0.43*Input_variable$LE/(597-0.564*Input_variable$Ta)

  # Input variable assignment
  Cappm<-Input_variable$Cof # CO2 concentration in the atmosphere, ppm
  Tem<-Input_variable$Ta # Air temperature, degrees Celsius
  Day<-Input_variable$DOY # Day of year, day
  time<-Input_variable$time # Local time, h
  RH<-Input_variable$RH # Relative humidity, %
  PPFD<-Input_variable$PPFD # Photosynthetic photon flux density, μmol m-2 s-1
  SWC<-Input_variable$SVWC30cm # Soil volumetric moisture at depth of 30 cm, %
  Rn<-Input_variable$Rn # Net radiation at the canopy surface, W m-2
  G<-Input_variable$G # Soil heat flux, W m-2
  VPD1<-Input_variable$VPDhpa/10 # Vapor pressure deficit, kPa

  # Procedure variable assignment for loop computation
  x<-nrow(Input_variable)
  Cippm<-rep(0,x);Ca<-rep(0,x);Ci<-rep(0,x);Error1<-rep(0,x);COp<-rep(0,x);
  Kc<-rep(0,x);Ko<-rep(0,x);K<-rep(0,x);fN<-rep(0,x);fT<-rep(0,x);Vm<-rep(0,x);
  Vc<-rep(0,x);Jmax<-rep(0,x);J<-rep(0,x);Vj1<-rep(0,x);Rd<-rep(0,x);
  Vjsh1<-rep(0,x);Ld<-rep(0,x);Et<-rep(0,x);to<-rep(0,x);h<-rep(0,x);d<-rep(0,x);
  sinB<-rep(0,x);kb1<-rep(0,x);kb2<-rep(0,x);pcb<-rep(0,x);X<-rep(0,x);Y<-rep(0,x);
  Z<-rep(0,x);Icsh<-rep(0,x);bsh<-rep(0,x);csh<-rep(0,x);Jsh1<-rep(0,x);
  Jsh2<-rep(0,x);bsunsh<-rep(0,x);csunsh<-rep(0,x);Jsunsh1<-rep(0,x);
  Jsunsh2<-rep(0,x);Vjsh2<-rep(0,x);Vjsh<-rep(0,x);Vjsun<-rep(0,x);VcVjsun<-rep(0,x);
  VcVjsh<-rep(0,x);Asunsh<-rep(0,x);
  Acsun<-rep(0,x);Acsh<-rep(0,x);Acanopy<-rep(0,x);AV<-rep(0,x);Lsh<-rep(0,x)
  Vj<-rep(0,x);kb<-rep(0,x);Lsun<-rep(0,x);ZZ<-rep(0,x);
  Icsun<-rep(0,x);Ic<-rep(0,x);Ratio1<-rep(0,x)
  Asun<-rep(0,x);Ash<-rep(0,x);Ratio<-rep(0,x);CJ4<-rep(0,x);fsun<-rep(0,x);
  fsh<-rep(0,x);GPPsec<-rep(0,x);Re30min<-rep(0,x);Rh30min<-rep(0,x);
  NEP30min<-rep(0,x);gs<-rep(0,x);Rmf<-rep(0,x);Rms<-rep(0,x);Rmr<-rep(0,x);
  Rm<-rep(0,x);Rgf<-rep(0,x);Rgs<-rep(0,x);Rgr<-rep(0,x);Rg<-rep(0,x);
  Rm30min<-rep(0,x);Rg30min<-rep(0,x);NPP<-rep(0,x);
  NPP30min<-rep(0,x);GPP30min<-rep(0,x);Z0M<-rep(0,x);Z0V<-rep(0,x);
  d<-rep(0,x);svt<-rep(0,x);ra<-rep(0,x);LES<-rep(0,x);gsms<-rep(0,x);ETS<-rep(0,x)

  # Calculation process of TRIPLEX-CW-Flux model
  for(i in 1:x){
    # Rubisco-limited gross photosynthesis rates. (Assum: Rubisco-limited gross photosynthesis rates in shaded leaves and sunlit leaves are equal)
    j<-0
    # Setting initial value of intercellular CO2 concentration, assume: Cippm=Cappm
    Cippm[i]<-Cappm[i]-j

    Ca[i]<-Cappm[i]/1000000*1.013*100000 # Unit conversion: from ppm to Pa
    Ci[i]<-Cippm[i]/1000000*1.013*100000 # Unit conversion: from ppm to Pa

    Vm25<-Input_parameter$Vm25 # Maximum carboxylation rate at 25℃, μmol m-2 s-1
    Rgas<-Input_parameter$Rgas # Molar gas constant, m3 Pa mol-1 K-1
    O2<-Input_parameter$O2 # Oxygen concentration in the atmosphere, Pa
    N<-Input_parameter$N # Leaf nitrogen content, %
    Nm<-Input_parameter$Nm # Maximum nitrogen content, %
    fN<-N/Nm # Nitrogen limitation term
    Error1[i]<-Ci[i]/Ca[i] # Just for check
    COp[i]<-1.92*10^-4*O2*1.75^((Tem[i]-25)/10) # CO2 concentration point without dark respiration, Pa
    Kc[i]<-30*2.1^((Tem[i]-25)/10) # Michaelis–Menten constants for CO2, Pa
    Ko[i]<-30000*1.2^((Tem[i]-25)/10) # Michaelis–Menten constants for O2, Pa
    K[i]<-Kc[i]*(1+O2/Ko[i]) # Function of enzyme kinetics, Pa
    fT[i]<-(1+exp((-220000+710*(Tem[i]+273))/(Rgas*(Tem[i]+273))))^-1 # Temperature limitation term
    Vm[i]<-Vm25*2.4^((Tem[i]-25)/10)*fT[i]*fN # Maximum carboxylation rate, μmol m-2 s-1
    Vc[i]<-Vm[i]*(Ci[i]-COp[i])/(Ci[i]+K[i]) # Rubisco-limited gross photosynthesis rates, μmol m-2 s-1

    # Light-limited gross photosynthesis rates for big leaf
    Jmax[i]<-29.1+1.64*Vm[i] # Light-saturated rate of electron transport in the photosynthetic carbon reduction cycle in leaf cells, μmol m-2 s-1
    J[i]<-Jmax[i]*PPFD[i]/(PPFD[i]+2.1*Jmax[i]) # Electron transport rate, μmol m-2 s-1
    Vj1[i]<-J[i]*(Ci[i]-COp[i])/(4.5*Ci[i]+10.5*COp[i])
    Vj[i]<-ifelse(Vj1[i]>0,Vj1[i],0) # Light-limited gross photosynthesis rates, μmol m-2 s-1
    # Vj[i]<-max(J[i]*(Ci[i]-COp[i])/(4.5*Ci[i]+10.5*COp[i]),0) # Light-limited gross photosynthesis rates, μmol m-2 s-1

    # Solar geometry
    Ls<-Input_parameter$Ls # Standard longitude of time zone
    Le<-Input_parameter$Le # Local longitude, degree
    latitude<-Input_parameter$latitude # Local latitude, degree
    lat<-latitude*3.14/180 # Convert angle to radian, radian
    LAI<-Input_parameter$LAI # Leaf area index, m2 m-2
    Ld[i]<-2*3.14*(Day[i]-1)/365 # Day angle
    Et[i]<-0.017+0.4281*cos(Ld[i])-7.351*sin(Ld[i])-3.349*cos(2*Ld[i])-9.731*sin(Ld[i]) # Equation of time, min
    to[i]<-12+(4*(Ls-Le)-Et[i])/60 # Solar noon, h
    h[i]<-3.14*(time[i]-to[i])/12 # Hour angle of sun, radians
    d[i]<--23.4*3.14/180*cos(2*3.14*(Day[i]+10)/365) # Solar declination angle, radians
    sinB[i]<-sin(lat)*sin(d[i])+cos(lat)*cos(d[i])*cos(h[i]) # Solar elevation angle, radians

    # Sun/Shade model
    # Leaf area index for sunlit and shaded of the canopy
    kb[i]<-ifelse(sinB[i]>0,0.5/sinB[i],0) # Beam radiation extinction coefficient of canopy
    LAIc<-LAI # Leaf area index of canopy, m2 m-2
    Lsun[i]<-ifelse(kb[i]==0,0,(1-exp(-kb[i]*LAIc))/kb[i]) # Sunlit leaf area index of canopy, m2 m-2
    Lsh[i]<-LAIc-Lsun[i] # Shaded leaf area index of canopy, m2 m-2

    # Canopy reflection coefficients
    lsc<-0.15 # Leaf scattering coefficient of PAR
    ph<-(1-sqrt(1-lsc))/(1+sqrt(1-lsc)) # Reflection coefficient of a canopy with horizontal leaves
    pcd<-0.036 # Canopy reflection coefficient for diffuse PAR
    I<-2083 # Total incident PAR, μmol m-2 ground s-1
    fd<-0.159 # Fraction of diffuse irradiance
    Ib0<-I*(1-fd) # Beam irradiance
    Id0<-I*fd # Diffuse irradiance
    kd<-0.719 # Diffuse and scattered diffuse PAR extinction coefficient

    kb1[i]<-kb[i]*sqrt(1-lsc)
    kb2[i]<-0.46/sinB[i] # Beam and scattered beam PAR extinction coefficient
    pcb[i]<-1-exp(-2*ph*kb[i]/(1+kb[i])) # Canopy reflection coefficient for beam PAR


    # Absorbed irradiance of sunlit and shaded fractions of canopies
    X[i]<-Ib0*(1-lsc)*(1-exp(-kb[i]*LAIc))*sinB[i] # Direct-beam irradiance absorbed by sunlit leaves
    Y[i]<-Id0*(1-pcd)*(1-exp(-(kb[i]+kb1[i])*Lsun[i]))*(kd/(kd+kb[i]))*sinB[i] # Diffuse irradiance absorbed by sunlit leaves
    ZZ[i]<-Ib0*((1-pcb[i])*(1-exp(-(kb1[i]+kb[i])*Lsun[i]))*kb1[i]/(kb1[i]+kb[i])-(1-lsc)*(1-exp(-2*kb[i]*Lsun[i]))/2)*sinB[i]
    Z[i]<-ifelse(is.na(ZZ[i]),0,ZZ[i]) # Scattered-beam irradiance absorbed by sunlit leaves
    Icsun[i]<-ifelse(kb[i]==0,0,X[i]+Y[i]+Z[i]) # Irradiance absorbed by the sunlit fraction of the canopy
    Ic[i]<-ifelse(kb[i]==0,0,((1-pcb[i])*Ib0*(1-exp(-kb1[i]*LAIc))+(1-pcd)*Id0*(1-exp(-kd*LAIc)))*sinB[i]) # Total irradiance absorbed, per unit ground area
    Icsh[i]<-Ic[i]-Icsun[i] # Irradiance absorbed by the shaded fractions of canopy

    # Irradiance dependence of electron transport of sunlit and shaded fractions of canopies
    bsh[i]<--(0.425*Icsh[i]+Jmax[i]) # Sum of PAR effectively absorbed by PSII and Jmax
    csh[i]<-Jmax[i]*0.425*Icsh[i]

    a<-0.7 # Curvature of leaf response of electron transport to irradiance
    Jsh1[i]<-(-bsh[i]+sqrt(bsh[i]^2-4*a*csh[i]))/(2*a) # Positive solution
    Jsh2[i]<-(-bsh[i]-sqrt(bsh[i]^2-4*a*csh[i]))/(2*a) # Negative solution

    bsunsh[i]<--(0.425*Ic[i]+Jmax[i])
    csunsh[i]<-0.425*Jmax[i]*Ic[i]
    Jsunsh1[i]<-(-bsunsh[i]+sqrt(bsunsh[i]^2-4*a*csunsh[i]))/(2*a) # Positive solution
    Jsunsh2[i]<-(-bsunsh[i]-sqrt(bsunsh[i]^2-4*a*csunsh[i]))/(2*a) # Negative solution

    Ratio1[i]<-ifelse(Lsun[i]==0,0,Jsh1[i]/Jsunsh1[i])

    Ratio[i]<-ifelse(Ratio1[i]>0.4,0.4,Ratio1[i])

    # Gross photosynthesis rates for sunlit and shaded leaves
    Vjsh1[i]<-0.2*Vj[i]
    Vjsh2[i]<-Vj[i]*Ratio[i]
    Vjsh[i]<-ifelse(Vjsh1[i]>Vjsh2[i],Vjsh1[i],Vjsh2[i]) # Light-limited gross photosynthesis rates of shaded leaves, μmol m-2 s-1
    Vjsun[i]<-(Vj[i]-Vjsh[i]) # Light-limited gross photosynthesis rates of sunlit leaves, μmol m-2 s-1
    VcVjsun[i]<-ifelse(Vc[i]<Vjsun[i],Vc[i],Vjsun[i]) # Gross photosynthesis rates of sunlit leaves, μmol m-2 s-1
    VcVjsh[i]<-ifelse(Vc[i]<Vjsh[i],Vc[i],Vjsh[i]) # Gross photosynthesis rates of shaded leaves, μmol m-2 s-1


    # Net CO2 assimilation rate for sunlit and shaded leaves
    Rd[i]<-Vm[i]*0.015 # Leaf dark respiration, μmol m-2 s-1
    Asun[i]<-ifelse(VcVjsun[i]-Rd[i]<0,0,VcVjsun[i]-Rd[i]) # Net CO2 assimilation rate of sunlit leaves, μmol m-2 s-1
    Ash[i]<-ifelse(VcVjsh[i]-Rd[i]<0,0,VcVjsh[i]-Rd[i]) # Net CO2 assimilation rate of shaded leaves, μmol m-2 s-1
    #Asunsh[i]<-ifelse(Vc[i]<Vj[i],Vc[i],Vj[i]) # Just for test

    # Net CO2 assimilation rate for sunlit and shaded canopy
    Acsun[i]<-Asun[i]*Lsun[i] # Net CO2 assimilation rate for sunlit canopy, μmol m-2 s-1
    Acsh[i]<-Ash[i]*Lsh[i] # Net CO2 assimilation rate for shaded canopy, μmol m-2 s-1
    Acanopy[i]<-Acsun[i]+Acsh[i] # Net CO2 assimilation rate for canopy, μmol m-2 s-1

    # Stomatal model
    m<-Input_parameter$m # Coefficient
    g0<-Input_parameter$g0 # Initial stomatal conductance, m mol m-2 s-1
    S_swc<-Input_parameter$SWCs # Saturated soil volumetric moisture content at depth of 30 cm, %
    W_swc<-Input_parameter$SWCw # Wilting soil volumetric moisture content at depth of 30 cm, %
    VPD_close<-Input_parameter$VPD_close # The VPD at stomatal closure
    VPD_open<-Input_parameter$VPD_open # The VPD at stomatal opening
    gs[i]<-(g0+m*RH[i]*Acanopy[i]/Ca[i]*(max(0,min((SWC[i]-W_swc)/(S_swc-W_swc),1)))*(max(0,min((VPD_close-VPD1[i])/(VPD_close-VPD_open),1)))) # Stomatal conductance at leaf level, m mol m-2 s-1

    # Net CO2 assimilation rate for canopy by Leuning (1990)
    AV[i]<-(gs[i]*22.4/(8.314*(Tem[i]+273))*(Ca[i]-Ci[i])/1.6)*LAI/2

    #CJ4[i]<-ifelse(kb[i]==0,0,1)
    #fsun[i]<-exp(-kb[i]*LAIc)*CJ4[i]
    #fsh[i]<-1-fsun[i]


    # Gross primary production
    GPPsec[i]<-12/1000000*(Acanopy[i]+Rd[i])*1 # g C m-2 s-1


    # Ecosystem respiration
    # Respiration parameters
    Mf<-Input_parameter$Mf # Biomass density of for leaf, kg C m-2 day-1
    Ms<-Input_parameter$Ms # Biomass density of for sapwood, kg C m-2 day-1
    Mr<-Input_parameter$Mr # Biomass density of for root, kg C m-2 day-1

    rmf<-Input_parameter$rmf # Maintenance respiration coefficient for leaf
    rms<-Input_parameter$rms # Maintenance respiration coefficient for stem
    rmr<-Input_parameter$rmr # Maintenance respiration coefficient for root

    rgf<-Input_parameter$rgf # Growth respiration coefficient for leaf
    rgs<-Input_parameter$rgs # Growth respiration coefficient for sapwood
    rgr<-Input_parameter$rgr # Growth respiration coefficient for root

    raf<-Input_parameter$raf # Carbon allocation fraction for leaf
    ras<-Input_parameter$ras # Carbon allocation fraction for sapwood
    rar<-Input_parameter$rar # Carbon allocation fraction for root

    Q10<-Input_parameter$Q10 # Temperature sensitivity factor
    Tref<-Input_parameter$Tref # Base temperature for Q10, ℃

    # Maintenance respiration
    Rmf[i]<-Mf*rmf*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for leaf, gC m-2 s-1
    Rms[i]<-Ms*rms*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for stem, gC m-2 s-1
    Rmr[i]<-Mr*rmr*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for root, gC m-2 s-1
    Rm[i]<-Rmf[i]+Rms[i]+Rmr[i] # Total maintenance respiration, gC m-2 s-1
    Rm30min[i]<-1000*Rm[i]/24/2 # Unit coversion, from gC m-2 30min-1

    # Growth respiration
    Rgf[i]<-rgf*raf*GPPsec[i] # Growth respiration for leaf, gC m-2 s-1
    Rgs[i]<-rgs*ras*GPPsec[i] # Growth respiration for sapwood, gC m-2 s-1
    Rgr[i]<-rgr*rar*GPPsec[i] # Growth respiration for root, gC m-2 s-1
    Rg[i]<-Rgf[i]+Rgs[i]+Rgr[i] # Total growth respiration, gC m-2 s-1
    Rg30min[i]<-1800*Rg[i] # Unit coversion, from gC m-2 30 min-1

    # Heterotrophic respiration
    R10<-1.5 # Soil respiration rate in 10 degrees Celsius
    Rh30min[i]<-(R10*Q10^((Tem[i]-10)/10))/48 # Heterotrophic respiration, gC m-2 30 min-1

    Re30min[i]<-Rh30min[i]+Rm30min[i]+Rg30min[i] # Ecosystem respiration, gC m-2 30 min-1

    # Carbon flux in ecosystem
    GPP30min[i]<-ifelse(PPFD[i]==0,0,1800*GPPsec[i]) # Gross primary production, gC m-2 30min-1
    NPP30min[i]<--(GPP30min[i]-Rm30min[i]-Rg30min[i]) # Net primary production, gC m-2 30min-1
    NEP30min[i]<-GPP30min[i]-Re30min[i] # Net ecosystem production, gC m-2 30min-1

    # Evapotranspiration by Penman–Monteith model
    Cp<-1.013*1000 # Specific heat of the air, J kg-1 degrees Celsius-1
    Ve<-Input_variable$Vms # The wind speed at height HV1, m s-1
    Pdensity<-rep(1.29,x) # Air density, kg m-3
    r<-0.66/10 # Psychrometric constant, kPa degrees Celsius-1
    HV1<-rep(Input_parameter$HV1,x) # The height at wind measurement, m
    Hcanopy<-rep(Input_parameter$hc,x) # The average canopy height, m
    k<-rep(0.41,x) # Von karman’s constant

    gsms[i]<-gs[i]*22.4*10^-6*LAI/2 # Stomatal conductance at canopy level, m s-1
    Z0M[i]<-0.1*Hcanopy[i] # Roughness height for momentum transfer, m
    Z0V[i]<-0.5*Z0M[i] # Roughness height for vapor and heat transfer, m
    d[i]<-0.7*Hcanopy[i] # Zero plane displacement height, m
    svt[i]<-4098*(0.6108*exp(17.27*Tem[i]/(Tem[i]+237.3)))/(Tem[i]+237.3)^2 # The slope of the saturation vapor pressure against temperature curve, kPa degrees Celsius-1
    ra[i]<-log((HV1[i]-d[i])/Z0M[i])*log((HV1[i]-d[i])/Z0V[i])/(k[i]^2*Ve[i]) # Aerodynamic resistance, s m-1
    LES[i]<-(svt[i]*(Rn[i]-G[i])+Pdensity[i]*Cp*VPD1[i]/ra[i])/(svt[i]+r*(1+1/gsms[i]/ra[i])) # Latent heat, w m-2
    ETS[i]<-0.43*LES[i]/(597-0.564*Tem[i]) # Evapotranspiration, mm 30 min-1

    #Iteration condition of TRIPLEX_CW_Flux: (abs(Acanopy[i]-AV[i])>1)|(Acanopy[i]>3)
    while((abs(Acanopy[i]-AV[i])>1)|(Acanopy[i]>3)){

      # Iteration
      j<-j+1
      Cippm[i]<-Cappm[i]-j # Setting initial value of intercellular CO2 concentration, assume:Cippm=Cappm
      Ca[i]<-Cappm[i]/1000000*1.013*100000 # Unit conversion: from ppm to Pa
      Ci[i]<-Cippm[i]/1000000*1.013*100000 # Unit conversion: from ppm to Pa

      Vm25<-Input_parameter$Vm25 # Maximum carboxylation rate at 25degrees Celsius, μmol m-2 s-1
      Rgas<-Input_parameter$Rgas # Molar gas constant, m3 Pa mol-1 K-1
      O2<-Input_parameter$O2 # Oxygen concentration in the atmosphere, Pa
      N<-Input_parameter$N # Leaf nitrogen content, %
      Nm<-Input_parameter$Nm # Maximum nitrogen content,%
      fN<-N/Nm # Nitrogen limitation term
      Error1[i]<-Ci[i]/Ca[i] # Just for check
      COp[i]<-1.92*10^-4*O2*1.75^((Tem[i]-25)/10) # CO2 concentration point without dark respiration, Pa
      Kc[i]<-30*2.1^((Tem[i]-25)/10) # Michaelis–Menten constants for CO2, Pa
      Ko[i]<-30000*1.2^((Tem[i]-25)/10) # Michaelis–Menten constants for O2, Pa
      K[i]<-Kc[i]*(1+O2/Ko[i]) # Function of enzyme kinetics, Pa
      fT[i]<-(1+exp((-220000+710*(Tem[i]+273))/(Rgas*(Tem[i]+273))))^-1 # Temperature limitation term
      Vm[i]<-Vm25*2.4^((Tem[i]-25)/10)*fT[i]*fN # Maximum carboxylation rate, μmol m-2 s-1
      Vc[i]<-Vm[i]*(Ci[i]-COp[i])/(Ci[i]+K[i]) # Rubisco-limited gross photosynthesis rates, μmol m-2 s-1

      # Light-limited gross photosynthesis rates for big leaf
      Jmax[i]<-29.1+1.64*Vm[i] # Light-saturated rate of electron transport in the photosynthetic carbon reduction cycle in leaf cells, μmol m-2 s-1
      J[i]<-Jmax[i]*PPFD[i]/(PPFD[i]+2.1*Jmax[i]) # Electron transport rate, μmol m-2 s-1
      Vj1[i]<-J[i]*(Ci[i]-COp[i])/(4.5*Ci[i]+10.5*COp[i])
      Vj[i]<-ifelse(Vj1[i]>0,Vj1[i],0) # Light-limited gross photosynthesis rates, μmol m-2 s-1
      # Vj[i]<-max(J[i]*(Ci[i]-COp[i])/(4.5*Ci[i]+10.5*COp[i]),0) # Light-limited gross photosynthesis rates, μmol m-2 s-1

      # Solar geometry
      Ls<-Input_parameter$Ls # Standard longitude of time zone
      Le<-Input_parameter$Le # Local longitude, degree
      latitude<-Input_parameter$latitude # Local latitude, degree
      lat<-latitude*3.14/180 # Convert angle to radian, radian
      LAI<-Input_parameter$LAI # Leaf area index, m2 m-2
      Ld[i]<-2*3.14*(Day[i]-1)/365 # Day angle
      Et[i]<-0.017+0.4281*cos(Ld[i])-7.351*sin(Ld[i])-3.349*cos(2*Ld[i])-9.731*sin(Ld[i]) # Equation of time, min
      to[i]<-12+(4*(Ls-Le)-Et[i])/60 # Solar noon, h
      h[i]<-3.14*(time[i]-to[i])/12 # Hour angle of sun, radians
      d[i]<--23.4*3.14/180*cos(2*3.14*(Day[i]+10)/365) # Solar declination angle, radians
      sinB[i]<-sin(lat)*sin(d[i])+cos(lat)*cos(d[i])*cos(h[i]) # Solar elevation angle, radians

      # Sun/Shade model
      # Leaf area index for sunlit and shaded of the canopy
      kb[i]<-ifelse(sinB[i]>0,0.5/sinB[i],0) # Beam radiation extinction coefficient of canopy
      LAIc<-LAI # Leaf area index of canopy, m2 m-2
      Lsun[i]<-ifelse(kb[i]==0,0,(1-exp(-kb[i]*LAIc))/kb[i]) # Sunlit leaf area index of canopy, m2 m-2
      Lsh[i]<-LAIc-Lsun[i] # Shaded leaf area index of canopy, m2 m-2

      # Canopy reflection coefficients
      lsc<-0.15 # Leaf scattering coefficient of PAR
      ph<-(1-sqrt(1-lsc))/(1+sqrt(1-lsc)) # Reflection coefficient of a canopy with horizontal leaves
      pcd<-0.036 # Canopy reflection coefficient for diffuse PAR
      I<-2083 # Total incident PAR μmol m-2 ground s-1
      fd<-0.159 # Fraction of diffuse irradiance
      Ib0<-I*(1-fd) # Beam irradiance
      Id0<-I*fd # Diffuse irradiance
      kd<-0.719 # Diffuse and scattered diffuse PAR extinction coefficient

      kb1[i]<-kb[i]*sqrt(1-lsc)
      kb2[i]<-0.46/sinB[i] # Beam and scattered beam PAR extinction coefficient
      pcb[i]<-1-exp(-2*ph*kb[i]/(1+kb[i])) # Canopy reflection coefficient for beam PAR


      # Absorbed irradiance of sunlit and shaded fractions of canopies
      X[i]<-Ib0*(1-lsc)*(1-exp(-kb[i]*LAIc))*sinB[i] # Direct-beam irradiance absorbed by sunlit leaves
      Y[i]<-Id0*(1-pcd)*(1-exp(-(kb[i]+kb1[i])*Lsun[i]))*(kd/(kd+kb[i]))*sinB[i] # Diffuse irradiance absorbed by sunlit leaves
      ZZ[i]<-Ib0*((1-pcb[i])*(1-exp(-(kb1[i]+kb[i])*Lsun[i]))*kb1[i]/(kb1[i]+kb[i])-(1-lsc)*(1-exp(-2*kb[i]*Lsun[i]))/2)*sinB[i]
      Z[i]<-ifelse(is.na(ZZ[i]),0,ZZ[i]) # Scattered-beam irradiance absorbed by sunlit leaves
      Icsun[i]<-ifelse(kb[i]==0,0,X[i]+Y[i]+Z[i]) # Irradiance absorbed by the sunlit fraction of the canopy
      Ic[i]<-ifelse(kb[i]==0,0,((1-pcb[i])*Ib0*(1-exp(-kb1[i]*LAIc))+(1-pcd)*Id0*(1-exp(-kd*LAIc)))*sinB[i]) # Total irratiance absorbed
      Icsh[i]<-Ic[i]-Icsun[i] # Irradiance absorbed by the shaded fractions of canopy

      # Irradicance dependence of electron transport of sunlit and shaded fractions of canopies
      bsh[i]<--(0.425*Icsh[i]+Jmax[i]) # Sum of PAR effectively absorbed by PSII and Jmax
      csh[i]<-Jmax[i]*0.425*Icsh[i]

      a<-0.7 # Curvature of leaf response of electron transport to irradiance
      Jsh1[i]<-(-bsh[i]+sqrt(bsh[i]^2-4*a*csh[i]))/(2*a) # Positive solution
      Jsh2[i]<-(-bsh[i]-sqrt(bsh[i]^2-4*a*csh[i]))/(2*a) # Negetive solution

      bsunsh[i]<--(0.425*Ic[i]+Jmax[i])
      csunsh[i]<-0.425*Jmax[i]*Ic[i]
      Jsunsh1[i]<-(-bsunsh[i]+sqrt(bsunsh[i]^2-4*a*csunsh[i]))/(2*a) # Positive solution
      Jsunsh2[i]<-(-bsunsh[i]-sqrt(bsunsh[i]^2-4*a*csunsh[i]))/(2*a) # Negetive solution

      Ratio1[i]<-ifelse(Lsun[i]==0,0,Jsh1[i]/Jsunsh1[i])

      Ratio[i]<-ifelse(Ratio1[i]>0.4,0.4,Ratio1[i])

      # Gross photosynthesis rates for sunlit and shaded leaves
      Vjsh1[i]<-0.2*Vj[i]#
      Vjsh2[i]<-Vj[i]*Ratio[i]#
      Vjsh[i]<-ifelse(Vjsh1[i]>Vjsh2[i],Vjsh1[i],Vjsh2[i]) # Light-limited gross photosynthesis rates of shaded leaves, umol m-2 s-1
      Vjsun[i]<-(Vj[i]-Vjsh[i]) # Light-limited gross photosynthesis rates of sunlit leaves, μmol m-2 s-1
      VcVjsun[i]<-ifelse(Vc[i]<Vjsun[i],Vc[i],Vjsun[i]) # Gross photosynthesis rates of sunlit leaves, μmol m-2 s-1
      VcVjsh[i]<-ifelse(Vc[i]<Vjsh[i],Vc[i],Vjsh[i]) # Gross photosynthesis rates of shaded leaves, μmol m-2 s-1


      # Net CO2 assimilation rate for sunlit and shaed leaves
      Rd[i]<-Vm[i]*0.015 # Leaf dark respiration, μmol m-2 s-1
      Asun[i]<-ifelse(VcVjsun[i]-Rd[i]<0,0,VcVjsun[i]-Rd[i]) # Net CO2 assimilation rate of sunlit leaves, μmol m-2 s-1
      Ash[i]<-ifelse(VcVjsh[i]-Rd[i]<0,0,VcVjsh[i]-Rd[i]) # Net CO2 assimilation rate of shaded leaves, μmol m-2 s-1
      #Asunsh[i]<-ifelse(Vc[i]<Vj[i],Vc[i],Vj[i]) # Just for test

      # Net CO2 assimilation rate for sunlit and shaded canopy
      Acsun[i]<-Asun[i]*Lsun[i] # Net CO2 assimilation rate for sunlit canopy, μmol m-2 s-1
      Acsh[i]<-Ash[i]*Lsh[i] # Net CO2 assimilation rate for shaded canopy, μmol m-2 s-1
      Acanopy[i]<-Acsun[i]+Acsh[i] # Net CO2 assimilation rate for canopy, μmol m-2 s-1

      # Stomatal model
      m<-Input_parameter$m # Coefficient
      g0<-Input_parameter$g0 # Initial stomatal conductance, m mol m-2 s-1
      S_swc<-Input_parameter$SWCs # Saturated soil volumetric moisture content at depth of 30 cm, %
      W_swc<-Input_parameter$SWCw # Wilting soil volumetric moisture content at depth of 30 cm, %
      VPD_close<-Input_parameter$VPD_close # The VPD at stomatal closure
      VPD_open<-Input_parameter$VPD_open # The VPD at stomatal opening
      gs[i]<-(g0+m*RH[i]*Acanopy[i]/Ca[i]*(max(0,min((SWC[i]-W_swc)/(S_swc-W_swc),1)))*(max(0,min((VPD_close-VPD1[i])/(VPD_close-VPD_open),1)))) # Stomatal conductance at leaf level, m mol m-2 s-1

      # Net CO2 assimilation rate for canopy by Leuning (1990)
      AV[i]<-(gs[i]*22.4/(8.314*(Tem[i]+273))*(Ca[i]-Ci[i])/1.6)*LAI/2

      #CJ4[i]<-ifelse(kb[i]==0,0,1)#
      #fsun[i]<-exp(-kb[i]*LAIc)*CJ4[i]#
      #fsh[i]<-1-fsun[i]#

      # Gross primary production
      GPPsec[i]<-12/1000000*(Acanopy[i]+Rd[i])*1# g C m-2 s-1

      # Ecosystem respiration
      # Respiration parameters
      Mf<-Input_parameter$Mf # Biomass density of for leaf, kg C m-2 day-1
      Ms<-Input_parameter$Ms # Biomass density of for sapwood, kg C m-2 day-1
      Mr<-Input_parameter$Mr # Biomass density of for root, kg C m-2 day-1

      rmf<-Input_parameter$rmf # Maintenance respiration coefficient for leaf
      rms<-Input_parameter$rms # Maintenance respiration coefficient for stem
      rmr<-Input_parameter$rmr # Maintenance respiration coefficient for root

      rgf<-Input_parameter$rgf # Growth respiration coefficient for leaf
      rgs<-Input_parameter$rgs # Growth respiration coefficient for sapwood
      rgr<-Input_parameter$rgr # Growth respiration coefficient for root

      raf<-Input_parameter$raf # Carbon allocation fraction for leaf
      ras<-Input_parameter$ras # Carbon allocation fraction for sapwood
      rar<-Input_parameter$rar # Carbon allocation fraction for root

      Q10<-Input_parameter$Q10 # Temperature sensitivity factor
      Tref<-Input_parameter$Tref # Base temperature for Q10

      # Maintenance respiration
      Rmf[i]<-Mf*rmf*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for leaf, gC m-2 s-1
      Rms[i]<-Ms*rms*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for stem, gC m-2 s-1
      Rmr[i]<-Mr*rmr*Q10^((Tem[i]-Tref)/10) # Maintenance respiration for root, gC m-2 s-1
      Rm[i]<-Rmf[i]+Rms[i]+Rmr[i] # Total maintenance respiration, gC m-2 s-1
      Rm30min[i]<-1000*Rm[i]/24/2 # Unit coversion, from gC m-2 30 min-1

      # Growth respiration
      Rgf[i]<-rgf*raf*GPPsec[i] # Growth respiration for leaf, gC m-2 s-1
      Rgs[i]<-rgs*ras*GPPsec[i] # Growth respiration for sapwood, gC m-2 s-1
      Rgr[i]<-rgr*rar*GPPsec[i] # Growth respiration for root, gC m-2 s-1
      Rg[i]<-Rgf[i]+Rgs[i]+Rgr[i] # Total growth respiration, gC m-2 s-1
      Rg30min[i]<-1800*Rg[i] # Unit coversion, from gC m-2 30 min-1

      # Heterotrophic respiration
      R10<-1.5# Soil respiration rate in 10 degrees Celsius
      Rh30min[i]<-(R10*Q10^((Tem[i]-10)/10))/48 # Heterotrophic respiration, gC m-2 30 min-1

      Re30min[i]<-Rh30min[i]+Rm30min[i]+Rg30min[i] # Ecosystem respiration, gC m-2 30 min-1

      # Carbon flux in ecosystem
      GPP30min[i]<-ifelse(PPFD[i]==0,0,1800*GPPsec[i]) # Gross primary production, gC m-2 30 min-1
      NPP30min[i]<--(GPP30min[i]-Rm30min[i]-Rg30min[i]) # Net primary production, gC m-2 30 min-1
      NEP30min[i]<-GPP30min[i]-Re30min[i] # Net ecosystem production, gC m-2 30 min-1

      # Evapotranspiration by Penman–Monteith model
      Cp<-1.013*1000 # Specific heat of the air, J kg-1 degrees Celsius-1
      Ve<-Input_variable$Vms # The wind speed at height HV1, m s-1
      Pdensity<-rep(1.29,x) # Air density, kg m-3
      r<-0.66/10 # Psychrometric constant, kPa ℃-1
      HV1<-rep(Input_parameter$HV1,x) # The height at wind measurement, m
      Hcanopy<-rep(Input_parameter$hc,x) # The average canopy height, m
      k<-rep(0.41,x) # Von karman’s constant

      gsms[i]<-gs[i]*22.4*10^-6*LAI/2 # Stomatal conductance at canopy level, m s-1
      Z0M[i]<-0.1*Hcanopy[i] # Roughness height for momentum transfer, m
      Z0V[i]<-0.5*Z0M[i] # Roughness height for vapor and heat transfer, m
      d[i]<-0.7*Hcanopy[i] # Zero plane displacement height, m
      svt[i]<-4098*(0.6108*exp(17.27*Tem[i]/(Tem[i]+237.3)))/(Tem[i]+237.3)^2 # The slope of the saturation vapor pressure against temperature curve, kPa degrees Celsius-1
      ra[i]<-log((HV1[i]-d[i])/Z0M[i])*log((HV1[i]-d[i])/Z0V[i])/(k[i]^2*Ve[i]) # Aerodynamic resistance, s m-1
      LES[i]<-(svt[i]*(Rn[i]-G[i])+Pdensity[i]*Cp*VPD1[i]/ra[i])/(svt[i]+r*(1+1/gsms[i]/ra[i])) # Latent heat, w m-2
      ETS[i]<-0.43*LES[i]/(597-0.564*Tem[i]) # Evapotranspiration, mm 30 min-1

      # Output condition of TRIPLEX_CW_Flux model: ((abs(Acanopy[i]-AV[i])<1)|(Acanopy[i]<3))
      if ((abs(Acanopy[i]-AV[i])<1)|(Acanopy[i]<3)){
        break
      }
    }
  }
  result<-data.frame(Input_variable,NEP30min,ETS,GPP30min,Re30min)
  #View(result)

  # Draw the graph of Simulated NEP and ET
  dev.new(title = "Simulated results of NEP and ET", width=6400,height=3200,
          noRStudioGD = TRUE)
  par(mfrow=c(1,2))
  par(oma=c(0,0,1,1),mar=c(4.5,5,1,0.5))
  plot(result$NEP30min~result$ObserveNEE30,pch=21,las=1,
       cex.axis=1.5,cex.lab=1.5,font=2,cex=3 ,mgp=c(3, 0.5, 0), col="blue",lwd=3,tck=-0.01,
       xlab=expression(Observed~NEP~(g~C~m^-2~30*min^-1)),
       ylab=expression(Simulated~NEP~(g~C~m^-2~30*min^-1)),
       xlim=c(min(result$NEP30min,result$ObserveNEE30)-0.2,
              max(result$NEP30min,result$ObserveNEE30)+0.2),
       ylim=c(min(result$NEP30min,result$ObserveNEE30)-0.2,
              max(result$NEP30min,result$ObserveNEE30)+0.2))
  box(lwd=3)
  lm.sol1<-lm(result$NEP30min~result$ObserveNEE30)
  summary(lm.sol1)
  abline(lm.sol1, lwd=3, col="red")
  par(new=T)
  curve(x+0, -100,100,bty="l", col="grey60",add=T,lty=2,lwd=4)
  xNEEmin<-min(result$NEP30min,result$ObserveNEE30)
  yNEEmax<-max(result$NEP30min,result$ObserveNEE30)
  text(xNEEmin-0.2+0.01,yNEEmax+0.2-0.03, expression(R^2), cex=2, adj=0)
  text(xNEEmin-0.2+0.18,yNEEmax+0.2-0.03,"=", adj=0,cex=2)
  text(xNEEmin-0.2+0.25,yNEEmax+0.2-0.03,round(summary(lm.sol1)$r.squared,2),
       adj=0,cex=2)
  text(xNEEmin-0.2+0.01,yNEEmax+0.2-0.2, "RMSE=", cex=2, adj=0)
  text(xNEEmin-0.2+0.45,yNEEmax+0.2-0.2,
       round(sqrt(sum(residuals(lm.sol1)^2)/(nrow(result)-2)),2), adj=0,cex=2)

  plot(result$ETS~result$OETS,pch=21,las=1,cex.axis=1.5, cex.lab=1.5,
       xlab=expression(Observed~ET~(mm~30*min^-1)),
       ylab=expression(Simulated~ET~(mm~30*min^-1)),
       xlim=c(min(result$OETS,result$ETS)-0.1,max(result$OETS,result$ETS)+0.1),
       ylim=c(min(result$OETS,result$ETS)-0.1,max(result$OETS,result$ETS)+0.1),
       mgp=c(3, 0.5, 0),col="blue",lwd=3,font=2,cex=3,tck=-0.01)
  lm.sol2<-lm(result$ETS~result$OETS)
  summary(lm.sol2)
  abline(lm.sol2, lwd=3, col="red")
  par(new=T)
  curve(x+0, -100,100,bty="l", col="grey60",add=T,lty=2,lwd=4)
  box(lwd=3)
  xETmin<-min(result$OETS,result$ETS)
  yETmax<-max(result$OETS,result$ETS)
  text(xETmin-0.1+0.01,yETmax+0.1-0.01, expression(R^2), cex=2, adj=0)
  text(xETmin-0.1+0.08,yETmax+0.1-0.01,"=", adj=0,cex=2)
  text(xETmin-0.1+0.15,yETmax+0.1-0.01,
       round(summary(lm.sol2)$r.squared,2),adj=0,cex=2)
  text(xETmin-0.1+0.01,yETmax+0.1-0.08, "RMSE=", cex=2, adj=0)
  text(xETmin-0.1+0.23,yETmax+0.1-0.08,
       round(sqrt(sum(residuals(lm.sol2)^2)/(nrow(result)-2)),2), adj=0,cex=2)

  yeardata<-function(){

    # Divided into four seasons for further analysis
    result$season<-ifelse(result$Month>=3&result$Month<=5,
                                  "Spring",
                                  ifelse(result$Month>=6&
                                           result$Month<=8,"Summer",
                                         ifelse(result$Month>=9&
                                                  result$Month<=11,
                                                "Autumn","Winter")))

    result$seasonnum<-ifelse(result$Month>=3&
                                       result$Month<=5,1,
                                     ifelse(result$Month>=6&
                                              result$Month<=8,2,
                                            ifelse(result$Month>=9&
                                                     result$Month<=11
                                                   ,3,4)))

    # Draw the graph of Simulated ET results in four seasons
    dev.new(title = "Simulated results of seasonal ET",
            width=10000,height=10000, noRStudioGD = TRUE)
    par(mfrow=c(2,2))
    par(oma=c(3,5,1,1),mar=c(5.5,6,1,1))
    Season2<-c("Spring","Summer","Autumn","Winter")
    Seasequen2<-c("(a)","(b)","(c)","(d)")
    Season2<-data.frame(Season2)
    Seasequen2<-data.frame(Seasequen2)
    R2<-rep(0,4);RMSE<-rep(0,4);intercept<-rep(0,4);slope<-rep(0,4);N<-rep(0,4)
    par(ask=F)
    sum(result$OETS)
    sum(result$ETS)
    for(i in 1:4){
      subdata<-subset(result,result$seasonnum==i)
      lmmatri<-lm(subdata$ETS~subdata$OETS)
      R2[i]<-summary(lmmatri)$r.squared
      N[i]<-nrow(subdata)
      RMSE[i]<-sqrt(sum(residuals(lmmatri)^2)/(N[i]-2))
      intercept[i]<-lmmatri[[1]][1]
      slope[i]<-lmmatri[[1]][2]
      plot(subdata$ETS~subdata$OETS,pch=21,las=1,cex.axis=3,xlab="",tck=-0.01,
           cex.lab=2,font=2,cex=2,ylab="", mgp=c(3, 1.5, 0),col="blue",lwd=3,
           xlim=c(min(result$OETS,result$ETS)-0.1,max(result$OETS,result$ETS)+0.1),
           ylim=c(min(result$OETS,result$ETS)-0.1,max(result$OETS,result$ETS)+0.1))
      curve(x+0, -100,100,bty="l", col="grey60",add=T,lty=2,lwd=4)
      box(lwd=3)
      mtext(expression(Observed~ET~(mm~30*min^-1)),side = 1,
            line = 1.5,cex=3,outer = T, font=2)
      mtext(expression(Simulated~ET~(mm~30*min^-1)),side = 2,
            line = 1,cex=3,outer = T, font=2)
      xSeasonalETmin<-min(result$OETS,result$ETS)
      ySeasonalETmax<-max(result$OETS,result$ETS)
      text(xSeasonalETmin-0.1+0.01,ySeasonalETmax+0.1-0.03,Seasequen2[i,1],
           font=2,adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.08,ySeasonalETmax+0.1-0.03,Season2[i,1],
           font=2,adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.01,ySeasonalETmax+0.1-0.1,
           expression(R^2), adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.06,ySeasonalETmax+0.1-0.1,"=",
           adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.1,ySeasonalETmax+0.1-0.1,
           round(R2[i],2),adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.01,ySeasonalETmax+0.1-0.16,
           "RMSE=", adj=0,cex=3)
      text(xSeasonalETmin-0.1+0.18,ySeasonalETmax+0.1-0.16,
           round(RMSE[i],2), adj=0,cex=3)
    }

    # Draw the graph of Simulated NEP results in four seasons
    dev.new(title = "Simulated results of seasonal NEP",
            width=10000,height=10000,noRStudioGD = TRUE)
    par(mfrow=c(2,2))
    par(oma=c(3,5,1,1),mar=c(5.5,6,1,1))
    Season1<-c("Spring","Summer","Autumn","Winter")
    Seasequen1<-c("(a)","(b)","(c)","(d)")
    Season1<-data.frame(Season1)
    Seasequen1<-data.frame(Seasequen1)
    R2<-rep(0,4);RMSE<-rep(0,4);intercept<-rep(0,4);slope<-rep(0,4);N<-rep(0,4)
    par(ask=F)
    for(i in 1:4){
      subdata<-subset(result,result$seasonnum==i)
      lmmatri<-lm(subdata$NEP30min~subdata$ObserveNEE30)
      R2[i]<-summary(lmmatri)$r.squared
      N[i]<-nrow(subdata)
      RMSE[i]<-sqrt(sum(residuals(lmmatri)^2)/(N[i]-2))
      intercept[i]<-lmmatri[[1]][1]
      slope[i]<-lmmatri[[1]][2]
      plot(subdata$NEP30min~subdata$ObserveNEE30,pch=21,las=1,cex.axis=3,xlab="",
           cex.lab=2,font=2,cex=2,ylab="", col="blue",
           lwd=3,tck=-0.01, mgp=c(3, 1.5, 0),
           xlim=c(min(result$NEP30min,result$ObserveNEE30)-0.2,
                  max(result$NEP30min,result$ObserveNEE30)+0.2),
           ylim=c(min(result$NEP30min,result$ObserveNEE30)-0.2,
                  max(result$NEP30min,result$ObserveNEE30)+0.2))

      curve(x+0, -100,100,bty="l", col="grey60",add=T,lty=2,lwd=4)
      box(lwd=3)
      mtext(expression(Observed~NEP~(g~C~m^-2~30*min^-1)),side = 1,line = 1.5,
            cex=3,outer = T, font=2)
      mtext(expression(Simulated~NEP~(g~C~m^-2~30*min^-1)),side = 2,line = 1,
            cex=3,outer = T, font=2)
      xSeasonalNEPmin<-min(result$NEP30min,result$ObserveNEE30)
      ySeasonalNEPmax<-max(result$NEP30min,result$ObserveNEE30)
      text(xSeasonalNEPmin-0.2+0.01,ySeasonalNEPmax+0.2-0.03,
           Seasequen1[i,1], font=2,adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.2,ySeasonalNEPmax+0.2-0.03,
           Season1[i,1], font=2,adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.01,ySeasonalNEPmax+0.2-0.15,
           expression(R^2), adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.12,ySeasonalNEPmax+0.2-0.15,
           "=", adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.2,ySeasonalNEPmax+0.2-0.15,
           round(R2[i],2),adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.01,ySeasonalNEPmax+0.2-0.25,
           "RMSE=", adj=0,cex=3)
      text(xSeasonalNEPmin-0.2+0.35,ySeasonalNEPmax+0.2-0.25,
           round(RMSE[i],2), adj=0,cex=3)
    }

    # Draw the graph of diurnal dynamics for observed and simulated NEP
    dev.new(title = "Diurnal dynamics of observed and simulated NEP",
            width=22000,height=25000, noRStudioGD = TRUE)
    par(mfrow=c(6,2))
    par(oma=c(5,6,2,4),mar=c(5.5,5.5,3,1))
    month2<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    sequen1<-c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)")
    month2<-data.frame(month2)
    sequen1<-data.frame(sequen1)
    for(i in 1:12){
      subdata<-subset(result,result$Month==i)
      length(subdata$OETS)
      index1<-(1:length(subdata$OETS))
      lab1<-seq(min(subdata$DOY),max(subdata$DOY),2)
      time1<-index1/48
      par(mar=c(3,5,1,1))
      plot(subdata$ETS~time1,type="l",las=1,cex.axis=3,xlab="",cex.lab=3,
           ylab="",col="red",lwd=4,xaxt="n",
           ylim=c((min(subdata$OETS)-0.05),
                  (max(subdata$OETS)+0.15)))#yaxt="n",mgp=c(5, 1, 0),
      box(lwd=3)
      mtext("Day of the year",side = 1,line =2,cex=3,outer = T, font=2)
      mtext(expression(ET~(mm~30*min^-1)),
            side = 2,line = 1,cex=3,outer = T,font=2)
      points(subdata$OETS~time1,pch=16,col="black",cex=3)
      text(1.5,max(subdata$OETS)+0.08,month2[i,1], adj=0,cex=4)
      text(0,max(subdata$OETS)+0.08,sequen1[i,1], adj=0,cex=4)
      axis(1,at=seq(0,max(time1)-1,2),tck=0.01,
           labels = lab1,cex.axis=3,tick = T)#
    }

    # Draw the graph of diurnal dynamics for observed and simulated ET
    dev.new(title = "Diurnal dynamics of observed and simulated ET",
            width=22000,height=25000, noRStudioGD = TRUE)
    par(mfrow=c(6,2))
    par(oma=c(5,6,2,4),mar=c(5.5,5.5,3,1))
    month2<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    sequen1<-c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)")
    month2<-data.frame(month2)
    sequen1<-data.frame(sequen1)
    for(i in 1:12){
      subdata<-subset(result,result$Month==i)
      length(subdata$ObserveNEE30)
      index1<-(1:length(subdata$ObserveNEE30))
      lab1<-seq(min(subdata$DOY),max(subdata$DOY),2)
      time1<-index1/48
      par(mar=c(3,5,1,1))
      plot(subdata$NEP30min~time1,type="l",las=1,cex.axis=3,xlab="",cex.lab=3,
           ylab="",col="red",lwd=4,xaxt="n",
           ylim=c((min(subdata$ObserveNEE30)-0.05),
                  (max(subdata$ObserveNEE30)+0.15)))#yaxt="n",mgp=c(5, 1, 0),
      box(lwd=3)
      mtext("Day of the year",side = 1,line =2,cex=3,outer = T, font=2)
      mtext(expression(NEP~(g~C~m^-2~30*min^-1)),
            side = 2,line = 1,cex=3,outer = T,font=2)
      points(subdata$ObserveNEE30~time1,pch=16,col="black",cex=3)
      text(1.5,max(subdata$ObserveNEE30)+0.08,month2[i,1], adj=0,cex=4)
      text(0,max(subdata$ObserveNEE30)+0.08,sequen1[i,1], adj=0,cex=4)
      axis(1,at=seq(0,max(time1)-1,2),tck=0.01,
           labels = lab1,cex.axis=3,tick = T)#
    }

    # Draw the graph of environmental factors during the studied period
    dev.new(title = "Environmental factors",
            width = 15, height = 5, noRStudioGD = TRUE)
    par(mfrow=c(3,1))
    par(oma=c(3,2,1,2))
    par(mar=c(5,7,2,7))

    par(mar=c(2,6,1,5)) # Rainfall & Ta
    Ta_mon<-as.data.frame(aggregate(Input_variable$Ta,
                                    list(Input_variable$Month),mean,na.rm=T))

    rainfall_mon<-as.data.frame(aggregate(Input_variable$Rainfall,
                                          list(Input_variable$Month),sum,na.rm=T))
    rainfall_mon<-rainfall_mon[,-1]
    rainfall_mon<-t(rainfall_mon)
    barplot(rainfall_mon,ylim = c(0,max(rainfall_mon+20)),cex.axis = 2,ylab = "",
            space = 0,las=1,col="gray",cex.lab=2,font=2)
    box(lwd=3)
    mtext("Rainfall (mm)",side = 2,line = 4,cex=2,outer = F, font=1)
    axis(1,at=seq(1.5,11.5,2),labels =seq(2,12,2),tick = F,cex.axis=2)
    legend("topleft",legend ="Rainfall",fill="gray",bty="n",horiz=F,cex=2.5)

    par(new=T)
    Ta_mon$Ta_x<-seq(0.5,11.5,1)
    plot(Ta_mon[,2]~Ta_mon[,1],las=1,cex.axis=2,xlab="",cex.lab=3,font=2,cex=2,
         ylab="",xaxt="n",yaxt="n",pch=20,type='b',lty=3,
         ylim=c(0,max(Ta_mon[,2]+5)),col="black",lwd=3)
    axis(side = 4,las=1,mgp=c(3, 1, 0),font=2,cex.axis=2)

    mtext("Temperature",side = 4,line = 4,cex=2,outer = F, font=1)
    legend("topright","Ta",pch = 20,lty = 3,col = "black",bg="black",
           cex=2.5,bty="n",horiz = T)

    par(mar=c(2,6,1,5)) # VPD&SWC
    VPD_mon<-as.data.frame(aggregate(Input_variable$VPDhpa/10,
                                     list(Input_variable$Month),mean,na.rm=T))
    SWC_mon<-as.data.frame(aggregate(Input_variable$SVWC30cm,
                                     list(Input_variable$Month),mean,na.rm=T))

    plot(VPD_mon[,2]~VPD_mon[,1],las=1,cex.axis=2,xlab="",cex.lab=3,font=2,cex=2,
         ylab="",pch=8,type='b',lty=3,ylim=c(0,max(VPD_mon[,2]+0.5)),
         col="black",lwd=3,bg="black")
    mtext("VPD (kPa)",side = 2,line = 4,cex=2,outer = F, font=1)

    par(new=T)
    plot(SWC_mon[,2]~SWC_mon[,1],las=1,cex.axis=2,xlab="",cex.lab=3,font=2,cex=2,
         ylab="",xaxt="n",yaxt="n",pch=1,type='b',lty=1,
         ylim=c(0,max(SWC_mon[,2]+10)),col="black",lwd=3,bg="black")
    axis(side = 4,las=1,mgp=c(3, 1, 0),font=2,cex.axis=2)
    box(lwd=3)
    mtext("SWC (%)",side = 4,line = 4,cex=2,outer = F, font=1)
    legend("topleft",c("VPD","SWC"),pch = c(8,1),lty = c(3,1),
           col = c("black","black"),cex=2.5,bty="n",horiz = T)

    par(mar=c(2,6,1,5)) # Rn
    Rn_mon<-as.data.frame(aggregate(Input_variable$Rn,
                                    list(Input_variable$Month),
                                    mean,na.rm=T))
    plot(Rn_mon[,2]~Rn_mon[,1],las=1,cex.axis=2,xlab="",cex.lab=3,font=2,cex=2,
         ylab="",pch=17,type='b',lty=3,ylim=c(0,max(Rn_mon[,2]+20)),
         col="black",lwd=3,bg="black")
    box(lwd=3)
    mtext(expression(Rn~(W~m^-2)),side = 2,line = 3,cex=2,outer = F, font=1)
    legend("topleft","Rn",pch = 17,lty = 3,col = "black",
           cex=2.5,bty="n",horiz = T)
    mtext("Month",side = 1,line = 1.5,cex=2,outer =T, font=1)

  }

  # Whether to input data for more than one year
  if(overyear==TRUE){
    yeardata()
  }

  return(result)
}

