rad2degree <- function(angle){
  return(180*angle/pi)
}
degree2rad <- function(angle){
  return(pi*angle/180)
}
calculaAngulo <- function(fp, adiantado){
  phi <- 0
  if(adiantado){
    phi <- -acos(fp)
  } else {
    phi <- acos(fp)
  }
  return(phi)
}

V1 = list(S = 110000, V1 = 4400, V2 = 440, f =60,
          R1 = 2.5, X1 = 3.7417, GM = (0.061983/1000), BM = (0.45030/1000),
          R2 = 0.025, X2 = 0.037417, fp= 0.6, adiantado = TRUE)

solution <- function(dados){
  
  sbase <- 3*dados$S
  vlinhaP <- dados$V1
  vfaseP <- dados$V1
  vlinhaS <- sqrt(3)*dados$V2
  vfaseS <- dados$V2
  
  vbasea <- vlinhaP
  vbaseb <- vlinhaS
  ibasea <- sbase/vbasea
  ibaseb <- sbase/vbaseb
  a <- dados$V1/dados$V2
  z <- complex(real = (dados$R1 + (a^2)*dados$R2), imaginary = (dados$X1+ (a^2)*dados$X2))
  ie <- complex(modulus = (sbase/(a*vlinhaS)), argument = -calculaAngulo(dados$fp, dados$adiantado))
  ic <- ie/a
  vc <- a*vlinhaS
  ve <- z*ie + vc
  se <- Mod(ve)*Mod(ie)
  vepu <- ve/vbasea
  sepu <- se/sbase
  vcpu <- vc/vbasea
  re <- (Mod(vepu)-Mod(vcpu))/Mod(vcpu)
  pepu <- sepu*cos(Arg(ve)-Arg(ie))
  pcpu <- 1*dados$fp
  n <- pcpu/pepu
  
  solution <- list(sbase=sbase, vlinhaP=vlinhaP, vlinhaS=vlinhaS, vfaseS=vfaseS, vfaseP=vfaseP, a=a,
                   vbasea=vbasea, vbaseb=vbaseb,ibasea=ibasea, ibaseb=ibaseb, z=z,
                   ie = ie, vc=vc, ve=ve,se=se, ic=ic, vepu=vepu, sepu=sepu,re=re,n=n)
  
  return(solution)
}