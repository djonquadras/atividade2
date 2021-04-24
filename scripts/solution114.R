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
  
  sbase <- dados$S
  vbasea <- dados$V1
  vbaseb <- dados$V2
  ibasea <- sbase/vbasea
  ibaseb <- sbase/vbaseb
  a <- dados$V1/dados$V2
  z <- complex(real = (dados$R1 + (a^2)*dados$R2), imaginary = (dados$X1+ (a^2)*dados$X2))
  ie <- complex(modulus = (dados$S/(a*dados$V2)), argument = -calculaAngulo(dados$fp, dados$adiantado))
  ic <- ie/a
  vc <- a*dados$V2
  ve <- z*ie + vc
  se <- Mod(ve)*Mod(ie)
  vepu <- ve/vbasea
  sepu <- se/sbase
  vcpu <- vc/vbasea
  re <- (Mod(vepu)-Mod(vcpu))/Mod(vcpu)
  pepu <- sepu*cos(Arg(ve)-Arg(ie))
  pcpu <- 1*dados$fp
  n <- pcpu/pepu
  
  solution <- list(sbase=sbase, vbasea=vbasea, vbaseb=vbaseb,ibasea=ibasea, ibaseb=ibaseb, z=z,
                   ie = ie, vc=vc, ve=ve,se=se, ic=ic, vepu=vepu, sepu=sepu,re=re,n=n)
  
  return(solution)
}