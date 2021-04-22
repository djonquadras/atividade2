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



solution <- function(dados){
  
  vATFase <- dados$V1/sqrt(3)
  vATLinha <- (dados$V1)
  vBTLinha <- (dados$V2)
  vBTFase <- (dados$V2)/sqrt(3)
  a <- vATFase/vBTFase
  vcr <- a*vBTFase
  zeqSEC <- dados$Z/3
  zeqPRI <- (a^2)*(dados$Z)/3
  vcr <- a*vBTFase
  ic <- complex(modulus = (dados$P3PHI)/(sqrt(3)*vATLinha*dados$FP), argument = calculaAngulo(dados$FP, dados$ADIANTADO))
  ve <- ic*zeqPRI + vcr
  ie = a*ic
    
  is <- (dados$CARGA)/(sqrt(3)*dados$VL*dados$FP)
  phi <- calculaAngulo(dados$FP, dados$ADIANTADO)
  is <- complex(modulus = is, argument = -phi)
  vss <- dados$VL/sqrt(3)
  vps <- is*zeqSEC + vss

  vpp <- vps*a
  vsp <- vss/a
  ip <- is/a
  sp <- sqrt(3)*Mod(ie)*Mod(ve)
  fpp <- cos(Arg(ve)-Arg(ie))

  re <- (Mod(vps)-Mod(vss))/(Mod(vss))
  pj3phi <- 3*Re(zeqSEC)*Mod(is)^2
  n <- (dados$CARGA)/((dados$CARGA)+(dados$P3PHI)+(pj3phi))
  solution <- list(ZEQ = zeqSEC, a = a, zeqPRI = zeqPRI, is = is, phi = phi,
                   vss = vss, vps = vps, vpp = vpp, vsp = vsp, ip = ip, sp = sp,
                   fpp = fpp, re = re, n=n, pj3phi=pj3phi,
                   ve = ve, ic = ic, vcr=vcr, ie=ie)
  return(solution)
}