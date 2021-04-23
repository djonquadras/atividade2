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
  ic <- complex(modulus = (dados$CARGA)/(sqrt(3)*vATLinha*dados$FP), argument = -calculaAngulo(dados$FP, dados$ADIANTADO))
  ve <- ic*zeqPRI + vcr
  ie = a*ic
    
  phi <- calculaAngulo(dados$FP, dados$ADIANTADO)

  
  sfonte <- sqrt(3)*Mod(ie)*Mod(ve)
  fpe <- cos(Arg(ve)-Arg(ie))

  re <- (Mod(ve)-Mod(vcr))/(Mod(vcr))
  pj3phi <- 3*Re(zeqSEC)*Mod(ic)^2
  n <- (dados$CARGA)/((dados$CARGA)+(dados$P3PHI)+(pj3phi))
  
  solution <- list(ZEQ = zeqSEC, a = a, zeqPRI = zeqPRI, is = ic, phi = phi,
                   se = sfonte, fpe = fpe, re = re, n=n, pj3phi=pj3phi,
                   ve = ve, ic = ic, vcr=vcr, ie=ie,vcr=vcr, vATFase=vATFase, vATLinha=vATLinha, vBTLinha=vBTLinha, vBTFase=vBTFase)
  return(solution)
}