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

solutionToEachTrafo <- function(dados){
  
  zyEqSec <- (dados$Z)/3
  vATFase <- dados$V1/sqrt(3)
  vATLinha <- (dados$V1)
  vBTLinha <- (dados$V2)
  vBTFase <- (dados$V2)/sqrt(3)
  a <- vATFase/vBTFase
  vcr <- a*vBTFase
  zyEqPri <- (a^2)*(dados$Z)/3
    is <- NULL
    ss <- NULL
  
  sol <- list(zyEqSec = zyEqSec, vATFase = vATFase, vATLinha = vATLinha, vBTLinha = vBTLinha,
              vBTFase = vBTFase, a = a, vcr = vcr, zyEqPri = zyEqPri, is = is, ss = ss)
}

solution <- function(dados1,dados2){
  solTrafo1 <- solutionToEachTrafo(dados1)
  solTrafo2 <- solutionToEachTrafo(dados2)
  
  is <- complex(modulus = (dados1$P3PHI)/(sqrt(3)*solTrafo1$vATLinha*dados2$FP), argument = calculaAngulo(dados2$FP, dados2$ADIANTADO))
  ie <- is*solTrafo1$a
  
  solTrafo2$is <- (is)/(1+(solTrafo2$zyEqPri/solTrafo1$zyEqPri)) 
  solTrafo1$is <- is - solTrafo2$is
  
  solTrafo1$ss <- solTrafo1$vcr*Conj(solTrafo1$is)
  solTrafo2$ss <- solTrafo1$vcr*Conj(solTrafo2$is)
  
  sol <- list(solTrafo1 = solTrafo1, solTrafo2 = solTrafo2, solCarga = list(is = is, ie = ie))

  }