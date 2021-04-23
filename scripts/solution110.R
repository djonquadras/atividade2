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
    
  ic <- NULL
  ie <- NULL
  ss <- NULL
  
  ve <-  NULL
  fp <- NULL
  re <- NULL
  n <-  NULL
  se <- NULL
  
  sol <- list(zyEqSec = zyEqSec, vATFase = vATFase, vATLinha = vATLinha, vBTLinha = vBTLinha,
              vBTFase = vBTFase, a = a, vcr = vcr, zyEqPri = zyEqPri, ic = ic, ss = ss, se=se,
              ve = ve, fp=fp,re=re,n=n,ie=ie)
}

solution <- function(dados1,dados2){
  solTrafo1 <- solutionToEachTrafo(dados1)
  solTrafo2 <- solutionToEachTrafo(dados2)
  
  ic <- complex(modulus = (dados1$P3PHI)/(sqrt(3)*solTrafo1$vATLinha*dados2$FP), argument = -calculaAngulo(dados2$FP, dados2$ADIANTADO))
  ie <- ic*solTrafo1$a
  
  solTrafo2$ic <- (ic)/(1+(solTrafo2$zyEqPri/solTrafo1$zyEqPri)) 
  solTrafo2$ie <- solTrafo2$ic*solTrafo1$a
  
  solTrafo1$ic <- ic - solTrafo2$ic
  solTrafo1$ie <- solTrafo1$ic*solTrafo1$a
  
  solTrafo1$ss <- solTrafo1$vcr*Conj(solTrafo1$ic)
  solTrafo2$ss <- solTrafo1$vcr*Conj(solTrafo2$ic)
  Zpar <- (1/((1/solTrafo1$zyEqPri)+(1/solTrafo2$zyEqPri)))
  
  ve <- Zpar*ie + solTrafo1$vcr
  
  solTrafo1$ve <- solTrafo1$zyEqPri*solTrafo1$ie + solTrafo1$vcr
  solTrafo2$ve <- solTrafo2$zyEqPri*solTrafo2$ie + solTrafo1$vcr
  
  solTrafo1$fp <- cos(Arg(solTrafo1$ve)-Arg(solTrafo1$ie))
  solTrafo2$fp <- cos(Arg(solTrafo2$ve)-Arg(solTrafo2$ie))
  
  solTrafo1$se <- sqrt(3)*Mod(solTrafo1$ve)*Mod(solTrafo1$ie)
  solTrafo2$se <- sqrt(3)*Mod(solTrafo2$ve)*Mod(solTrafo2$ie)
  
  solTrafo1$re <- (Mod(solTrafo1$ve)-Mod(solTrafo1$vcr))/Mod(solTrafo1$vcr)
  solTrafo2$re <- (Mod(solTrafo2$ve)-Mod(solTrafo2$vcr))/Mod(solTrafo2$vcr)
  
  solTrafo2$n <- 1-Mod((solTrafo2$ss*dados2$FP)/(solTrafo2$se*solTrafo2$fp))
  solTrafo1$n <- 1-Mod((solTrafo1$ss*dados1$FP)/(solTrafo1$se*solTrafo1$fp))
  
  sfonte <- sqrt(3)*Mod(ve)*Mod(ie)
  fpfonte <- cos(Arg(ve)-Arg(ie))
  

  
  sol <- list(solTrafo1 = solTrafo1, solTrafo2 = solTrafo2, solCarga = list(ic = ic, ie = ie, ve=ve, Zpar=Zpar,
                                                                            sfonte=sfonte,fpfonte=fpfonte))

  }