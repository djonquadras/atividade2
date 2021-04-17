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
  a <- ceiling((dados$V1/sqrt(3))/dados$V2)
  
  zeqSEC <- dados$Z/3
  is <- (dados$CARGA)/(sqrt(3)*dados$VL*dados$FP)
  phi <- calculaAngulo(dados$FP, dados$ADIANTADO)
  is <- complex(modulus = is, argument = -phi)
  vss <- dados$VL/sqrt(3)
  vps <- is*zeqSEC + vss
  
  vpp <- vps*a
  vsp <- vss/a
  zeqPRI <-  zeqSEC*(a^2)
  ip <- is/a

  solution <- list(ZEQ = zeqSEC, a = a, zeqPRI = zeqPRI, is = is, phi = phi,
                   vss = vss, vps = vps, vpp = vpp, vsp = vsp, ip = ip)
  return(solution)
}