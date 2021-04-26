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
  i2 <- dados$S/dados$V2
  i3 <- dados$S/dados$V1
  i1 <- i2-i3
  strans <- i2*dados$V2
  snominal <- i1*dados$V1
  spassante <- snominal - strans
  somaPerdas1 <- ((1/dados$R)-1)*dados$S*dados$FP1
  somaPerdas2 <- ((1/dados$R)-1)*dados$S*dados$FP2
  n1 <- (snominal*cos(dados$FP1))/(snominal*cos(dados$FP1)+somaPerdas1)
  n2 <- (snominal*cos(dados$FP2))/(snominal*cos(dados$FP2)+somaPerdas2)
  
  solution <- list(i1=i1, i2=i2, i3=i3, strans=strans, snominal=snominal,
                   spassante = spassante, somaPerdas1=somaPerdas1, somaPerdas2=somaPerdas2,
                   n1=n1, n2=n2)
  
  return(solution)
}