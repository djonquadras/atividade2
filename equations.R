calcula <- function(a,b,c){
  return(a+b*c)
}

calcula2 <- function(a,b,c){
  s <- a+b+c
  p <- a*b*c
  lista <- list(soma = s, produto = p)
  return(lista)
}
  
  