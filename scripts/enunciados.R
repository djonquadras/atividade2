selecionaDados <- function(questao, versao){
  dados <- NULL
  if(versao == "1"){
    dados <- questao$V1
  }
  if(versao == "2"){
    dados <- questao$V2
  }
  if(versao == "3"){
    dados <- questao$V3
  }
  if(versao == "4"){
    dados <- questao$V4
  }
  return(dados)
}


questao17 <- list(V1 = list(S = 110000, V1 = 4400, V2 = 440, f =60),
                  V2 = list(S = 20000, V1 = 2400, V2 = 240, f =60),
                  V3 = list(S = 50000, V1 = 2400, V2 = 120, f =60),
                  V4 = list(S = 300000, V1 = 11000, V2 = 2300, f =60))