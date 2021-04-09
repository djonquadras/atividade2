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


questao107 <- list(V1 = list(S = 110000, V1 = 4400, V2 = 440, f =60),
                   V2 = list(S = 20000, V1 = 2400, V2 = 240, f =60),
                   V3 = list(S = 50000, V1 = 2400, V2 = 120, f =60),
                   V4 = list(S = 300000, V1 = 11000, V2 = 2300, f =60))

questao108 <- list(V1 = list(S = 330000,
                             V1=7621.02,
                             V2=440,
                             f=60,
                             Z=complex(real = 0.05, imaginary = 0.074833),
                             P3PHI = 3600,
                             IL=2,
                             VL=7621.02,
                             CARGA=198000,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V2 = list(S = 60000,
                             V1=4156.92,
                             V2=240,
                             f=60,
                             Z=complex(real = 0.0370376, imaginary = 0.0635895),
                             P3PHI = 366,
                             IL=1.8,
                             VL=240,
                             CARGA=36000,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V3 = list(S = 150000,
                             V1=4156.92,
                             V2=120,
                             f=60,
                             Z=complex(real = 0.00502224, imaginary = 0.0102944),
                             P3PHI = 1188,
                             IL=16.71,
                             VL=120,
                             CARGA=90000,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V4 = list(S = 900000,
                             V1=19052.56,
                             V2=2300,
                             f=60,
                             Z=complex(real = 0.109958, imaginary = 0.348024),
                             P3PHI = 6420,
                             IL=6.18,
                             VL=2300,
                             CARGA=540000,
                             FP=0.6,
                             ADIANTADO=TRUE))

questao110 <- list(V1 = list(S = 660000,
                             V1=7621.02,
                             V2=440,
                             f=60,
                             Z=complex(real = 0.0142876, imaginary = 0.0264176),
                             P3PHI = 5290.3,
                             IL=1.77,
                             VL=7621.02,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V2 = list(S = 120000,
                             V1=4156.92,
                             V2=240,
                             f=60,
                             Z=complex(real = 0.02337967, imaginary = 0.0432287),
                             P3PHI = 961.87,
                             IL=10.25,
                             VL=240,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V3 = list(S = 300000,
                             V1=4156.92,
                             V2=120,
                             f=60,
                             Z=complex(real = 0.00233797, imaginary = 0.00432287),
                             P3PHI = 2404.68,
                             IL=51.23,
                             VL=120,
                             FP=0.6,
                             ADIANTADO=TRUE),
                   V4 = list(S = 1800000,
                             V1=19052.56,
                             V2=2300,
                             f=60,
                             Z=complex(real = 0.143146, imaginary = 0.264676),
                             P3PHI = 14428.09,
                             IL=16.04,
                             VL=2300,
                             FP=0.6,
                             ADIANTADO=TRUE))