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

questao113 <- list(V1 = list(S = 110000, V1 = 4400, V2 = 440, f =60,
                             R1 = 2.5, X1 = 3.7417, GM = (0.061983/1000), BM = (0.45030/1000),
                             R2 = 0.025, X2 = 0.037417, fp= 0.6, adiantado = TRUE),
                   V2 = list(S = 20000, V1 = 2400, V2 = 240, f =60,
                             R1 = 1.85188, X1 = 3.17947, GM = (0.0211806/1000), BM = (0.037708706771/1000),
                             R2 = 0.01852, X2 = 0.03179, fp= 0.6, adiantado = TRUE),
                   V3 = list(S = 50000, V1 = 2400, V2 = 120, f =60,
                             R1 = 1.00445, X1 = 2.05889, GM = (0.0687500/1000), BM = (0.1889211/1000),
                             R2 = 0.00251, X2 = 0.00515, fp= 0.6, adiantado = TRUE),
                   V4 = list(S = 300000, V1 = 11000, V2 = 2300, f =60,
                             R1 = 1.25755, X1 = 3.98024, GM = (0.0176860/1000), BM = (0.06551427/1000),
                             R2 = 0.05498, X2 = 0.17401, fp= 0.6, adiantado = TRUE))

questao115 <- list(V1 = list(S = 220000, V1 = 4400, V2 = 440, f = 60,
                             CIRC = 440, GER = 4840, FP1 = 0.6, FP2 = 0.8, ADIANTADO = FALSE, R = 0.965),
                   V2 = list(S = 27500, V1 = 4400, V2 = 440, f = 60,
                             CIRC = 4400, GER = 4840, FP1 = 0.6, FP2 = 0.8, ADIANTADO = FALSE, R = 0.945),
                   V3 = list(S = 110000, V1 = 4400, V2 = 440, f = 60,
                             CIRC = 4840, GER = 440, FP1 = 0.6, FP2 = 0.8, ADIANTADO = FALSE, R = 0.961),
                   V4 = list(S = 55000, V1 = 4400, V2 = 440, f = 60,
                             CIRC = 4840, GER = 4400, FP1 = 0.6, FP2 = 0.8, ADIANTADO = FALSE, R = 0.957))

questao116 <- list(V1 = list(PV = 29700, PP = 15000000, SV = 132000, SP = 15000000, TV = 9600, TP = 5250000,
                             E1Z = 0.069, E1V = 29700, E1P = 15000000,
                             E2Z = 0.056, E2V = 29700, E2P = 5250000,
                             E3Z = 0.038, E3V = 132000, E3P = 5250000),
                   V2 = list(PV = 29700, PP = 30000000, SV = 132000, SP = 30000000, TV = 9600, TP = 10500000,
                             E1Z = 0.069, E1V = 29700, E1P = 30000000,
                             E2Z = 0.056, E2V = 29700, E2P = 10500000,
                             E3Z = 0.038, E3V = 132000, E3P = 10500000),
                   V3 = list(PV = 14850, PP = 30000000, SV = 66000, SP = 30000000, TV = 4800, TP = 10500000,
                             E1Z = 0.069, E1V = 14850, E1P = 30000000,
                             E2Z = 0.056, E2V = 14850, E2P = 10500000,
                             E3Z = 0.038, E3V = 66000, E3P = 10500000),
                   V4 = list(PV = 14850, PP = 15000000, SV = 66000, SP = 15000000, TV = 4800, TP = 5250000,
                             E1Z = 0.069, E1V = 14850, E1P = 15000000,
                             E2Z = 0.056, E2V = 14850, E2P = 5250000,
                             E3Z = 0.038, E3V = 66000, E3P = 5250000))