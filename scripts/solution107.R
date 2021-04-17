rad2degree <- function(angle){
  return(180*angle/pi)
}
degree2rad <- function(angle){
  return(pi*angle/180)
}

solution <- function(dados){
  vef <-  round(sqrt(3)*dados$V1,2)
  
  vab1 <-  complex(modulus = vef, argument = degree2rad(-30))
  vbc1 <-  complex(modulus = vef, argument = degree2rad(-150))
  va1 <-  complex(modulus = vef, argument = degree2rad(90))
  vc1 <-  vab1 + vbc1 + va1
  
  vab2 <-  complex(modulus = vef, argument = degree2rad(-30))
  vbc2 <-  complex(modulus = vef, argument = degree2rad(30))
  va2 <-  complex(modulus = vef, argument = degree2rad(90))
  vc2 <-  vab2 + vbc2 + va2
  
  vab3 <-  complex(modulus = vef, argument = degree2rad(-30))
  vbc3 <-  complex(modulus = vef, argument = degree2rad(30))
  va3 <-  complex(modulus = vef, argument = degree2rad(90))
  vc3 <-  vab3 + vbc3 + va3
  
  
  resultado <- list(
    Vef = vef,
    VAB = complex(modulus = vef, argument = degree2rad(0)),
    VBC = complex(modulus = vef, argument = degree2rad(-120)),
    VCA = complex(modulus = vef, argument = degree2rad(120)),
    VAN = complex(modulus = dados$V1, argument = degree2rad(-30)),
    VBN = complex(modulus = dados$V1, argument = degree2rad(-150)),
    VCN = complex(modulus = dados$V1, argument = degree2rad(90)),
    VAB1 = vab1, VBC1 = vbc1, VA1 = va1, VC1  = vc1,
    VAB2 = vab2, VBC2 = vbc2, VA2 = va2, VC2  = vc2,
    VAB3 = vab3, VBC3 = vbc3, VA3 = va3, VC3  = vc3
    
    )
}