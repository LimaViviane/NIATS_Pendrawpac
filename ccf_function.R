#####--------------- ccf_function ---------------#####
#
# -----------------------------------------------------
# Author: Viviane Lima
# Contact e-mail: viviane.engbiomedical@gmail.com
# Address:  Centre for Innovation and Technology Assessment in Health, 
#           Faculty of Electrical Engineering, Federal University of Uberlândia, 
#           Uberlândia, Brazil
# -----------------------------------------------------
#
# Apply the cross correlation function (CCF).
# 
# Description:
#       A function used by SelectAxis to select the axis that
#       best describes the movement performed. Apply the cross
#       correlation function (CCF) between the input data and T
#       he curve corresponding to the sinusoid or spiral draw.
#      
# Input:
#       df:  dataframe resulting from LoadTREMSENFile.
#
#       curva:  A character that represents the type of the drawing. 'S' for sinusoid and 'E' for spiral
# 
#       dados Time serie corresponding to one of the axes of the accelerometer.
#      
# Output:
#       Returns the average of the differences between the maximum and minimum values
#       of the cross correlation of the input signal and the standard curve in comparison
#       with the maximum and minimum points of the autocorrelation function of the standard curve.
#
 


ccf_function <- function(df, dados, curva)
{
  dfsinal <- df
  dados <- dados
  y <- NA
  
  if(curva == "S")
  {
    
    fs <- 300                                 # frequencia de amostragem (300 Hz)
    dt <- 1/fs                                # resolucao temporal
    tam <- length(dfsinal$X.Time) * dt        # tempo maximo em segundos
    t <- seq(from=0, to=tam,by=dt)            # vetor de tempo
    
    freq <- (length(dfsinal$X.Time)/4) * dt
    fy <- 1/freq                              # frequencia de oscilacao do sinal
    
    senoide <- sin(2*pi*fy*t)                 # sinal sintetico
    
    correl <- ccf( dados , senoide,   lag.max = length(dfsinal$X.Time), type = c("correlation"),
                   plot = FALSE)
    
    y <- correl[["acf"]]
    
    max_sen <- c(-0.1250000,  0.2499997, -0.3749991,  0.4999978, -0.6249985,  0.7499992, -0.8749998,  1.0000000,
                 -0.8749998,  0.7499992, -0.6249985,  0.4999978, -0.3749991,  0.2499997, -0.1250000)
    
    max_sen2 <- abs(max_sen)
    
    k <- 1
    j <- 1
    m <- 1
    
    maximos <- NA
    maximos2 <- NA
    erro <- NA
    
    for (l in 2:(length(y)-1)) {
      
      if((y[l] > y[l-1] & y[l+1] < y[l]) ||  (y[l-1] > y[l] & y[l+1] > y[l]) )  
      {
        
        maximos[k] <- y[l]
        k <- k+1
      }
      
    }
    
    maximos2 <- abs(maximos)
    
    
    for (i in 1:15) {
      
      erro[m] <- max_sen2[i] - maximos2[i]
      m <- m +1 
    }
    
    md_erro <- mean(erro)
    
    return(md_erro)
    
  }
  
  if(curva == "E")
  {
    dt <- 6*pi/ length(dfsinal$X.Time) 
    
    t <-seq(from=0, to=6*pi, by=dt) #parametro t 
    
    espiral <- data.frame(Eixo.X=1*(t)*cos(t), Eixo.Y=1*(t)*sin(t))
    #plot(espiral)
    
    correl <- ccf(dados, espiral$Eixo.Y,   lag.max = length(dfsinal$X.Time), type = c("correlation"),
                  plot = FALSE)
    
    y <- correl[["acf"]]
    
    max_esp <- c(0.00002561342, -0.06631342, 0.1452651, -0.3262855, 0.5156660, -0.7563879, 1.000000,
                 -0.7563879, 0.5156660, -0.3262855, 0.1452651, -0.06631342, 0.00002561342)
    
    max_esp2 <- abs(max_esp)
    
    k <- 1
    j <- 1
    m <- 1
    
    maximos <- NA
    maximos2 <- NA
    erro <- NA
    
    for (l in 2:(length(y)-1)) {
      
      if((y[l] > y[l-1] & y[l+1] < y[l]) ||  (y[l-1] > y[l] & y[l+1] > y[l]) )  
      {
        
        maximos[k] <- y[l]
        k <- k+1
      }
      
    }
    
    maximos2 <- abs(maximos)
    
    
    for (i in 1:13) {
      
      erro[m] <- max_esp2[i] - maximos2[i]
      m <- m +1 
    }
    
    md_erro <- mean(erro)
    
    return(md_erro)
    
    
  }
  
}
