#####--------------- selectAxis---------------#####
#
# -----------------------------------------------------
# Author: Viviane Lima
# Contact e-mail: viviane.engbiomedical@gmail.com
# Address:  Centre for Innovation and Technology Assessment in Health, 
#           Faculty of Electrical Engineering, Federal University of Uberlândia, 
#           Uberlândia, Brazil
# -----------------------------------------------------
#
# Select the axis that best describes the hand movement when performing the task
# 
# Description:
#       A function that receives a dataframe and selects the accelerometer
#       axis whose signal best represents the hand's oscillatory movement 
#       when drawing a sinusoid or Archimedes spiral.
#     
# Input:
#       df: dataframe resulting from smoothing.
#
#       curve: A character that represents the type of the drawing. 'S' for sinusoid and 'E' for spiral.
#       
# Output:
#       A time series with the best selected axis.
#
# Example of use:
#       df.selectAxis <- selectAxis(df, curve = 'S')
#   or
#       df.selectAxis <- selectAxis(df, curve = 'E')
#

selectAxis <- function(df, curve) #Utiliza os resultados da funcao "Ccf_function" para selecionar o melhor eixo 
{
  dfsinal <- df
  tipo <- curve
  maior <- NA 
  
  X <- NA
  Y <- NA
  Z <- NA
  
  X_comp <- NA
  Y_comp <- NA
  Z_comp <- NA
  
  X <- dfsinal$X.A1.X.
  Y <- dfsinal$X.A1.Y.
  Z <- dfsinal$X.A1.Z.
  
  X_comp <- ccf_function(df = dfsinal, dados = X, curva = tipo)
  Y_comp <- ccf_function(df = dfsinal, dados = Y, curva = tipo)
  Z_comp <- ccf_function(df = dfsinal, dados = Z, curva = tipo)
  
  
  medias <- c(abs(X_comp), abs(Y_comp), abs(Z_comp))
  
  
  menor_valor <- which(medias %in% min(medias)) 
  
  melhor <- as.numeric(menor_valor)
  
  if(identical(melhor, 1))
  {
    print("Melhor eixo: X")
    return(dfsinal$X.A1.X.)
  }
  
  if (identical(melhor, 2))
  {
    print("Melhor eixo: Y")
    return(dfsinal$X.A1.Y.)
  }
  
  
  if (identical(melhor, 3))
  {
    print("Melhor eixo: Z")
    return(dfsinal$X.A1.Z.)
  }
  
}
