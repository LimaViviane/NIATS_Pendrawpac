#####--------------- splitData---------------#####
#
# -----------------------------------------------------
# Author: Viviane Lima
# Contact e-mail: viviane.engbiomedical@gmail.com
# Address:  Centre for Innovation and Technology Assessment in Health, 
#           Faculty of Electrical Engineering, Federal University of Uberlândia, 
#           Uberlândia, Brazil
# -----------------------------------------------------
#
# Split the sign into two tasks: contour and free draw.
# 
# Description:
#       A function that takes the data collected by TREMSEN and
#       separates it into two files: one containing the task of
#       contouring the drawing and the other of drawing without
#       contour. The separate data has all the axes of the inertial sensors.
#     
# Input:
#       df:  dataframe resulting from LoadTREMSENFile.
#
# Output:
#       A list containing two signs: one with the task of contouring
#       the drawing and another with the sign of the freehand drawing.
# 
# Example of use:
#         df.splitData <- splitData(df) 
#
#
splitData <- function(df) 
{
  dados <- df
  vectorLabel <- dados$X.PULSE.LABEL
  vector1 <- as.numeric(levels(vectorLabel)[vectorLabel])
  timeC <- 0
  timeCf <- 0
  timeDL <- 0
  timeDLf <- 0 
  
  for(i in 1:length(vector1))
  {
    if(identical(vector1[i], 1, num.eq = TRUE) && identical(timeC, 0, num.eq = TRUE)){
      timeC <- i
    }
    if(identical(vector1[i], 1, num.eq = TRUE) && identical(vector1[i+1], 0, num.eq = TRUE))
      timeCf <- i
  }
  
  for(i in 1:length(vector1))
  {
    if(identical(vector1[i], 2, num.eq = TRUE) && identical(timeDL, 0, num.eq = TRUE)){
      timeDL <- i
    }
    if(identical(vector1[i], 2, num.eq = TRUE) && identical(vector1[i+1], 0, num.eq = TRUE))
      timeDLf <- i
  }
  
  
  contorno <- dados[timeC:timeCf, ]
  DesLivre <- dados[timeDL:timeDLf, ]
  
  return(list(contorno = contorno, DesLivre = DesLivre))
}

