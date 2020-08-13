
#' @title Removes the peaks at the beginning and end of the time serie that are not part of the movement.
#' 
#' @name removeSpikes
#'
#' @description A function that removes the peaks present at the beginning and end of the time series
#'              that correspond to noises of unwanted hand movement.
#'     
#' @param eixo Time series from one of the axes (X, Y or Z) of the accelerometer.
#'
#' @return Returns the time series without noise. 
#' 
#' @examples  
#' \dontrun{  Xdf.removeSpikes <- removeSpikes(df$X)
#'             Ydf.removeSpikes <- removeSpikes(df$Y)
#'             Zdf.removeSpikes <- removeSpikes(df$Z)}
#'       
#' @importFrom graphics boxplot
#'
#' @author Viviane Lima
#'
#' @export

removeSpikes <- function(eixo) 
{
  
  k <- boxplot(eixo, plot = FALSE)
  
  Q3 <- k$stats[4] # Terceiro quartil
  Q1 <- k$stats[2] # Primeiro quartil
  IQR <- Q3 - Q1   # distancia interquartil
  
  
  h <- 0.5 * IQR # limiar para detectar os picos indesejados
  
  UIF <- as.numeric(Q3 + h) 
  
  LIF <- as.numeric(Q1 - h) 
  
  
  df <- data.frame(time=(1:length(eixo)), signal = eixo)
  df$UIF <- NA
  df$LIF <- NA
  
  indxUIF <- which(eixo > UIF)
  indxLIF <-which(eixo < LIF)
  
  eixo[indxUIF] <- UIF
  eixo[indxLIF] <- LIF
  
  return(eixo)
}