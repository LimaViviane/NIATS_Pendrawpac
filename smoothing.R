

#' @title Smooths data.  
#' 
#' @name smoothing
#'
#' @description A function that receives a data frame collected through TREMSEN 
#'              and smooths the signals from accelerometer 1 and 2.
#'
#' @param df dataframe resulting from removeSpikes.
#'
#' @details Although the data collected with TREMSEN have the magnetorometer and gyroscope sensors,
#'          this function is configured to remove trends only from the signals from accelerometers 1 and 2.
#'
#' @return A data frame with the signals of accelerometers 1 and 2 without noise. 
#' 
#' @examples \dontrun{  df.smoothing <- smoothing(df)}
#' 
#' @importFrom stats loess predict
#'
#' @author Viviane Lima
#'
#' @export
smoothing <- function(df)  
{
  dfsinal <- df
  
  ### Acelerometro 1: X ###
  
  x1_loessMod15 <- loess(dfsinal$X.A1.X. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  x1_smoothed15 <- predict( x1_loessMod15) 
  
  dfsinal$X.A1.X. <- x1_smoothed15
  
  
  ### Acelerometro 1: Y ###
  y1_loessMod15 <- loess(dfsinal$X.A1.Y. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  y1_smoothed15 <- predict(y1_loessMod15)
  
  dfsinal$X.A1.Y. <- y1_smoothed15
  
  
  ### Acelerometro 1: Z ###
  z1_loessMod15 <- loess(dfsinal$X.A1.Z. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  z1_smoothed15 <- predict(z1_loessMod15)
  
  dfsinal$X.A1.Z. <- z1_smoothed15
  
  
  
  ### Acelerometro 2: X ###
  x2_loessMod15 <- loess(dfsinal$X.A2.X. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  x2_smoothed15 <- predict( x2_loessMod15) 
  
  dfsinal$X.A2.X. <- x2_smoothed15
  
  
  ### Acelerometro 2: Y ###
  y2_loessMod15 <- loess(dfsinal$X.A2.Y. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  y2_smoothed15 <- predict(y2_loessMod15)
  
  dfsinal$X.A2.Y. <- y2_smoothed15
  
  
  ### Acelerometro 2: Z ###
  z2_loessMod15 <- loess(dfsinal$X.A2.Z. ~ dfsinal$X.Time., data=dfsinal, span=0.15) # 15% smoothing span
  z2_smoothed15 <- predict(z2_loessMod15)
  
  dfsinal$X.A2.Z. <- z2_smoothed15
  
  
  return(dfsinal)
}
