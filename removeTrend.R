
#' @title Removes linear and non-linear trends. 
#' 
#' @name removeTrend
#'
#' @description A function that receives a signal collected through TREMSEN and removes
#'               linear and non-linear trends from each axis of the accelerometers. 
#'
#' @param df A dataframe.
#'
#' @details Although the data collected with TREMSEN have the magnetorometer and gyroscope sensors,
#'          this function is configured to remove trends only from the signals from accelerometers 1 and 2.
#'          
#' @return A dataframe containing the signals from accelerometers 1 and 2 without linear and non-linear trends. 
#' 
#' @examples \dontrun{ df.removeTrend <- removeTrend(df)}
#' 
#' @importFrom stats loess predict
#'
#' @author Viviane Lima
#' 
#' @export
removeTrend <- function(df) 
{
  dfsinal <- df
  
  ### Acelerometro 1: X ###
  
  x1_loessMod75 <- loess(dfsinal$X.A1.X. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  x1_smoothed75 <- predict( x1_loessMod75) 
  
  dfsinal$X.A1.X. <- dfsinal$X.A1.X. - x1_smoothed75
  
  
  ### Acelerometro 1: Y ###
  y1_loessMod75 <- loess(dfsinal$X.A1.Y. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  y1_smoothed75 <- predict(y1_loessMod75)
  
  dfsinal$X.A1.Y. <- dfsinal$X.A1.Y. - y1_smoothed75
  
  
  ### Acelerometro 1: Z ###
  z1_loessMod75 <- loess(dfsinal$X.A1.Z. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  z1_smoothed75 <- predict(z1_loessMod75)
  
  dfsinal$X.A1.Z. <- dfsinal$X.A1.Z. - z1_smoothed75
  
  ### Acelerometro 2: X ###
  x2_loessMod75 <- loess(dfsinal$X.A2.X. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  x2_smoothed75 <- predict( x2_loessMod75) 
  
  dfsinal$X.A2.X. <- dfsinal$X.A2.X. - x2_smoothed75
  
  
  ### Acelerometro 2: Y ###
  y2_loessMod75 <- loess(dfsinal$X.A2.Y. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  y2_smoothed75 <- predict(y2_loessMod75)
  
  dfsinal$X.A2.Y. <- dfsinal$X.A2.Y. - y2_smoothed75
  
  
  ### Acelerometro 2: Z ###
  z2_loessMod75 <- loess(dfsinal$X.A2.Z. ~ dfsinal$X.Time., data=dfsinal, span=0.75) # 75% smoothing span
  z2_smoothed75 <- predict(z2_loessMod75)
  
  dfsinal$X.A2.Z. <- dfsinal$X.A2.Z. - z2_smoothed75
  
  
  return(dfsinal)
}