

#' @title Application and analysis of the Autocorrelation function (ACF).
#' 
#' @name acfApplication
#'
#' @description A function that applies the ACF and shows the corresponding correlogram.
#'              In addition, it calculates the area on the curve and compares the estimated
#'              coorelogram with the correlogram of the standard, sinusoidal or spiral design,
#'              and returns the mean difference between the maximum and minimum of the two graphs.   
#'     
#' @param dt dataframe resulting from selecAxis.
#' 
#' @param curve Character that indicates the type of curve used in the task.'S' for sinusoid or 'E' for spiral.
#'           
#' @return Returns a list containing the average of the difference between the maximum and minimum points of
#'         the ACF graph of the standard figure and the ACF graph of the input time series; and also the total
#'         area over the ACF curve of the time series. 
#'         
#' @examples 
#' \dontrun{df.acfApplication <- acfApplication(dt, curve = 'S')
#'           df.acfApplication <- acfApplication(dt, curve = 'E')}   
#'         
#' @author Viviane Lima
#'
#'  
#' @importFrom stats acf ccf
#' @importFrom pracma trapz
#'  
#' @export

acfApplication <- function(dt, curve){
  
  dplotfilt1 <- acf(dt , lag.max = length(dt),
                    type = c("correlation"),
                    plot = TRUE)
  
  if(curve == "S")
  {
    media_vmm <- NA
    modulo_y1 <- NA
    AreaTotal1 <- NA
    pos_vale <- NA
    pos_pico <- NA
    x <- NA
    y <- NA
    
    x <- dplotfilt1[[4]]
    y <- dplotfilt1[[1]]
    
    k <- 1
    j <- 1
    
    
    
    for (l in 2:(length(y)-1)) {
      
      if((y[l] > y[l-1] & y[l+1] < y[l])  )  
      {
        pos_pico[k] <- l
        
        k <- k+1
      }
      
      if((y[l-1] > y[l] & y[l+1] > y[l]))  
      {
        pos_vale[j] <- l
        
        j <- j+1
        
      }
    }
    
    a <- c(pos_vale, pos_pico)
    
    pos_vmm <- sort(a)
    
    max_min <- c(-0.8749997,  0.7499989, -0.6249986,  0.4999996, -0.3750000,  0.2500000, -0.1249999)
    
    
    Ep1 <- max_min[2] - y[pos_vmm[2]]
    Ep2 <- max_min[4] - y[pos_vmm[4]]
    Ep3 <- max_min[6] - y[pos_vmm[6]]
    
    Ev1 <- max_min[1] - y[pos_vmm[1]]
    Ev2 <- max_min[3] - y[pos_vmm[3]]
    Ev3 <- max_min[5] - y[pos_vmm[5]]
    Ev4 <- max_min[7] - y[pos_vmm[7]]
    
    media_vmm <- mean(c(abs(Ep1), abs(Ep2), abs(Ep3), abs(Ev1), abs(Ev2), abs(Ev3), abs(Ev4)))
    
    modulo_y1 <- abs(dplotfilt1[[1]])
    
    AreaTotal1 <- trapz(dplotfilt1[[4]], modulo_y1)
    
    return(list(Media_Vmm = media_vmm, Area_total = AreaTotal1))
  }
  
  
  if(curve == "E")
  {
    media_vmm <- NA
    modulo_y1 <- NA
    AreaTotal1 <- NA
    pos_vale <- NA
    pos_pico <- NA
    x <- NA
    y <- NA
    
    x <- dplotfilt1[[4]]
    y <- dplotfilt1[[1]]
    
    k <- 1
    j <- 1
    
    
    for (l in 2:(length(y)-1)) {
      
      if((y[l] > y[l-1] & y[l+1] < y[l])  )  
      {
        pos_pico[k] <- l
        
        k <- k+1
      }
      
      if((y[l-1] > y[l] & y[l+1] > y[l]))  
      {
        pos_vale[j] <- l
        
        j <- j+1
        
      }
    }
    
    a <- c(pos_vale, pos_pico)
    
    pos_vmm <- sort(a)
    
    max_min <- c(-0.75638792,	0.5156660, -0.3262855, 0.1452651, -0.06631342)
    
    
    Ep1 <- max_min[2] - y[pos_vmm[2]]
    Ep2 <- max_min[4] - y[pos_vmm[4]]
    
    Ev1 <- max_min[1] - y[pos_vmm[1]]
    Ev2 <- max_min[3] - y[pos_vmm[3]]
    Ev3 <- max_min[5] - y[pos_vmm[5]]
    
    media_vmm <- mean(c(abs(Ep1), abs(Ep2), abs(Ev1), abs(Ev2), abs(Ev3)))
    
    modulo_y1 <- abs(dplotfilt1[[1]])
    
    AreaTotal1 <- trapz(dplotfilt1[[4]], modulo_y1)
    
    print("Area total:")
    print(AreaTotal1)
    print("Media_vmm:")
    print(media_vmm)
    
    return(list(Media_Vmm = media_vmm, Area_total = AreaTotal1))
  }
  
}
