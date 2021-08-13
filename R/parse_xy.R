#' parse_xy
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @importFrom grDevices xy.coords
#'
#' @return A list containing the elements data, x_var, y_var, trials_var
parse_x <- function(x, y = NULL, data,  x_var, y_var, model = "all", trials_var, family = NULL){
  if(!is(x, "formula")){
    xlabel <- deparse1(substitute(x))
    ylabel <- if (!is.null(y)) 
      deparse1(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log = "")
    data <- data.frame(xy$x, xy$y)
    
    if(nrow(data)==0) stop("Your x input data contains no rows")
    
    x_var <- xy$xlab 
    y_var <- xy$ylab
    colnames(data) <- c(x_var, y_var)
    
    if(missing(trials_var)){
      trials_var <- NA
    } else {
      if(max(data[ , y_var])>1){
       data[ , y_var] <- as.integer(round(data[ , y_var]))
       warning("You have supplied a trials_var argument, forcing your y data to integer.")
      }
      
      if(is(trials_var, "numeric")){
        if(length(trials_var)==nrow(data)){
          data$trials <- trials_var
          trials_var <- "trials" 
          if(max(data[ , y_var])<=1){
            data[ , y_var] <- as.integer(round(data[ , y_var] * data$trials))
            warning("The maximum observed value of the response was < 1 and assumed to be a proportion of trials_var.")
          }
          
        } else {
          stop("The length of trials_var must equal length of x")
        }
        
      } else {
        stop("If data are passed using x, trials_var must contain a numeric vector indicating the number of trials for each observation")            
      }
      
    }
    
  } else {
    
    stop("bnec does not yet support formula syntax")
  }
  
  return(list(data = data, x_var = x_var, y_var = y_var, trials_var = trials_var))
}
