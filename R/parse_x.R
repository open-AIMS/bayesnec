#' parse_x
#'
#' Fits a concentration(dose)-response model using brms
#'
#' @inheritParams bnec
#'
#' @importFrom grDevices xy.coords
#' @importFrom methods is
#'
#' @return A list containing the elements data, x_var, y_var, trials_var
parse_x <- function(x, y, data,  x_var, y_var, model, trials_var, family){
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
      if(!is.na(trials_var)){
        if(max(data[ , y_var])>1){
         data[ , y_var] <- as.integer(round(data[ , y_var]))
         warning("Argument trials_var supplied, implying a binomial family, y data treated as integer.")
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
          stop("If data are passed using x, trials_var must contain a numeric",
               " vector indicating the number of trials for each observation.")
        }        
      }
  
    }
    
  } else {
    if(missing(data)) stop("You must supply data if using formula syntax.")

    if(!missing(x_var) || !missing(y_var) || !missing(model) || !is.na(trials_var)) {
      warning("Arguments x_var, y_var, trials_var and model ignored in the case of formula syntax")      
    }
    
    get_vals <- gsub("model", "", unlist(strsplit(as.character(x), ",")))

    if(grepl("|", get_vals[2], fixed = TRUE)){
      inputs <- gsub("[^\\w]*", "", unlist(strsplit(get_vals, " | trials")), perl = TRUE)
      y_var <- inputs[2]
      x_var <- inputs[5]
      model <- inputs[7]  
      trials_var <- inputs[4]
    } else {
      inputs <- gsub("[^\\w]*", "", get_vals, perl = TRUE)

      y_var <- inputs[2]
      x_var <- inputs[3]
      model <- inputs[4]      
    }

  }
  list(data = data, x_var = x_var, y_var = y_var, trials_var = trials_var,
       model = model)
}
