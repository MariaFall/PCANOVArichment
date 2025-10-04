
#====##====##====##====##====##====##====##====##====##====##====##====##====#
#====##====##====##====##=====#GENERAL ANALYSIS UTILS#=====##====##====##====#
#====##====##====##====##====##====##====##====##====##====##====##====##====#



SumNormal = function(x){
  1000*x/sum(x, na.rm=T);
}

AutoScale = function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

LogTransform <- function(x, min.val, log_base) {
  
  log_base_num <- as.numeric(log_base)
  value <- (x + sqrt(x^2 + min.val^2)) / 2
  
  if (log_base_num == 2) {
    return(log2(value))
  } else if (log_base_num == 10) {
    return(log10(value))
  } else {
    warning(paste("Invalid log_base provided:", log_base))
    return(NA) 
  }
}
