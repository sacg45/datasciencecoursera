complete <- function(directory, id = 1:332){
  
  fld_list <- c()
  id_list <- c()
  num_cases <- c()
  
  for (x in list.files(directory)){
    fld_list <- c(fld_list,substr(x,1,3))
  }
  
  for(x in id){
    if( x<10 ){
      id1 <- paste(sep = "", "00", x)
    } else if ( x<100 ) {
      id1 <- paste(sep = "", "0", x)
    } else {
      id1 <- paste(sep = "", x)
    }
    
    if (id1 %in% fld_list == TRUE) {
      pollution_data = read.csv(paste(sep = "",directory,"/",id1,".csv"))
      
      total_cases <-  sum(!is.na(pollution_data$sulfate) & !is.na(pollution_data$nitrate))
      
      id_list <- c(id_list, id1)
      num_cases <- c(num_cases, total_cases)
      
    }
    
  }
  result <- data.frame(id = id_list, nobs = num_cases)
  return(result)
}   
