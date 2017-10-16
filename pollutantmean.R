pollutantmean <- function(directory, pollutant, id){
  
  total_sum = 0
  
  total_count = 0
  
  my_mean <- c()
  
  for(x in id){
    if( x<10 ){
      id1 <- paste(sep = "", "00", x)
    } else if ( x<100 ) {
      id1 <- paste(sep = "", "0", x)
    } else {
      id1 <- paste(sep = "", x)
    }
    
    pollution_data = read.csv(paste(sep = "",directory,"/",id1,".csv"))
    
    if (pollutant == "sulfate"){
      total_sum <- total_sum + sum(pollution_data$sulfate,na.rm = TRUE)
      
      total_count <- total_count + sum(!is.na(pollution_data$sulfate))
      
    } else if (pollutant == "nitrate"){
      total_sum <- total_sum + sum(pollution_data$nitrate,na.rm = TRUE)
      
      total_count <- total_count + sum(!is.na(pollution_data$nitrate))
      
    }
    
  }
  result <- total_sum/total_count
  return(result)
}   
