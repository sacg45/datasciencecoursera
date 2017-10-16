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

corr <- function(directory, threshold = 0){
  
  tmp_cor <- c()
  
  for (x in list.files(directory)){
    pollution_data = read.csv(paste(sep = "",directory,"/",x))
    
    numbers_rec = length((pollution_data[!is.na(pollution_data$nitrate) & !is.na(pollution_data$sulfate),]$sulfate))
    
    if (numbers_rec > threshold){
      add_cor <- cor(x =pollution_data[!is.na(pollution_data$nitrate) & !is.na(pollution_data$sulfate),]$sulfate, y = pollution_data[!is.na(pollution_data$nitrate) & !is.na(pollution_data$sulfate),]$nitrate)
    
      tmp_cor <- c(tmp_cor, add_cor)
      
      }
    
  }

  return(tmp_cor)
}  