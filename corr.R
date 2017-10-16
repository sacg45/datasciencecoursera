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