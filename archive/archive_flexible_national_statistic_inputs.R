additional_data <- list()
for (count in c(1:length(data_path))){
  #Read the file as a tibble and hold it in a transition state whilst it's checked for errors
  ts <- read.csv(data_path[count],row.names=1) %>% tibble()
  #Check for a known formatting issue in the column names and correct it
  if (colnames(ts)[1]== "ï..LSOA11CD"||colnames(ts)[1]== "LSOA.code..2011."){
    colnames(ts)[1] <- "LSOA11CD"
  }
  #Store the transition state in the list of data
  additional_data[count] <- ts
}

#Create a tibble to collect all the additional data in
additional_data_tibble <- additional_data[1]

#If there's more than one table of additional data, combine them all
if(length((additional_data)>1)){
  for (count in c(2:length(additional_data))){
    inner_join(additional_data_tibble,additional_data[count],by="LSOA11CD")
  }}
