#'A function showing a summary of the prawn inputted to allow the data to be roughly checked
#'
#'@param prawn_path the location of the prawn to be analysed
#'

data_flagger <- function(prawn_path){

  #read in the data
  data <- read.csv(file=prawn_path,
                   row.names=1,
                   check.names=FALSE) %>% tibble()

  #Pivot the data longer so each source can be treated separately
  %>% group_by()

  #mutate in the standard deviation, median, mean and max
  mutate(mean =mean,
         median =median,
         stdev= stde)
  #output the  data in a location next to the prawn used
  write.csv(x = output,
            file = paste0())
}
