#'A function showing a summary of the prawn inputted to allow the data to be roughly checked
#'
#'@param prawn_path the location of the prawn to be analysed, should be the filepath to a csv file containing a raw prawn
#'
#'@export

data_flagger <- function(prawn_path){

  #read in the data
  data <- read.csv(file=prawn_path,
                   row.names=1,
                   check.names=FALSE) %>% tibble()


  cols <- as.numeric(ncol(data))

  #select all the data columns, the first two and the last columns will always be useless and LSOA respectively
  long_data <- data %>% select(-c(1,2,cols)) %>%
    #make the data longer, pivoting along everything
    pivot_longer(cols=everything(),
                 names_to = "Emission_source",
                 values_to = "Emissions") %>%

  #Pivot the data longer so each source can be treated separately
  group_by(Emission_source)
  napercent <- long_data %>% summarise(percentage_missing_data=sum(is.na(Emissions)/length(Emissions)))

  naless_data <- long_data %>% replace(is.na(.), 0)

  summary <- naless_data %>% summarise(
                       mean_emission=mean(Emissions),
                       median_emission=median(Emissions),
                       stdev=sd(Emissions),
                       max=max(Emissions),
                       ) %>%
    inner_join(napercent,by="Emission_source")

  summary
}
