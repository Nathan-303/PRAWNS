#'this function returns a tibble containing the absolute and percentage difference
#'between the mean and median of the intercept and actual value at the top and
#'bottom deciles, operates from a file or an object depending on if prawn or
#'input path is used
#'@param prawn_path the path for the data source, defaults to FALSE this should
#'reference a csv file
#'
#'@param input_path the object to use as a data source defaults to FALSE this
#'should be the name of an existing object
#'
#'@param deciles the deciles targeted, should be a numeric vector of length 2 containing integers between 1 and 10
#'
#' @keywords data
#' @export

#'
#' @examples
#' stat_wrangler()
#'
#'

stat_wrangler <- function(prawn_path=FALSE, input_path=FALSE,deciles){
  #read the input file if a filepath is given
  if (prawn_path != FALSE){
    data <- read.csv(prawn_path)
  }

  #reference an existing object if an object name is given
  if (input_path != FALSE){
    data <- input_path
  }

  #make the data long so the sources can be processed separately
  data <- data %>% pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
           ,"Waste treatment and disposal","point_sources"),
    names_to = "Emission_source",
    values_to = "emissions") %>% group_by(Emission_source)

  #get the summary stats for deciles 1 and 10
  point_summary <- filter(data, IMD %in% deciles) %>%
    #add grouping for summarise
    group_by(IMD) %>%
    #calculate the mean and median for each decile
    summarise(mean=mean(Total),median=median(Total))

  #create a linear model for use in the next part
  linear_fit <- lm(Total~IMD, data=data)

  #calculate the value of the linear model at each point
  linear_intercepts <- tibble(IMD=c(1,10),
                              lm_intercept=linear_fit$coefficients[1]+linear_fit$coefficients[2]*c(1,10))

  #calculate the median for each decile
  meds <- data %>% group_by(IMD) %>%
    summarise(median=median(Total))

  #create a linear model for use in the next part
  med_fit <- lm(median~IMD, data=meds)

  #calculate the value of the linear model at each point
  med_intercepts <- tibble(IMD=c(1,10),
                           median_intercept=med_fit$coefficients[1]+med_fit$coefficients[2]*c(1,10))

  #Meld all three point values
  output1 <- inner_join(point_summary,linear_intercepts,by="IMD") %>%
             inner_join(med_intercepts,by="IMD")

  #Calculate the flat difference between the deciles
  output2 <- tibble(output1[1,]-output1[2,])


  #Calculate the % difference between the deciles
  output3 <- tibble(output2/output1[1,])

  #bind the outputs together
  output <- rbind(output1,output2) %>% rbind(output3) %>%
    mutate(IMD=as.character(IMD))
    output$IMD[3] <- "Flat difference"
    output$IMD[4] <- "% difference"

  output
}

