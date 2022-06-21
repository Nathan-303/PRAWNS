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
    data <- read.csv(prawn_path,
                     row.names=1,
                     check.names=FALSE)
  }

  #reference an existing object if an object name is given
  if (input_path != FALSE){
    data <- input_path
  }

  #make the data long so the sources can be processed separately
  long_data <- data %>% pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
           ,"Waste treatment and disposal","Point sources"),
    names_to = "Emission_source",
    values_to = "emissions") %>%
    group_by(Emission_source) %>%
    mutate(emissions=replace_na(emissions,0))

  #get the summary stats for deciles 1 and 10
  point_summary <- filter(group_by(long_data,IMD,Emission_source), IMD %in% c(1,10)) %>%
    #add grouping for summarise
    #group_by(IMD) %>%
    #calculate the mean and median for each decile
    summarise(mean=mean(emissions),median=median(emissions)) %>%
    #Make the data wide so the summary columns can be calculated
    pivot_wider(names_from=IMD,values_from=c(mean,median)) %>%
    #calculate the flat and % differences between the deciles
    mutate(mean_flat_difference=mean_10-mean_1,
           mean_percentage_differnce=mean_flat_difference/mean_1,
           median_flat_difference=median_10-median_1,
           median_percentage_differnce=median_flat_difference/median_1,)

  #jerry rig a fix to do(lm()) not wnting to work
  unbound <- long_data %>% do(tidy(lm(emissions~IMD, data=.)))


  line_calculations <- mutate(binding,Emission_source=pull(group_keys(long_data))) %>%

    #rename the columns to avoid confusion
    rename(intercept="estimate_(Intercept)", gradient=estimate_IMD) %>%

    #calculate the intercepts
    mutate(mean_line_1=intercept+gradient,
           mean_line_10=intercept+10*gradient,
           flat_mean_regression_differnce=mean_line_10-mean_line_1,
           percentage_mean_regression=flat_mean_regression_differnce/mean_line_1)



  #calculate the median for each decile
  meds <- long_data  %>% group_by(IMD,Emission_source) %>%
    summarise(median=median(emissions))

  #create a linear model for use in the next part
  med_fit <- meds %>% ungroup(IMD) %>% group_by(Emission_source) %>% do(tidy(lm(median~IMD, data=.)))

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

