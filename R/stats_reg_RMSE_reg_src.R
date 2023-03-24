#'this function returns a tibble containing the absolute and percentage difference
#'between the mean and median of the intercept and actual value at the top and
#'bottom deciles, operates from a file or an object depending on if prawn or
#'input path is used. The error values are currently not very well labeled
#'
#'@param prawn_path the path for the data source, defaults to FALSE this should
#'reference a csv file
#'
#'@param input_path the object to use as a data source defaults to FALSE this
#'should be the name of an existing object
#'
#'@param MSE_tinker calculates the RMSE when only the quantile specified is used
#'in analysis. if this is the default at 0.99 then MSE99 in the output will be
#'calculated from a model where data above the 99th percentile is removed. if
#'small alterations to this change RMSE dramatically it shows that the highest
#'values are having a large inmact on the RMSE
#'
#' @keywords data
#'
#' @export
#'
#' @examples
#' stats_reg_RMSE_reg_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx")

stats_reg_RMSE_reg_src <- function(prawn_path="unspecified", input_path="unspecified",MSE_tinker=0.999,pollutant){
  #read the input file if a filepath is given
  if (prawn_path != "unspecified"){
    data <- read.csv(prawn_path,
                     row.names=1,
                     check.names=FALSE)
  }

  #reference an existing object if an object name is given
  if (input_path != "unspecified"){
    data <- input_path
  }


  end_of_sources <- which(colnames(data)=="LSOA11CD")-1

  source_list <- colnames(data)[c(1:end_of_sources)]

  long_data <- data %>%
    pivot_longer(
      cols=all_of(c(source_list,"Point sources")),
      names_to = "Emission_source",
      values_to = "emissions") %>% group_by(Emission_source)%>%
    mutate(emissions=replace_na(emissions,0))


  #get the residuals
  resid <- long_data %>% mutate(resids=resid(lm(emissions~IMD,data=cur_data()))) %>% group_by(IMD,Emission_source)

  res_anchor <- resid %>% summarise(bound=quantile(emissions,probs=0.99))

  hmm <- inner_join(resid %>%dplyr::select(Emission_source,resids,IMD),res_anchor, by=c("Emission_source","IMD")) %>%filter(resids<=bound)

  topgone <- long_data %>% mutate(bound=quantile(emissions,probs=MSE_tinker)) %>%
    filter(emissions<=bound) %>%
    mutate(resids=resid(lm(emissions~IMD,data=cur_data())))

  #make a new function because range isnt wahat it says it is
  rangeffs <- function(data){
    thing <- range(data)
    result <- thing[2]-thing[1]
    result
  }
  down_the_rabbit_hole <- ggplot(data=hmm,aes(x=resids))+

    geom_histogram(bins = 60)+

    scale_y_continuous(expand=c(0,0))+

    facet_wrap(~Emission_source,scale="free")+


    labs(x=bquote("Residual values for "~.(pollutant)~"emissions when fitted to a linear model/ tonnes "~km^"-2"),
         y="Frequency")

  deeper_down <- down_the_rabbit_hole+

    geom_line(aes(y=dnorm(resids,
                          mean=tapply(resids,Emission_source,mean)[PANEL],
                          sd=tapply(resids,Emission_source,sd)[PANEL])*
                            #Scale the line so it's visible
                            tapply(resids,Emission_source,rangeffs)[PANEL]/60*nrow(data)))

  res_plot <- ggplot(data=hmm,aes(x=factor(IMD),y=resids))+
    geom_violin(trim=TRUE)+
    geom_hline(yintercept = 0)+
    facet_wrap(~Emission_source,scale="free_y")+
    labs(y=bquote("Residuals for linear model linking "~.(pollutant)~"emissions and IMD/ tonnes "~km^"-2"),
         x="IMD decile where 10 is least deprived")


  RMSE <- resid %>% group_by(Emission_source) %>% mutate(resids=resids^2) %>% summarise(MSE=mean(resids)) %>% mutate(RMSE=MSE^0.5)
  RMSE99 <- topgone %>% group_by(Emission_source) %>% mutate(resids=resids^2) %>% summarise(MSE99=mean(resids)) %>% mutate(RMSE99=MSE99^0.5)
  mean_reg_mod <-long_data %>%
    #get the rsquared
    do(glance(lm(emissions~IMD, data=.))) %>% inner_join(RMSE,by="Emission_source")%>% inner_join(RMSE99,by="Emission_source")

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

  #
  mean_regression <- long_data %>% do(tidy(lm(emissions~IMD, data=.))) %>%
    #pivot out the stats so each source is on one row
    pivot_wider(names_from=term,
              values_from=c(estimate,p.value,std.error,statistic)) %>%

    #rename the columns to avoid confusion
    rename(intercept="estimate_(Intercept)", gradient=estimate_IMD) %>%

    #calculate the intercepts
    mutate(mean_line_1=intercept+gradient,
           mean_line_10=intercept+10*gradient,
           flat_mean_regression_differnce=mean_line_10-mean_line_1,
           percentage_mean_regression=flat_mean_regression_differnce/mean_line_1)



  #calculate the median for each decile
  median_values <- long_data  %>% group_by(IMD,Emission_source) %>%
    summarise(median=median(emissions))
  #Get the r2 value through shenanigans
  medarr <- median_values %>% ungroup(IMD) %>% group_by(Emission_source) %>%
    #get the tabulated stats from regression on the median data
    do(glance(lm(median~IMD, data=.)))
  #create a linear model for use in the next part
  median_regression <- median_values %>% ungroup(IMD) %>% group_by(Emission_source) %>%
    #get the tabulated stats from regression on the median data
    do(tidy(lm(median~IMD, data=.)))%>%
    #pivot out the data so each source is on one line
    pivot_wider(names_from=term,
                values_from=c(estimate,
                              p.value,
                              std.error,
                              statistic)) %>%
    #rename the columns to avoid issues
    rename(intercept="estimate_(Intercept)", gradient=estimate_IMD
           ) %>%

    bind_cols(medarr$r.squared) %>% rename(median_R_squared="...10") %>%

    #calculate the intercepts
    mutate(median_line_1=intercept+gradient,
           median_line_10=intercept+10*gradient,
           flat_median_regression_differnce=median_line_10-median_line_1,
           percentage_median_regression=flat_median_regression_differnce/median_line_1)


    med_reg_mod <- median_values %>% ungroup(IMD) %>% group_by(Emission_source) %>%
      #get the rsquared
      do(glance(lm(median~IMD, data=.)))
  #calculate the value of the linear model at each point

  #Meld all three point values
  output <- inner_join(point_summary,mean_regression,by="Emission_source") %>%
             inner_join(median_regression,by="Emission_source")

  #

  bigout <- list(mean_reg_mod,med_reg_mod,output,res_plot,deeper_down)

  bigout


}

