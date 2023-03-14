#' Plot the progression in pollutant inequality over time
#'
#' @param time_range the years to be used, used for tracking filenames, should
#' be a consecutive numerical vector
#' @param pollutant the pollutant being investigated, this should be in quotes
#' and correspond with its name in the data
#' @param name_generator the procedure to generate the input filename, should be
#'  a function with year as the imput
#' @param output_path to specify a place to put the output, will otherwise just
#'  output to the working directory to stay file structure agnostic
#' @param premade defaults to no, used if it's been run before and the file with
#'  a summary of the different years exists so you can save time, if inputting
#'  then it should be a filepath
#'
#` @keywords faceted, sources
#' @export
#' @examples
#' temporal_inequalities()
#'

temporal_inequalities <- function(time_range,pollutant,name_generator,output_path="",premade="no"){

  if(premade=="no"){
  for (year in time_range){

    file_format="agg_png"
    if(grepl("agg_",file_format)==TRUE){
      file_format1 <- gsub(pattern="agg_",x=file_format,replacement = "")}
  print("Calling creator")
    modelled_pollutant <- process_create_prawns(csv_coordinates_path = name_generator(year),
                                        shapefile_path = "Data/2011_LSOA_shapefile_20m_generalised",
                                        pollutant_data_name = pollutant,
                                        year = year,
                                        pollutant = pollutant,
                                        is_raw = TRUE
    )
    print("prawns made")

    write.csv(x=modelled_pollutant,
              file = paste0(output_path,"Modelled ",pollutant," emissions in ",year,".csv"))

    basic_plot <- model_plot(model_prawn_path=paste0(output_path,"Modelled ",pollutant," emissions in ",year,".csv"),
                             pollutant=pollutant,
                             year=year)

    graph_saver(filename= paste0(output_path,
                                 pollutant," concentrations by decile ",year,".",file_format1),
                plot=basic_plot,
                file_format = file_format,
                type=2,
                scaling=1.2)




    point_summary <- filter(group_by(modelled_pollutant,IMD), IMD %in% c(1,10)) %>%
      #add grouping for summarise
      #group_by(IMD) %>%
      #calculate the mean and median for each decile
      summarise(mean=mean(poll_mean),median=median(poll_mean)) %>%
      #Make the data wide so the summary columns can be calculated
      pivot_wider(names_from=IMD,values_from=c(mean,median)) %>%
      #calculate the flat and % differences between the deciles
      mutate(mean_flat_difference=mean_10-mean_1,
             mean_percentage_differnce=mean_flat_difference/mean_1,
             median_flat_difference=median_10-median_1,
             median_percentage_differnce=median_flat_difference/median_1,) %>%

      mutate(year=year)


    mean_regression <- modelled_pollutant %>% do(tidy(lm(poll_mean~IMD, data=.))) %>%
      #pivot out the stats so each source is on one row
      pivot_wider(names_from=term,
                  values_from=c(estimate,p.value,std.error,statistic)) %>%

      #rename the columns to avoid confusion
      rename(intercept="estimate_(Intercept)", gradient=estimate_IMD) %>%

      #calculate the intercepts
      mutate(mean_line_1=intercept+gradient,
             mean_line_10=intercept+10*gradient,
             flat_mean_regression_differnce=mean_line_10-mean_line_1,
             percentage_mean_regression=flat_mean_regression_differnce/mean_line_1) %>%
      mutate(year=year)



    #calculate the median for each decile
    median_values <- modelled_pollutant  %>% group_by(IMD) %>%
      summarise(median=median(poll_mean))
    #Get the r2 value through shenanigans
    medarr <- median_values %>%
      #get the tabulated stats from regression on the median data
      do(glance(lm(median~IMD, data=.)))
    #create a linear model for use in the next part
    median_regression <- median_values %>%
      #get the tabulated stats from regression on the median data
      do(tidy(lm(median~IMD, data=.)))%>%
      pivot_wider(names_from=term,
                  values_from=c(estimate,
                                p.value,
                                std.error,
                                statistic)) %>%
      #rename the columns to avoid issues
      rename(intercept="estimate_(Intercept)", gradient=estimate_IMD
      ) %>%

      bind_cols(medarr$r.squared) %>%

      #calculate the intercepts
      mutate(median_line_1=intercept+gradient,
             median_line_10=intercept+10*gradient,
             flat_median_regression_differnce=median_line_10-median_line_1,
             percentage_median_regression=flat_median_regression_differnce/median_line_1,
             year=year)


    #calculate the value of the linear model at each point

    #Meld all three point values

    output <- inner_join(point_summary,mean_regression,by="year") %>%
      inner_join(median_regression,by="year")

    if(exists("amalgm")==FALSE){
      amalgm <- output}else{
        amalgm <- bind_rows(amalgm,output)
      }


    year <- year+1
  }

  write.csv(x=amalgm,
            file=paste0(output_path,"yearly ",pollutant," emissions.csv"))

  combined_years <- read.csv(file=paste0(output_path,"yearly ",pollutant," emissions.csv"))
  }else{
  combined_years <- read.csv(file=premade)
}
  flatdiff <- ggplot(data=combined_years)+

    aes(x=year)+

    geom_line(aes(y=flat_mean_regression_differnce,
                  colour="Mean"))+

    geom_line(aes(y=flat_median_regression_differnce,
                  colour="Median"))+

    labs(xaxis="Year",
         yaxis=bquote("Difference between the value of a linear regression line for the most and least deprived deciles/ "~
                      mu~
                        "g "~m^"-3")
    )

  graph_saver(filename= paste0(output_path,"temporal inequality in ",pollutant,"flat values.png"),
              plot=flatdiff,
              file_format = "agg_png",
              type=5,
              scaling=0.7)

  percdiff <- ggplot(data=combined_years)+

    aes(x=year)+

    geom_line(aes(y=percentage_mean_regression*100,
                  colour="Mean"))+

    geom_line(aes(y=percentage_median_regression*100,
                  colour="Median"))+

    labs(xaxis="Year",
         yaxis=paste0("Percentage difference between most and least deprived deciles by linear regression"))
  graph_saver(filename= paste0(output_path,"temporal inequality in ",pollutant,"percentage values.png"),
              plot=percdiff,
              file_format = "agg_png",
              type=5,
              scaling=0.7)

onevsten <- ggplot(data=combined_years)+
  aes(x=year)+
  geom_line(aes(y=mean_1,
                colour="Most deprived",
                linetype="Mean")
  )+

  geom_line(aes(y=mean_10,
                colour="Least deprived",
                linetype="Mean")
  )+
geom_line(aes(y=median_1,
              colour="Most deprived",
              linetype="Median")
)+

  geom_line(aes(y=median_10,
                colour="Least deprived",
                linetype="Median")
  )+

  scale_colour_manual(values = c("Most deprived"="#FB8022FF","Least deprived"="royalblue"),
                      name= "Decile represented")+

  scale_linetype_discrete(name="Average used")+

  labs(x="Year",
       y=paste0("Background ",pollutant," levels")
       )

graph_saver(filename= paste0(output_path,"difference between ",pollutant," background for most and least deprived.png"),
            plot=onevsten,
            file_format = "agg_png",
            type=5,
            scaling=0.7)
}
