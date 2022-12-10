#' A function for creating a graphic summarising a city
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param shapefile_path The filepath for the shapefile that is to be used
#'
#' @param targets The name of the thing you're trying to isolate e.g. "Manchester"
#' can also be a vector of valid values
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @param output_path The filepath to output to, a folder will be created at
#' this location which contains all the graphs produced by this code. Defaults
#' to FALSE

#' @keywords heatmap, graph,
#' @export
#' @examples
#' area_summary()
#'
area_summary <- function(prawn_path,
                         shapefile_path,
                         targets,
                         pollutant,
                         output_path=FALSE){



  #Reads in the demographic and pollution data
  raw_data <- read.csv(file=prawn_path,
                        row.names=1,
                        check.names=FALSE) %>% tibble()

  #Reads in the shapefiles
  raw_shapefile <- st_read(shapefile_path)

  #Takes the subset of the data where the city name matches the targets
  filtered_data <- filter(raw_data,TCITY15NM %in% targets)

  if (dim(filtered_data)[1]==0){
    stop(paste0("Nothing matches " ,targets," in the data, check for typos"))
  }



  stitched_shapefile <- inner_join(filtered_data,raw_shapefile, by="LSOA11CD") %>%
    mutate(IMD=as.factor(IMD))


# Graph creation ----------------------------------------------------------
    #Create a map showing hpw the population is distrubuted between the deciles in the chosen area,
    Decile_distribution <- ggplot()+geom_sf(data=stitched_shapefile,size=0.05)+
      aes(fill =IMD,geometry=geometry)+
      scale_fill_viridis_d()+
      labs(title=paste0("IMD distribution"))+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())+
      guides(fill=guide_legend(ncol=2, byrow=FALSE))

    #Create a heatmap for the Nox in the area
    Pollutant_distribution <- ggplot()+geom_sf(data=stitched_shapefile,size =0.05)+
      aes(fill = Total,geometry=geometry)+
      scale_fill_continuous(type ="viridis",direction =-1)+
      labs(title=paste0(pollutant," distribution"))+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())

    #Create a histogram showing the prevalence of each decile in the area
    City_histogram <- ggplot(data=stitched_shapefile)+

      aes(x=IMD,fill=IMD )+
      geom_bar()+
      labs(x="IMD decile",
           title=paste0(targets," IMD histogram"))+
      scale_x_discrete(
        breaks=c(1:10),
        expand = expansion(mult=0,add=0))+
      scale_fill_viridis_d()+
      guides(fill=guide_legend(ncol=2, byrow=FALSE))
#This is where the largest graph (mean/median and UK average starts)
    #Create a graph showing the relationship between NOx and decile within the chosen area
    City_profile <- ggplot(data=filtered_data)+
      aes(x=IMD,
          y=Total)+

      scale_x_continuous(
        breaks=c(1:10),
        minor_breaks = FALSE,
        expand = expansion(mult=0,add=0))+


      labs(x="IMD decile",
           y=paste0(pollutant," emissions"),
           title=paste0(targets," ", pollutant, "emission"))+

      #Plot the line of best fit for the mean
      geom_smooth(method="lm",
                  formula=y~x,
                  se=FALSE,
                  show.legend=FALSE,
                  aes(color='Mean'))+

      #Plot a line passing through the mean at each decile
      geom_line(stat="summary",
                fun=mean,
                aes(color='Mean'
                    ))+

      #Plot the line of best fit for the median
      geom_quantile(quantiles=0.5,
                    aes(color='Median'),
                    size =1,
                    formula=y~x,
                    na.rm=TRUE)+

      #Plot a line through the medians for each decile
      geom_line(stat="summary",
                fun=median,
                aes(color='Median'))+

      #Plot a regression line for the whole UK for comparison
      geom_smooth(data=read.csv(prawn_path)
                  , aes(
                    x=IMD,
                    y=Total,
                    color='UK Average'),
                  method="lm",
                  formula=y~x,
                  se=FALSE,
                  show.legend = FALSE)+

      #An extra line showing the UK average at each decile for comparison purposes
      geom_line(data=read.csv(prawn_path),stat="summary" ,
                fun=mean,
                aes(
        x=IMD,
        y=Total,colour='UK Average'),
        show.legend = FALSE
      )+

      scale_colour_manual(name="Line type",
                          breaks = c('Mean','Median','UK Average'),
                          values=c('Mean'='royalblue','Median'="#FB8022FF",'UK Average'='black'))


    #Creates a cumulative distribution plot showing what fractions of each decile are exposed to less than the amount of NOx on the axis
    city_freq <- ggplot(data=stitched_shapefile)+
      aes(x=Total,group=IMD,colour=IMD)+
      scale_colour_viridis_d(option="turbo")+
      stat_ecdf(
      )+
      labs(x= paste0(pollutant," emissions"),
           y= paste0("Fraction exposed to at least this much ",pollutant),
           title=paste0(" Cumulative distribution  of ",pollutant," emissions"))+
      scale_y_continuous(
        expand = expansion(mult=0,add=0),
      )+
      scale_x_discrete(
        expand = expansion(mult=0,add=0),
      )
    city_freq


    #Create a pivoted copy of the data so the sources can be graphed as separate variables
    long_chunk <- filtered_data %>% tibble() %>% mutate(point_sources=Total-Total_no_points)%>%
      pivot_longer(
        cols=c("Agricultural","Domestic combustion","Energy production",
               "Industrial combustion","Industrial production","Natural",
               "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
               ,"Waste treatment and disposal","point_sources"),
        names_to = "Emission_source",
        values_to = "emissions")
    long_chunk$Emission_source <- factor(long_chunk$Emission_source)

    long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,emissions,mean,.desc=TRUE))

    city_sources <- Decile_vs_emission_by_variable(
      active_stack = long_chunk,
      chosen_decile = IMD,
      chosen_grouping = Emission_source,
      xaxis = "IMD Decile",
      yaxis = "NOx emissions",
      title = paste0("Source breakdown for ",targets),
      chosen_variable = emissions
    )+
      geom_quantile(quantiles=0.5,linetype=2)

    #Create the gridded oputput object
    output <- ggarrange(Decile_distribution,Pollutant_distribution,City_histogram,City_profile,city_sources,city_freq,nrow=3,ncol=2) %>%
      annotate_figure(top=text_grob(paste0("Summary of ",pollutant," exposure in ",targets)))

# Archive results ---------------------------------------------------------



if (output_path!=FALSE){

    if (file.exists(output_path)) {

      ggsave(filename=paste0(output_path,"/Nox in ",targets[index]),
             plot=last_plot(),
             units = "mm",height = 160,width=160,
             device="png")

    } else {

      dir.create(output_path)

      ggsave(filename=paste0(output_path,"/Nox in ",targets[index]),
             plot=last_plot(),
             units = "mm",height = 160,width=160,
             device="png")
    }
}

    output
}


