#' A function for creating a graphic summarising a city through tiling smaller
#' graphs
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param shapefile_path The filepath for the shapefile that is to be used
#'
#' @param targets The name of the thing you're trying to isolate e.g.
#' "Manchester"
#' can also be a vector of valid values
#'
#' @param pollutant The name of the pollutant that's being examined, this is
#' used in the graph names
#'
#' @param output_path The filepath to output to, a folder will be created at
#' this location which contains all the graphs produced by this code. Defaults
#' to FALSE

#' @keywords heatmap, graph,
#'
#' @export
#'
#'@examples
#'focus_area_multiplot_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

focus_area_multiplot_src <- function(prawn_path,
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
    guides(fill=guide_legend(ncol=2))+
    theme(axis.text = element_text(size = 5))
      # theme(axis.text.x = element_blank(),
      #       axis.text.y = element_blank(),
      #       axis.ticks = element_blank(),
      #       legend.position="none")
  #calculate the value to wash out the scale at
  scalecap <- quantile(filtered_data$Total,0.95)
    #Create a heatmap for the Nox in the area
    Pollutant_distribution <- ggplot()+geom_sf(data=stitched_shapefile %>%
                                                 mutate(Total=case_when(Total>scalecap~scalecap,
                                                                        Total<scalecap~Total)),
                                               size =0.05)+
      aes(fill = Total,geometry=geometry)+
      scale_fill_continuous(type ="viridis",direction =-1)+
      theme(axis.text = element_text(size = 5))
#
#     #Create a histogram showing the prevalence of each decile in the area
#     City_histogram <- ggplot(data=stitched_shapefile)+
#
#       aes(x=IMD,fill=IMD )+
#       geom_bar()+
#       labs(x="IMD decile",
#            title=paste0("IMD scores in ",targets))+
#       scale_x_discrete(
#         breaks=c(1:10),
#         expand = expansion(mult=0,add=0))+
#       scale_fill_viridis_d()+
#      guides(fill=guide_legend(ncol=2, byrow=FALSE))
#This is where the largest graph (mean/median and UK average starts)

    reformed_data <- raw_data %>% filter(!TCITY15NM%in%c("","London")) %>%
      #aggregate some sources so it can be small
      mutate(`Industrial sources`=Solvents+
               `Energy production`+
               `Waste treatment and disposal`+
               `Industrial combustion`+
               `Industrial production`+
               `Point sources`,

             `Other sources`=Natural+
               `Agricultural`+
               `Other transport and mobile machinery`+
               `Domestic combustion`) %>%

      pivot_longer(
        cols=c("Road transport","Total","Other sources","Industrial sources"),
        names_to = "Emission_source",
        values_to = "emissions")

    source_summary <- ggplot(reformed_data %>% filter(TCITY15NM==targets))+
      geom_smooth(aes(IMD,
                      emissions,
                      colour = fct_reorder(Emission_source,emissions,mean,.desc=TRUE),
                      linetype ="Linear regression"),
                  method = "lm",
                  se=FALSE)+

      geom_line(data=reformed_data %>%
                  group_by(IMD,Emission_source)%>%
                  filter(TCITY15NM==targets)%>%
                  summarise(emissions=mean(emissions)),
                aes(x=IMD,
                    y=emissions,
                    colour = Emission_source,
                    linetype="Average points"))+

      geom_line(data=reformed_data %>%
                  group_by(IMD,Emission_source)%>%
                  filter(Emission_source=="Total")%>%
                  summarise(emissions=mean(emissions)),
                aes(x=IMD,
                    y=emissions,
                    colour = Emission_source,
                    linetype="National average"))+

      labs(x="IMD decile where 10 is least deprived",
           y=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
           colour="Emission source")+

      scale_x_continuous(
        breaks=c(1:10),
        expand = expansion(mult=0,add=0),
        minor_breaks = FALSE)+

      scale_y_continuous(expand=c(0,0))+

      scale_linetype_manual(values=c("Average points"="dashed",
                                     "Linear regression"="solid",
                                     "National average"="dotted"),
                            name="Values plotted")+

      #
      #     scale_linewidth(name= "Line plotted",
      #                         labels=c("Average points","Linear regression"),
      #                         guide="legend")+

      scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF"))+

      guides(colour=guide_legend(override.aes=list(size=3)),

             linetype=guide_legend(override.aes =list(colour="black",
                                                      shape=c(NA,NA,NA),
                                                      linewidth=c(1,1,1)))
      )+

      theme(panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    source_summary
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
           title=paste0(pollutant, " emissions in ",targets))+

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
                    color='England mean'),
                  method="lm",
                  formula=y~x,
                  se=FALSE,
                  show.legend = FALSE)+

      #An extra line showing the UK average at each decile for comparison purposes
      geom_line(data=read.csv(prawn_path),stat="summary" ,
                fun=mean,
                aes(
        x=IMD,
        y=Total,colour='England mean'),
        show.legend = FALSE
      )+

      scale_colour_manual(name="Line type",
                          breaks = c('Mean','Median','England mean'),
                          values=c('Mean'='royalblue','Median'="#FB8022FF",'England mean'='black'))


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
    long_chunk <- filtered_data %>% tibble() %>%
      pivot_longer(
        cols=c("Agricultural","Domestic combustion","Energy production",
               "Industrial combustion","Industrial production","Natural",
               "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
               ,"Waste treatment and disposal","Point sources"),
        names_to = "Emission_source",
        values_to = "emissions")
    long_chunk$Emission_source <- factor(long_chunk$Emission_source)

    long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,emissions,mean,.desc=TRUE))

    city_sources <- facet_sources_src(pollutant="NOx",
                                      year=2019,
                                      input_prawn=filtered_data)

    #Create the gridded oputput object
# Archive results ---------------------------------------------------------

#do some destination management
    if(!file.exists(paste0("City emissions/",targets))){
      dir.create(paste0("City emissions/",targets))
      dir.create(paste0("City emissions/",targets,"/",pollutant))
    }else{
      if(!file.exists(paste0("City emissions/",targets,"/",pollutant))){
        dir.create(paste0("City emissions/",targets,"/",pollutant))
      }
    }

process_graph_saver(plot = city_sources,
                    filename = paste0("City emissions/",targets,"/",pollutant,
                                      "/comprehensive source summary.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 1
                    )

process_graph_saver(plot = Pollutant_distribution,
                    filename = paste0("City emissions/",targets,"/",pollutant,
                                      "/pollutant_map.png"),
                    file_format = "agg_png",
                    type = 1,
                    scaling = 1
)

process_graph_saver(plot = source_summary,
                    filename = paste0("City emissions/",targets,"/",pollutant,
                                      "/grouped source summary.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.6
)

process_graph_saver(plot = Decile_distribution,
                    filename = paste0("City emissions/",targets,"/",pollutant,
                                      "/deprivation map.png"),
                    file_format = "agg_png",
                    type = 1,
                    scaling = 0.8
)
}


