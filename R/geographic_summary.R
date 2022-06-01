#' A function for creating a graphic summarising a city
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param shape_path The filepath for the shapefile that is to be used
#'
#' @param column_targeted The name of the column to be searched. It is reccomended
#' to use somehtign like city or county/UA that is geographically specific, but
#' any variable can be used
#'
#' @param targets The name of the thing you're trying to isolate e.g. "Manchester"
#' can also be a vector of valid values
#'
#' @param key_variable The variable that is common between the shapefile and csv
#' files specified in data_path, defaults to LSOA19CD
#'
#' @param output_path The filepath to output to, a folder will be created at
#' this location which contains all the graphs produced by this code. Defaults
#' to FALSE

#' @keywords heatmap, graph,
#' @export
#' @examples
#' geographic_summary()
#'
geographic_summary <- function(prawn_path,
                               shape_path,
                               column_targeted,
                               targets,
                               key_variable="LSOA19CD",
                               output_path=FALSE){

  #Reads in the demographic and pollution data
  raw_data <- read.csv(file={{prawn_path}},
                        row.names=1,
                        check.names=FALSE) %>% tibble()

  #Reads in the shapefiles
  raw_shapefiles <- vect(x=shape_path)

  #Iterates separately for each entry in targets


  #Takes the subset of the data where the value in column_targeted matches target
  filtered_data <- subset(raw_data, subset= {{column_targeted}} == targets)

  filtered_shapefiles <- subset(raw_shapefiles,subset = raw_shapefiles$key_variable %in% filtered_data$key_variable )

  #Stitch the data you want onto the shapefile, this section has assumptions about the column names, the continous and factored deciles are for graphing capacity
  stitched_shapefile <- filtered_shapefiles
    stitched_shapefile$LSOA_Decile <- filtered_data$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs. %>% factor()
    stitched_shapefile$NOx <- filtered_data$Total
    stitched_shapefile$Cont_decile <- filtered_data$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.

# Graph creation ----------------------------------------------------------
    #Create a map showing hpw the population is distrubuted between the deciles in the chosen area,
    Decile_distribution <- ggplot()+geom_sf(data=stitched_shapefile,size=0.05)+
      aes(fill =LSOA_Decile)+
      scale_fill_viridis_d()+
      labs(title=paste0(custom_name," IMD distribution"))+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())+
      guides(fill=guide_legend(ncol=2, byrow=FALSE))

    #Create a heatmap for the Nox in the area
    Pollutant_distribution <- ggplot()+geom_sf(data=stitched_shapefile,size =0.05)+
      aes(fill = NOx)+
      scale_fill_continuous(type ="viridis",direction =-1)+
      labs(title=paste0(custom_name," NOx distribution"))+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())

    #Create a histogram showing the prevalence of each decile in the area
    City_histogram <- ggplot(data=stitched_shapefile)+

      aes(x=Cont_decile,fill=LSOA_Decile )+
      geom_bar()+
      labs(x="IMD decile",
           title=paste0(custom_name," IMD histogram"))+
      scale_x_continuous(
        breaks=c(1:10),
        expand = expansion(mult=0,add=0))+
      scale_fill_viridis_d()+
      guides(fill=guide_legend(ncol=2, byrow=FALSE))

    #Create a graph showing the relationship between NOx and decile within the chosen area
    City_profile <- ggplot(data=stitched_shapefile)+
      aes(x=Cont_decile,
          y=NOx,
      )+

      coord_cartesian(xlim = c(1, 10),
                      ylim= c(0,50))+

      scale_x_continuous(
        breaks=c(1:10),
        expand = expansion(mult=0,add=0))+

      scale_y_continuous(
        expand = expansion(mult=0,add=0))+

      labs(x="IMD decile",
           y="NOx emissions",
           title=paste0(custom_name," NOx emission"))+

      geom_line(stat="summary",color="blue")+

      geom_smooth(method="lm",formula=y~x,se=TRUE,show.legend=FALSE, aes(color='Mean'))+

      geom_boxplot(aes(x=Cont_decile,y=NOx,group=Cont_decile,color='Median'),width=0.5,alpha=0)+

      geom_quantile(quantiles=0.5,aes(color='Median'),size =1)+

      geom_smooth(data=read.csv(prawn_path)
                  , aes(
                    x=Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
                    y=Total,
                    color='UK Average'),
                  method="lm",
                  formula=y~x,
                  se=FALSE,
                  show.legend = FALSE)+

      #An extra line showing the UK average for comparison purposes
      geom_line(data=read.csv(prawn_path),stat="summary" , aes(
        x=Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
        y=Total,colour='UK Average'),
        method="lm",
        formula=y~x,
        se=FALSE,
        show.legend = FALSE,
      )+

      scale_colour_manual(name="Line type",
                          breaks = c('Mean','Median','UK Average'),
                          values=c('Mean'='blue','Median'='red','UK Average'='black'))


    #Creates a cumulative distribution plot showing what fractions of each decile are exposed to less than the amount of NOx on the axis
    city_freq <- ggplot(data=stitched_shapefile)+
      aes(x=NOx,group=Cont_decile,colour=LSOA_Decile)+
      scale_colour_viridis_d(option="turbo")+
      stat_ecdf(
      )+coord_cartesian(xlim = c(0, 100))+
      labs(x="NOx emissions",
           y="Fraction exposed to at least this much nox",
           title=paste0(custom_name," Cumulative distribution"))+
      scale_y_continuous(
        expand = expansion(mult=0,add=0),
      )+
      scale_x_continuous(
        expand = expansion(mult=0,add=0),
      )
    city_freq


    #Create a pivoted copy of the data so the sources can be graphed as separate variables
    long_chunk <- stitched_shapefile %>% tibble() %>% mutate(point_sources=Total-Total_no_points)%>%
      pivot_longer(
        cols=c("Agricultural","Domestic combustion","Energy production",
               "Industrial combustion","Industrial production","Natural",
               "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
               ,"Waste treatment and disposal","point_sources"),
        names_to = "Emission_source",
        values_to = "NOx_emissions")
    long_chunk$Emission_source <- factor(long_chunk$Emission_source)

    long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,NOx_emissions,mean,.desc=TRUE))

    city_sources <- Decile_vs_emission_by_variable(
      active_stack = long_chunk,
      chosen_decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
      chosen_grouping = Emission_source,
      xaxis = "IMD Decile",
      yaxis = "NOx emissions",
      title = paste0("Source breakdown for ",city_list[index]),
      chosen_variable = NOx_emissions,
      Pollutant = "NOx"

    )+
      geom_quantile(quantiles=0.5,linetype=2)
    city_summary <- ggarrange(Decile_distribution,Pollutant_distribution,City_histogram,City_profile,city_sources,city_freq,nrow=3,ncol=2) %>%
      annotate_figure(top=text_grob(paste0("Summary of ",pollutant," exposure in ",custom_name)))

# Archive results ---------------------------------------------------------



if (output_path==TRUE){

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
}


