#' Create a graph showing emissions from the major source clusters  for the most
#' deprived 20% of LSOAs

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param year The year of the data, used in graph annotations
#'
#' @param area_type The type of area investigated, can be "City" or "County/UA"
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' cartesian_area_for_most_deprived_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)

cartesian_area_for_most_deprived_src <- function(prawn_path,pollutant,year){
  data <- read.csv(prawn_path,
                   row.names=1,
                   check.names=FALSE)

  reformed_data <- data %>%
    #aggregate all non
    mutate(`Industrial\nsources`=Solvents+
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
      cols=c("Road transport","Total","Other sources","Industrial\nsources"),
      names_to = "Emission_source",
      values_to = "emissions") %>%
    #sort into thirds based on IMD rank
  mutate(expanse=expanse/10^6,
         third=ntile(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,5),
         size_decile=ntile(expanse,10)) %>%

    filter(third==1) %>%

    group_by(size_decile,Emission_source)


axticks <- quantile(reformed_data$expanse,probs=seq(0.1,1,0.1)) %>% signif(2) %>% as.numeric()

axticks2 <- paste0(c(1:10),"\n",axticks,"km^2")
almalgm <- reformed_data %>% summarise(emissions=mean(emissions))

  output <- ggplot(data=reformed_data %>% summarise(emissions=mean(emissions)))+

    aes(x=size_decile,y=emissions)+

    geom_line(aes(colour=Emission_source),
              linewidth=2)+

    scale_x_continuous(breaks=c(1:10),
                       labels=paste0(c(1:10),"\n",axticks),
                       expand=expansion(0,0),
                       minor_breaks = FALSE)+

    labs(x=expression(atop("LSOA expanse decile","Area of the largest LSOA in this decile /"~km^2)),
              y=bquote("Mean "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"))+

    scale_colour_manual(breaks=c("Total","Road transport","Other sources","Industrial\nsources"),
                        values=c("black","royalblue","#FB8022FF","deeppink2"),
                        name="Emission source")+

    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.y = element_text(hjust=0.95))+

    guides(colour=guide_legend(override.aes=list(linewidth=2),fill=NA))




  output


}

