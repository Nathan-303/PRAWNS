#' Create a graph showing how size affects emissions

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' size_poker()
#'

size_poker <- function(prawn_path,pollutant,year){
  data <- read.csv(prawn_path,
                   row.names=1,
                   check.names=FALSE)

  reformed_data <- data %>%
    #aggregate all non
    mutate(`Other sources`=Solvents+
             Natural+
             `Energy production`+
             `Waste treatment and disposal`+
             `Agricultural`+
             `Point sources`+
             `Domestic combustion`+
             `Industrial combustion`+`
           Industrial production`+
           `Other transport and mobile machinery`) %>%

    pivot_longer(
      cols=c("Road transport","Total","Other sources"),
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

    geom_line(aes(colour=Emission_source))+

    scale_x_continuous(breaks=c(1:10),
                       labels=paste0(c(1:10),"\n",axticks),
                       expand=expansion(0,0))+

    labs(x=expression(atop("LSOA expanse decile","Area of the largest LSOA in this decile /"~km^2)),
              y=bquote("Mean "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2))+

    scale_colour_manual(breaks=c("Total","Road transport","Other sources"),
                        values=c("black","blue","orange"),
                        name="Emission source")



  output


}

