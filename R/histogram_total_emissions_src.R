#' Create a histogram of the average total emissions with the median and mean
#' values labelled

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant used, shows in axis title, defaults to NOx
#'
#' @param year The year of measurements, used in axis title, defaults to 2020
#'
#' @param input_prawn Uses a prawns object already present in R rther than reading a csv
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' histogram_total_emissions_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

histogram_total_emissions_src <- function(prawn_path="blank", pollutant, year,input_prawn="blank"){

  if (prawn_path!="blank"){
    raw <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble()
  }else{
    raw <- input_prawn
  }

  raw <- raw %>%

    mutate(upperbound=quantile(Total,0.99)) %>%

    filter(Total<=upperbound)

  histo <- ggplot(data=raw)+

    geom_histogram(bins=80,aes(x=Total),boundary=0)+

    geom_vline(aes(xintercept = mean(Total),colour="Mean"),size=1,key_glyph="path")+

    geom_vline(aes(xintercept = median(Total),colour="Median"),size=1,key_glyph="path")+

    scale_colour_manual(name="Averages",
                        values=c("Mean"="#FB8022FF","Median"="royalblue"),
                        breaks=c("Mean","Median"),
                        guide=guide_legend(override.aes = list(size=c(2,2))))+

    coord_cartesian(expand=FALSE)+

    labs(x=bquote("Average "~.(pollutant)~"emissions for an LSOA in "~.(year)~"/ tonnes "~km^"-2"),
         y="Frequency")
  caveat <- "top1percent"

  output <- list(histo,caveat)

  output

}

