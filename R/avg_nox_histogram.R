#' Create a histogram of the average NOx per LSOA with the median and mean values labelled

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant used, shows in axis title, defaults to NOx
#'
#' @param year The year of measurements, used in axis title, defaults to 2020

#'
#' @keywords faceted, sources
#' @export
#' @examples
#' avg_nox_histogram()
avg_nox_histogram <- function(prawn_path, pollutant="NOx", year =2020){

  raw <- read.csv(prawn_path,
                  row.names=1,
                  check.names=FALSE)

  histo <- ggplot(data=raw)+

    geom_histogram(bins=80,aes(x=Total))+

    scale_x_continuous(limits = c(0,50),expand=c(0,0))+

    geom_vline(aes(xintercept = mean(Total),colour="Mean"),,size=2)+

    geom_vline(aes(xintercept = median(Total),colour="Median"),,size=2)+

    scale_colour_manual(name="Averages",
                        values=c("Mean"="orange","Median"="blue"),
                        breaks=c("Mean","Median"),
                        guide="legend",)+

    coord_cartesian(expand=FALSE)+

    labs(x=bquote("Average "~.(pollutant)~"emissions for an LSOA in "~.(year)~"/ tonnes "~km^2),
         y="Frequency")

  caveat <- nrow(raw %>% filter(Total>=50))

  output <- list(histo,caveat)

  output

}

