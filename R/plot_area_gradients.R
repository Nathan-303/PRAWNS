#' Create a histogram showign the gradients for every county/ua or city

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param area_type They type of area to group by, either city or county/UA

#'
#' @keywords faceted, sources
#' @export
#' @examples
#' plot_area_gradients()
plot_area_gradients <- function(prawn_path,area_type){

raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)
if (area_type == "County/UA"){
  refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,Area) %>% group_by(Area)
  }else{
    refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,TCITY15NM)
}



coeffs <- refined %>%
  #get the rsquared
  do(tidy(lm(Total~IMD, data=.))) %>%

  filter(term=="IMD")

curiosity <- coeffs %>% filter(p.value<=0.05)

test <- ggplot()+
  geom_histogram(data=coeffs,
                 aes(x=estimate),bins=50)+

  geom_histogram(data=curiosity,
                 aes(x=estimate),
                 fill="orange",bins=50)+

  coord_cartesian(ylim=c(0,14),expand=FALSE)

test
}
