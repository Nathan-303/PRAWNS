#' Create a histogram showing the emission gradient for each area (city or
#' county/UA) and whether it has a p value of below 0.05

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param area_type The type of area to group by, either city or county/UA
#'
#' @param pollutant The pollutant being investigated, used for graph labelling

#'
#' @keywords faceted, sources
#' @export
#' @examples
#' plot_area_gradients()
cartesian_area_gradients_src <- function(prawn_path,area_type,pollutant){

raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)
if (area_type == "County/UA"){
  refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,Area) %>% group_by(Area)
  }else{
    refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,TCITY15NM) %>% group_by(TCITY15NM)
}



coeffs <- refined %>%
  #get the rsquared
  do(tidy(lm(Total~IMD, data=.))) %>%

  filter(term=="IMD")

curiosity <- coeffs %>% filter(p.value<=0.05)

test <- ggplot()+
  geom_histogram(data=coeffs,
                 aes(x=estimate,fill="royalblue"),bins=50)+

  geom_histogram(data=curiosity,
                 aes(x=estimate,
                     fill="#FB8022FF"),
                 bins=50)+

  scale_fill_identity(guide="legend",labels=c("p>0.05","p<0.05"),breaks=c("royalblue","#FB8022FF"),name="p value")+
  scale_y_continuous(expand=expansion(add =c(0,1)))+
  labs(x=bquote("Gradient / tonnes "~pollutant~" "~km^"-2"~IMD^-1, ),
       y="Frequency"
  )
test
}
