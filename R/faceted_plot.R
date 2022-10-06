#' Creates a graph wit a little window showing every item in the group separately
#' e.g. if grouped by city there would be a tile for every city.
#' @param prawn_path the data source to use, should be a quoted csv filepath
#'
#' @param group The group to facet by, can be TCITY15NM to group by city or
#' anything else to group by county/UA, this is due to jerry rigging a path
#' around non standard evaluation
#'
#' @param pollutant the name of the pollutant, used to name the graphs
#' @export
#' faceted_plot

faceted_plot <- function(prawn_path,group,pollutant){

filtered_data <- read.csv(prawn_path) %>% tibble()

City_profile <- ggplot(data=filtered_data)+
  aes(x=IMD,
      y=Total)+

  scale_x_continuous(
    breaks=c(1:10),
    minor_breaks = FALSE,
    expand = expansion(mult=0,add=0))+


  labs(x="IMD decile",
       y=paste0(pollutant," emissions"),
       title=paste0(" ", pollutant, "emission"))+

  #Plot the line of best fit for the mean
  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(color='Mean'))+

  #Plot a line passing through the mean at each decile
  geom_line(stat="summary",
            aes(color='Mean'),
            fun=mean)+

  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(color='Median'),
                size =1,
                formula=y~x,)+

  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(color='Median'))+

#Plot a regression line for the whole UK for comparison
geom_smooth(data=read.csv(prawn_path) %>% tibble %>% dplyr::select(-c(TCITY15NM,Area))
            , aes(
              x=IMD,
              y=Total,
              color='UK mean'),
            method="lm",
            formula=y~x,
            se=FALSE)+

  scale_colour_manual(name="Colour",
                      breaks = c('Mean','Median','UK mean'),
                      values=c('Mean'='blue','Median'='red','UK mean'='black'))+

  labs(x="IMD decile where 10 is least deprived",
       y=bquote("Average "~.(pollutant)~"emissions/ tonnes "~km^2))

#Add on the facets conditional to the group variable
if (group=="TCITY15NM"){
  City_profile <- City_profile+ facet_wrap(~TCITY15NM,scales="free_y")
  }else{
    City_profile <- City_profile+
    facet_wrap(~Area,scales="free_y")}

City_profile
}
