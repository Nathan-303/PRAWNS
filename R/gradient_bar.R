#'This function creates a pie chart showing how each source sector contributes
#'to the overall gradient as a percentage, it works off a file created by
#'stat_wrangler and is currently designed to work within the environment of a
#'bulk_processor call
#'@param pollutant the polutant being examined, used in the graph title
#'
#'@param input_path the object to use as a data source defaults to FALSE this
#'should be the name of an existing object
#'
#' @keywords data
#' @export
#'
#' @examples
#' gradient_bar()


gradient_bar <- function(pollutant, input_path){
  raw <- read.csv(input_path,
                  row.names=1,
                  check.names=FALSE)

output <- ggplot(data=raw %>% filter(Emission_source!="Total"),
       aes(x=fct_reorder(Emission_source,mean_flat_difference),
           y=mean_flat_difference,
           fill=fct_reorder(Emission_source,mean_flat_difference)))+
  geom_col()+
  scale_fill_viridis_d()+
  labs(title = paste0("Using a linear model to predict the scope of the difference in emissions"),
       y=paste0("The change in predicted ", pollutant," concentration"))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

output
}
