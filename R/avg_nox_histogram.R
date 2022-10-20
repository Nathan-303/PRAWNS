#' Create a histogram showign the gradients for every county/ua or city

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'

#'
#' @keywords faceted, sources
#' @export
#' @examples
#' avg_nox_histogram()
avg_nox_histogram <- function(prawn_path){
  
  raw <- read.csv(prawn_path,
                  row.names=1,
                  check.names=FALSE)
 
  histo <- ggplot(data=raw)+
    
    aes(x=Total)+
    
    geom_histogram(bins=80)+
    
    scale_x_continuous(limits = c(0,50),expand=c(0,0))
   
  caveat <- nrow(raw %>% filter(Total>=50))
  
  output <- list(histo,caveat)
  
  output
  
}

