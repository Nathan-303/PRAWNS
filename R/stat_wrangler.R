#'this function returns a tibble containing the absolute and percentage difference
#'between the mean and median of the intercept and actual value at the top and
#'bottom deciles, operates from a file or an object depending on if prawn or
#'input path is used
#'@param prawn_path the path for the data source, defaults to FALSE this should
#'reference a csv file
#'
#'@param input_path the object to use as a data source defaults to FALSE this
#'should be the name of an existing object
#'
#' @keywords data
#' @export

#'
#' @examples
#' stat_wrangler()
#'
#'

stat_wrangler <- function(prawn_path=FALSE, input_path=FALSE){
  #read the input file if a filepath is given
  if (prawn_path != FALSE){
    data <- read.csv(prawn_path)
  }

  #reference an existing object if an object name is given
  if (input_path != FALSE){
    data <- input_path
  }

  #get the summary stats for deciles 1 and 10
  point_summary <- filter(data, IMD %in% c(1,10)) %>%
    #add grouping for summarise
    group_by(IMD) %>%
    #calculate the mean and median for each decile
    summarise(mean=mean(total),median=median(total))

  lm_summary <-
}

