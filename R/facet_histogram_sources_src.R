#' Create a faceted histogram showing all but the top 1% of the nox emission
#' average faceted by emission sector
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used
#'
#' @param pollutant the pollutant investigated, used to name axis
#'
#' @param year the year the data is from, used to name axis
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' LSOA_pollutant_histo(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

facet_histogram_sources_src <- function(prawn_path,pollutant="NOx",year=2020){

raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)

pollutant <- "NOx"
year <- 2020
#Make the data long to eneble grouping by source
end_of_sources <- which(colnames(raw)=="LSOA11CD")-1

source_list <- colnames(raw)[c(1:end_of_sources)]

long_data <- raw %>%
  pivot_longer(
    cols=all_of(c(source_list,"Point sources")),
    names_to = "Emission_source",
    values_to = "emissions") %>% group_by(Emission_source)%>%
  mutate(emissions=replace_na(emissions,0))

axis_key <- long_data %>% summarise(min=quantile(emissions,probs=0),bound=quantile(emissions,probs=0.99)) %>% group_by(Emission_source) %>% tibble

hmm <- inner_join(long_data %>% dplyr::select(Emission_source,emissions),axis_key, by="Emission_source") %>% filter(emissions<=bound)

histo <- ggplot(data=hmm)+

  geom_histogram(bins=80,aes(x=emissions))+

  coord_cartesian(expand=FALSE)+
labs(x=bquote("Average "~.(pollutant)~"emissions for an LSOA in "~.(year)~"/ tonnes "~km^"-2"),
       y="Frequency")+

  facet_wrap(~Emission_source,scale="free")

histo
}
