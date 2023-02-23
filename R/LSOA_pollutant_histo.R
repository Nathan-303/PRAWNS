#' Create a faceted histogram showing all but the top 1% of the nox emission average across lsoas

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'

#'
#' @keywords faceted, sources
#' @export
#' @examples
#' LSOA_pollutant_histo()

LSOA_pollutant_histo <- function(prawn_path,pollutant="NOx",year=2020){

raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)

pollutant <- "NOx"
year <- 2020
#Make the data long to eneble grouping by source
long_data <- raw %>% pivot_longer(
  cols=c("Agricultural","Domestic combustion","Energy production",
         "Industrial combustion","Industrial production","Natural",
         "Other transport and mobile machinery","Road transport","Solvents","Total"
         ,"Waste treatment and disposal","Point sources"),
  names_to = "Emission_source",
  values_to = "emissions") %>%
  group_by(Emission_source) %>%
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
