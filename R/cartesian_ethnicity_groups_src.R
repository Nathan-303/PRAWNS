#' Bind ethnicity data to the prawn and create graphs based on it
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant being investigated, used in graph titles
#'
#' @param year The year being investigated, used in graph titles
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)

cartesian_ethnicity_groups_src <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  rename(`Asian, Asian British or\nAsian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, \nBlack Welsh, Caribbean\nor African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple \nethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
         `Ethnic group: White`,
         `Other ethnic\ngroup`=
         `Ethnic group: Other ethnic group`
         ) %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or\nAsian Welsh`,
      `Black, Black British, \nBlack Welsh, Caribbean\nor African`,
      `Mixed or Multiple \nethnic groups`,
      `White`,
      `Other ethnic\ngroup`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Ethnic group`)

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=10)) %>%
  group_by(`Ethnic group`,Diversity_quintile)


plottable <- edata %>% inner_join(
  x=data,
  y=foray,
  by=c("LSOA11CD"="geography code")
) %>%
mutate(tile=ntile(Percentage,8))

area_rank <- plottable %>%
  dplyr::select(Total,`Ethnic group`,Percentage) %>%
  group_by(`Ethnic group`) %>%
  summarise(Mean =mean(Total),
                                        Median=median(Total),
                                        perc=mean(Percentage)) %>%
  pivot_longer(c(Mean, Median), names_to = paste0(pollutant,"_average"),values_to = "token") %>%
  group_by(NOx_average) %>%
  mutate(tile=ntile(perc,8))

boxrigger <- plottable %>% group_by(`Ethnic group`,tile) %>% summarise()
boxxy <- plottable %>% group_by(`Ethnic group`,tile) %>% summarise(q90=quantile(Total,c(0.90)),
                                                                    q10=quantile(Total,c(0.10)),
                                                                    q1=quantile(Total,c(0.25)),
                                                                    q3=quantile(Total,c(0.75)),
                                                                    med=quantile(Total,c(0.5)),
                                                                   maxper=max(Percentage),
                                                                   minper=min(Percentage)) %>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "emissions") %>%
  mutate(center=(maxper+minper)/2)
#Plot a faceted graph

output <- ggplot(data=plottable
)+

  geom_boxplot(data=boxxy,
               inherit.aes=FALSE,
               aes(x=center,
                   y=emissions,
                   group=tile),
               width=0.2,
               coef=10000000000000000000000000000000000000000000000000000000000000)+

  labs(x=paste0("Decile where 10 contains the most people within the group"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  scale_colour_viridis_d()+

  guides(fill = guide_legend(byrow = TRUE))+

  facet_wrap(~`Ethnic group`,scale="free_x")

output

}
