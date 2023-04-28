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

facet_sources_cartesian_ethnicity_groups_src <- function(prawn_path,pollutant,year){
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
    values_to = "flat_population"
  )


intermediate <- inner_join(data,edata,by=c("LSOA11CD"="geography code"))%>%

  mutate(`Weighted emissions`= Total*flat_population,
         `Weighted deprivation`=IMD*flat_population)


weighted_data <- intermediate %>%

  group_by(LAD19NM,`Ethnic group`) %>%

  summarise(popsum=sum(flat_population),
            emissions_sum=sum(`Weighted emissions`),
            IMD_sum=sum(`Weighted deprivation`)) %>%

  mutate(`Weighted emissions`=emissions_sum/popsum,
         `Weighted deprivation`=IMD_sum/popsum) %>%

  group_by(`Ethnic group`) %>%

  mutate(tile =dplyr::ntile(x=`Weighted deprivation`,
                                  n=10))

wide_table <- weighted_data %>% pivot_wider(
  id_cols = LAD19NM,
  names_from = `Ethnic group`,
  values_from = c(`Weighted emissions`,
                  `Weighted deprivation`)
)

delta_table <- wide_table %>% mutate(
  #calculate the differnce in deprivation
  as_dep=`Weighted deprivation_Asian, Asian British or
Asian Welsh`-`Weighted deprivation_White`,
  bl_dep=`Weighted deprivation_Black, Black British,
Black Welsh, Caribbean
or African`-`Weighted deprivation_White`,
  mx_dep=`Weighted deprivation_Mixed or Multiple
ethnic groups`-`Weighted deprivation_White`,
  other_dep=`Weighted deprivation_Other ethnic
group`-`Weighted deprivation_White`,

  #calculate the differnce in emissions
  as_ems=`Weighted emissions_Asian, Asian British or
Asian Welsh`-`Weighted emissions_White`,
  bl_ems=`Weighted emissions_Black, Black British,
Black Welsh, Caribbean
or African`-`Weighted emissions_White`,
  mx_ems=`Weighted emissions_Mixed or Multiple
ethnic groups`-`Weighted emissions_White`,
  other_ems=`Weighted emissions_Other ethnic
group`-`Weighted emissions_White`,
)

penultimate_pivot <- delta_table %>% pivot_longer(
  cols = c(as_dep,bl_dep,mx_dep,other_dep,
           as_ems,bl_ems,mx_ems,other_ems),
  names_to=c("Ethnicity","Measure"),
  names_sep = "_",
  names_repair = "unique"
)

final_pivot <- penultimate_pivot %>%
  pivot_wider(names_from=Measure,
              values_from = value)


ggplot(data=final_pivot)+

  aes(x=`dep`,
      y=`ems`)+

  #geom_boxplot(aes(group=tile))+

  geom_point(alpha=0.1)+

  geom_abline(slope=-1.45,
              intercept=0)+

  #geom_smooth(se = FALSE)+

  facet_wrap(~Ethnicity)

}
