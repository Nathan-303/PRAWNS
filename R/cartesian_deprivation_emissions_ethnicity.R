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

cartesian_deprivation_emissions_ethnicity <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=-c(date,geography,`geography code`),
    names_to = "Ethnic group",
    values_to = "flat_population"
  ) %>%

  mutate(`Ethnic group`=str_sub(`Ethnic group`,start=14L))

intermediate <- inner_join(data,edata,by=c("LSOA21CD"="geography code"))%>%

  mutate(`Weighted emissions`= Total*flat_population,
         `Weighted deprivation`=IMD*flat_population)

refcalc <- intermediate %>% dplyr::filter(`Ethnic group`==" White: English, Welsh, Scottish, Northern Irish or British") %>% 
  group_by(IMD) %>% 
  summarise(popsum=sum(flat_population),
            emissions_sum=sum(`Weighted emissions`)) %>% 
  mutate(emissions=emissions_sum/popsum)

refmodel <- lm(emissions~IMD,
               data=refcalc,
               )


weighted_data <- intermediate %>%

  group_by(`Ethnic group`) %>%

  summarise(popsum=sum(flat_population),
            emissions_sum=sum(`Weighted emissions`),
            IMD_sum=sum(`Weighted deprivation`)) %>%

  mutate(`Weighted emissions`=emissions_sum/popsum,
         `Weighted deprivation`=IMD_sum/popsum) %>%

  group_by(`Ethnic group`)%>%

  mutate(broad_group=case_when(
    grepl(pattern="Asian, Asian British",`Ethnic group`)==1~"Asian",
    grepl(pattern="Black, Black British",`Ethnic group`)==1~"Black",
    grepl(pattern="Mixed or Multiple",`Ethnic group`)==1~"Mixed or Multiple",
    grepl(pattern="Other ethnic group",`Ethnic group`)==1~"Other",
    grepl(pattern="White",`Ethnic group`)==1~"White",
  ))

keys <- unique(weighted_data$broad_group)

for(index in 1:length(keys)){
  chunk <- weighted_data %>%
    filter(broad_group==keys[index]) %>%
    ungroup() %>%
    mutate(subgroup=row_number())

  if(index==1){
    indexed_data <- chunk
  }else{
    indexed_data <- rbind(indexed_data,chunk)
  }
}

indexed_data <- indexed_data %>% mutate(
point_shape=case_when(
  subgroup==1~1,
  subgroup==2~16,
  subgroup==3~17,
  subgroup==4~18,
  subgroup==5~4,
  subgroup==6~15,
  ),
point_size=case_when(
  subgroup==1~2,
  subgroup!=1~1
  )
)%>% mutate(`Ethnic group`=`Ethnic group` %>% str_replace_all(
  c("Asian, Asian British or Asian Welsh: "="",
                         "Black, Black British, Black Welsh, Caribbean or African: "="",
                         "Mixed or Multiple ethnic groups: "="",
                         "Other ethnic group:"="")
  )
#close mutate
) %>%
  mutate(`Ethnic group`=str_trim(`Ethnic group`,"left")) %>%
  mutate(`Ethnic group`=case_when(
    `Ethnic group`=="Black, Black British, Black Welsh, Caribbean or African"~"Black, Black British, Black\nWelsh, Caribbean or African",
    `Ethnic group`=="White: English, Welsh, Scottish, Northern Irish or British"~"White: English, Welsh, Scottish,\nNorthern Irish or British",
    `Ethnic group`=="Asian, Asian British or Asian Welsh"~"Asian, Asian British\nor Asian Welsh",
    `Ethnic group`=="Mixed or Multiple ethnic groups"~"Mixed or Multiple\nethnic groups",
    `Ethnic group`=="Other Mixed or Multiple ethnic groups"~"Other Mixed or\nMultiple ethnic groups",
    !`Ethnic group`%in%c("Black, Black British, Black Welsh, Caribbean or African",
                         "White: English, Welsh, Scottish, Northern Irish or British",
                         "Asian, Asian British or Asian Welsh",
                         "Mixed or Multiple ethnic groups",
                         "Other Mixed or Multiple ethnic groups"
                         )~`Ethnic group`
  )) %>%
  #move it so the entires for other come at the end of the category rather than anywhere else
  mutate(nicer_order = c(1,2,3,4,6,5,7,8,9,10,11,15,13,14,12,16,18,17,19,20,21,22,24,23)) %>%

  arrange(nicer_order) %>%

  mutate(`Ethnic group`=factor(`Ethnic group`,levels=`Ethnic group`))

reference <- indexed_data %>% dplyr::filter(`Ethnic group`=="White: English, Welsh, Scottish,\nNorthern Irish or British") %>% 
  rename("majority"="Weighted emissions")

output_table <- indexed_data %>% mutate(majority=reference$majority,
                                        scaled_white=`Weighted deprivation`*refmodel$coefficients[2]+
                                          refmodel$coefficients[1]) %>%
  mutate(percentage_up_major=`Weighted emissions`/majority,
         percentage_scaled_major=`Weighted emissions`/scaled_white)

write.csv(x=output_table,file = paste0(pollutant,"emission averages.csv"))
output <- ggplot(data=indexed_data)+

  #Set the standard variables
  aes(x=`Weighted deprivation`,
      y=`Weighted emissions`,
      fill=`Ethnic group`
      )+

  # This one is for setting the legend without breaking the plot
  geom_point(alpha=0,stroke=2
  )+
  
  geom_smooth(data=data,
              aes(x=IMD,
                  y=Total),
              inherit.aes = FALSE,
              method="lm",
              formula=y~x,
              se=FALSE)+

  #Plot the data with the aesthetics wanted
  geom_point(aes(x=`Weighted deprivation`,
                 y=`Weighted emissions`,
                 colour=broad_group,
                 shape=as.factor(subgroup),
                 size=as.factor(point_size),
                 stroke=2
                 ),
  show.legend = FALSE
  )+

  #define the scales used for plotting the data, control point size here
  scale_size_manual("the legend",
                    breaks=c(1,2),
                    values=c(4,6))+

  scale_colour_manual("the legend",
                      values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2")
  )+

  scale_shape_manual("the legend",
                     values = c(1,16,17,18,4,15))+
  #Trim the axis as the line makes the scale too big
  coord_cartesian(xlim=c(3,6)
                  )+



  #Set the display parameters for the legend
  scale_fill_viridis_d("Ethnic\ngroup",guide=guide_legend(override.aes = list(
    colour=c(rep("black",6),
             rep("royalblue",4),
             rep("olivedrab1",5),
             rep("#FB8022FF",3),
             rep("deeppink2",6)),
    alpha=1,
    shape=indexed_data$point_shape,
    size=2),
    ncol=6))+
  
  # geom_smooth(data=refcalc,
  #             inherit.aes = FALSE,
  #             aes(x=IMD,
  #                 y=emissions),
  #             colour="deeppink2",
  #             show.legend = FALSE,
  #             se = FALSE
  # )+
  
  geom_smooth(data=refcalc,
              inherit.aes=FALSE,
              method="lm",
              formula=y~x,
              aes(x=IMD,
                  y=emissions),
              colour="deeppink2",
              show.legend = FALSE,
              se = FALSE
  )+
  # 
  # geom_point(data=refcalc,
  #             inherit.aes = FALSE,
  #             aes(x=IMD,
  #                 y=emissions),
  #             colour="deeppink2",
  #             show.legend = FALSE
  # )+

  theme_classic()+
  labs(y=bquote("Average"~.(pollutant)~"emissions/ tonnes "~km^"-2"))+
  theme(legend.position="bottom")+
  expand_limits(y=0)+
  scale_y_continuous(expand = expansion(mult = c(0, .095)))+
  scale_x_continuous(expand=c(0,0))

output
}
