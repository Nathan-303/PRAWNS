#' Create a faceted histogram showing all but the top 1% of the nox emission average across lsoas

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' p_values_for_chunks()

p_values_for_chunks <- function(prawn_path){
  data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

set.seed(8653)
if (nrow(data)<=73*384){
test <- sample(rep.int(x=c(1:384),times=73),size=nrow(data),replace=FALSE)

chunkable <- bind_cols(data,test) %>% rename(chunk=`...85`) %>% group_by(chunk)

long_data <- chunkable %>% pivot_longer(
  cols=c("Agricultural","Domestic combustion","Energy production",
         "Industrial combustion","Industrial production","Natural",
         "Other transport and mobile machinery","Road transport","Solvents","Total"
         ,"Waste treatment and disposal","Point sources"),
  names_to = "Emission_source",
  values_to = "emissions") %>%
  group_by(Emission_source,chunk) %>%
  mutate(emissions=replace_na(emissions,0))


hmm <- long_data %>% do(tidy(lm(emissions~IMD,data=.)))

aha <- hmm %>% pivot_wider(names_from=term,
            values_from=c(estimate,
                          p.value,
                          std.error,
                          statistic))

reveal <- ggplot(data=aha)+
  aes(x=p.value_IMD)+
  geom_histogram(boundary=0,bins = 60)+
  facet_wrap(~Emission_source,scale="free_y")+
  coord_cartesian(xlim=c(0,1),expand=FALSE)+geom_vline(xintercept=0.05,colour="orange")+
  labs(x="P value for linear model linking IMD and NOx emissions",
       y= "Frequency")
}else{
  reveal <- ggplot(data)+geom_bar(aes(x=IMD))+labs(title="Not here")
}


reveal

}

