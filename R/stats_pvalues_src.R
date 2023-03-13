#' Calculate p values for a linear regression model used on each source sector.
#' The data is split into random chunks
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' p_values_for_chunks()

stats_pvalues_src <- function(prawn_path){
  data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)


if (nrow(data)<=73*384){
test <- tibble(chunk=sample(rep.int(x=c(1:384),times=73),size=nrow(data),replace=FALSE))

chunkable <- bind_cols(data,test)%>% group_by(chunk)

end_of_sources <- which(colnames(data)=="Waste treatment \nand disposal")

source_list <- colnames(data)[c(1:end_of_sources)]

long_data <- data %>%
  pivot_longer(
    cols=all_of(c(source_list,"Point sources")),
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

