#' Calculate p values for a linear regression model used on each source sector.
#' The data is split into random chunks
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' stats_pvalues_src(
#'  prawn_path="PRAWN.csv")

stats_pvalues_src <- function(prawn_path){
  data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE) %>%
    dplyr::select(-c(Tot_area,Offshore))


if (nrow(data)<=73*384){
#By changing which line is commented out you can adjust sample size to see how that affects the p-values
test <- tibble(chunk=sample(rep.int(x=c(1:384),times=73),size=nrow(data),replace=FALSE))
#test <- tibble(chunk=sample(rep.int(x=c(1:192),times=146),size=nrow(data),replace=FALSE))
chunkable <- bind_cols(data,test)%>% group_by(chunk)

end_of_sources <- which(colnames(data)=="LSOA11CD")-1

source_list <- colnames(data)[c(1:end_of_sources)]

long_data <- chunkable%>%
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


plist <- aha %>% ungroup() %>%
 # filter(Emission_source=="Total") %>%
  dplyr::select(p.value_IMD,Emission_source) %>%
  group_by(Emission_source)

ultimatelist <- plist %>% group_map(~p.adjust(.x$p.value_IMD,
                                        method = "fdr"))


ultimatetibble <-  tibble(placeholder <- c(1:length(ultimatelist[[1]])))

for(source in c(1:nrow(group_keys(plist)))){
  name <- paste0(group_keys(plist)[source,1])
  ultimatetibble[[name]] <- ultimatelist[[source]]
}

plottable <- ultimatetibble %>%
  pivot_longer(cols = c(2:ncol(ultimatetibble)),
               names_to = "Emission_source")
reveal <- ggplot(data=plottable)+
  aes(x=value)+
  geom_histogram(boundary=0,bins = 60)+
  facet_wrap(~Emission_source,scale="free_y")+
  coord_cartesian(xlim=c(0,1),expand=FALSE)+geom_vline(xintercept=0.05,colour="orange")+
  labs(x="FDR adjusted p-value for linear model linking IMD and NOx emissions",
       y= "Frequency")
}else{
  reveal <- ggplot(data)+geom_bar(aes(x=IMD))+labs(title="Not here")
}


reveal

}

