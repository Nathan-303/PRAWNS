plot_couty_gradients <- function(prawn_path){

raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)

refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,Area) %>% group_by(Area)


coeffs <- refined %>%
  #get the rsquared
  do(tidy(lm(Total~IMD, data=.))) %>%

  filter(term=="IMD")

curiosity <- coeffs %>% filter(p.value<=0.05)

test <- ggplot()+
  geom_histogram(data=coeffs,
                 aes(x=estimate),bins=50)+

  geom_histogram(data=curiosity,
                 aes(x=estimate),
                 fill="orange",bins=50)+

  coord_cartesian(ylim=c(0,14),expand=FALSE)

test
}
