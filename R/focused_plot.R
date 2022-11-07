
focused_plot <- function(focused_prawn_path){

data <- active_stack <- read.csv(focused_prawn_path,
                                 check.names = FALSE,
                                 row.names = 1)

boxxy <- active_stack %>% group_by(IMD) %>% summarise(q90=quantile(Total,c(0.90)),
                                                          q10=quantile(Total,c(0.10)),
                                                          q1=quantile(Total,c(0.25)),
                                                          q3=quantile(Total,c(0.75)),
                                                          med=quantile(Total,c(0.5))) %>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "Emissions")

focused_long_prawn <- active_stack %>% pivot_longer(cols=c("Total","Road transport","Domestic combustion"),
                                                           values_to = "Emissions",
                                                           names_to="Source")

focused_window <- ggplot(data = focused_long_prawn)+
  aes(x=IMD,
      y=Emissions)+
  geom_boxplot(data=boxxy,aes(x=IMD,group=IMD,y=Emissions)) +

  geom_quantile(quantiles=0.5,
                size =1,
                formula=y~x,
                aes(colour=Source))+

  geom_smooth(data=focused_long_prawn %>% filter(Source=="Total"),
              method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(x=IMD,
                  y=Emissions,
                  linetype="dashed"),
              colour="blue")+

  geom_line(data=focused_long_prawn %>% filter(Source=="Total"),
            stat="summary",
            aes(x=IMD,
                y=Emissions,
                linetype="dotted"),
            colour = "blue")

focused_window
}
