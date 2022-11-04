
focused_plot()

boxxy <- londonless_prawn %>% group_by(IMD) %>% summarise(q90=quantile(Total,c(0.90)),
                                                          q10=quantile(Total,c(0.10)),
                                                          q1=quantile(Total,c(0.25)),
                                                          q3=quantile(Total,c(0.75)),
                                                          med=quantile(Total,c(0.5))) %>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "Emissions")

londonless_long_prawn <- londonless_prawn %>% pivot_longer(cols=c("Total","Road transport","Domestic combustion"),
                                                           values_to = "Emissions",
                                                           names_to="Source")

london_window <- ggplot(data = londonless_long_prawn)+
  aes(x=IMD,
      y=Emissions)+
  geom_boxplot(data=boxxy,aes(x=IMD,group=IMD,y=Emissions)) +

  geom_quantile(quantiles=0.5,
                size =1,
                formula=y~x,
                aes(colour=Source))+

  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(linetype="Linear regression"))+
