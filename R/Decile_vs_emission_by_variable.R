#This fuction creates a graph of emissions vs decile for a chosen variable,
#it works off a variable already specified in R, this was chosen over a file path as
#whilst it needs more steps it allows greater flexibility for a modular approach in the analysis

Decile_vs_emission_by_variable <- function(active_stack,chosen_decile,chosen_variable,chosen_grouping,title,xaxis,yaxis,UK_Average=FALSE,Pollutant="Chaos",Custom_scale=NULL,archive=FALSE){

  active_graph <- ggplot(data=active_stack)+
    aes(x={{chosen_decile}},
        y={{chosen_variable}},
        colour={{chosen_grouping}}
    )+

    scale_color_viridis(option="turbo", discrete = TRUE)+

    #coord_cartesian(xlim = c(1, 10),ylim=Custom_scale)+

    scale_x_continuous(
      breaks=c(1:10),
      expand = expansion(mult=0,add=0),
      minor_breaks = FALSE
    )+

    labs(x=xaxis,
         y=yaxis,
         title=title
    )+

    geom_line(stat="summary",linetype="Mean"
    )+


    geom_smooth(method="lm",formula=y~x,se=FALSE,show.legend=FALSE,linetype="Mean")+
    #Plot the line of best fit for the median
    geom_quantile(quantiles=0.5,
                  aes(linetype="Median"),
                  size =1)+

    #Plot a line through the medians for each decile
    geom_line(stat="summary",fun=median,aes(linetype="Median"))

if(UK_Average==TRUE){
    #New section to add the extra line
   active_graph <- active_graph + geom_smooth(data=read.csv("Outputs/Output_data/NOx/NOxsuperstack2019.csv")
                , aes(
      x={{chosen_decile}},
      y={{chosen_variable}},
      method="lm",
      #formula={{chosen_variable}}~{{chosen_decile}},
      se=FALSE,
      show.legend = FALSE,
      linetype="UK average"))+

     scale_linetype_manual("UK average"=1,"Mean"=2, "Median"=3)
}



  if(archive==TRUE){
    ggsave(filename=paste0("Outputs/Plots/",Pollutant,"/",title,".svg"),
           plot=last_plot(),
           device="svg")
    (chicken <- drive_put(
      media=paste0("Outputs/Plots/",Pollutant,"/",title,".svg"),
      path=paste0("Nathan_inequality_plots/Emissions/",Pollutant,"/",title,".svg"),
      type="svg"
    ))

  }
  active_graph

}
