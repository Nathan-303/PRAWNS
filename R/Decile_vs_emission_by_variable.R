#'Create a graph of emissions vs decile for a chosen variable
#'
#'it works off a variable already specified in R, this was chosen over a file path as
#whilst it needs more steps it allows greater flexibility for a modular approach in the analysis
#'
#'@param active_stack the data to use for the graph, should be an existing object in r
#'
#'@param chosen_decile the decile to use for the x scale, should match a column in the data
#'
#'@param chosen_variable the variable to use for the y scale, should match a column in the data
#'
#'@param chosen_grouping the variable to group by, should match a column in the data
#'
#'@param title the title to give th graph
#'
#'@param xaxis the label for the x axis
#'
#'@param yaxis the label for the y axis
#'
#'@param UK_average whether to show the UK average for reference, defaults to FALSE
#'
#'@export
#'Decile_vs_emission_by_variable



Decile_vs_emission_by_variable <- function(active_stack,chosen_decile,chosen_variable,chosen_grouping,title,xaxis,yaxis,UK_Average=FALSE){

  active_graph <- ggplot(data=active_stack)+
    aes(x={{chosen_decile}},
        y={{chosen_variable}},
        colour={{chosen_grouping}}
    )+

    scale_color_viridis(option="turbo", discrete = TRUE)+

    scale_x_continuous(
      breaks=c(1:10),
      expand = expansion(mult=0,add=0),
      minor_breaks = FALSE
    )+

    labs(x=xaxis,
         y=yaxis,
         title=title
    )+

    geom_line(stat="summary",
              aes(linetype="Mean"),
              na.rm = TRUE,
              fun=mean
    )+


    geom_smooth(method="lm",
                formula=y~x,
                se=FALSE,
                show.legend=FALSE,
                aes(linetype="Mean"),
                na.rm = TRUE)+
    #Plot the line of best fit for the median
    geom_quantile(quantiles=0.5,
                  aes(linetype="Median"),
                  size =1,
                  formula=y~x,
                  na.rm = TRUE)+

    #Plot a line through the medians for each decile
    geom_line(stat="summary",
              fun=median,
              aes(linetype="Median"),
              na.rm = TRUE)

if(UK_Average==TRUE){
    #New section to add the extra line
   active_graph <- active_graph + geom_smooth(data=read.csv(prawn_path,
                                                            row.names=1,
                                                            check.names=FALSE)
                , aes(
      x=chosen_decile,
      y=chosen_variable,
      linetype="UK average"),
      method="lm",
      #formula={{chosen_variable}}~{{chosen_decile}},
      se=FALSE,
      show.legend = FALSE,
      )
}

  active_graph

}
