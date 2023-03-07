#' Save ggplot2 graphs with sensible scaling and size options ready for publication
#'
#' @param plot The ggplot2 object to plot
#' @param filename The location to save the file to
#' @param file_format The format to save using, this can include agg_png and the
#'  agg family
#' @param type The graph size to save to 1 is a single column figure on a two
#' column page, 2 is a full page spread, both use a 16:9 aspect ratio, 3 is an
#' entire page, 4 is a graphical abstract
#' @param scaling The =scaling parameter to use, flexible because there are many
#'  things at play when the default is picked
#` @keywords faceted, sources
#' @export
#' @examples

process_graph_saver <- function(plot, filename,file_format,type,scaling=0.2){



  if(grepl("agg_",file_format)==TRUE){
    file_format1 <- gsub(pattern="agg_",x=file_format,replacement = "")
    file_format <- (eval(parse(text=file_format)))}

  if (type==1){
  # 1 column graphs
      ggsave(filename= filename,
             plot=plot,
             width=8.3,
             height=4.7,
             units = "cm",
             dpi = 600,
             device=file_format,
             scaling=scaling)}

  if (type==2){
# 2 column graphs
      ggsave(filename= filename,
             plot=plot,
             width=17.1,
             height=9.6,
             units = "cm",
             dpi = 600,
             device=file_format,
             scaling=scaling)
  }
# Full page graphs
   if (type==3){
     ggsave(filename= filename,
            plot=plot,
            width=210,
            height=260,
            units = "mm",
            dpi = 600,
            device=file_format,
            scaling=scaling)
   }

#Grpahical abstract thumbnail
  if (type==4){
    ggsave(filename= filename,
         plot=plot,
         width=8,
         height=4,
         units = "cm",
         dpi = 600,
         device=file_format,
         scaling=scaling)
  }

#full page powerpoint slide
  if (type==5){
    ggsave(filename= filename,
           plot=plot,
           width=1920,
           height=1080,
           units = "px",
           device=file_format,
           scaling=scaling)
  }
}

