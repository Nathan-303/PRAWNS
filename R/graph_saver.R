grapher <- function(plot, filename,file_format,scaling,type){
 
  
  
  if(grepl("agg_",file_format)==TRUE){
    file_format1 <- gsub(pattern="agg_",x=file_format,replacement = ""),
    file_format <- (eval(parse(text=file_format)))}else{
# 1 column graphs
      ggsave(filename= paste0("scalehunt.png"),
             plot=hmm,
             width=8.3,
             height=4.7,
             units = "cm",
             dpi = 600,
             device=agg_png,
             scaling=0.2)
# 2 column graphs
      ggsave(filename= paste0("scalehuntbeeeg.png"),
             plot=hmm,
             width=17.1,
             height=9.6,
             units = "cm",
             dpi = 600,
             device=agg_png,
             scaling=0.3)
# Large faceted graphs
      

      
    }
}
  #
  
  
  filename= paste0(proc_tag,"/medmeancomp.",file_format1),
plot=stat_facet,
width=8.3,
height=4.7,
units = "cm",
dpi = dpi,
device=file_format