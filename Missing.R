##################################
#verifying NAs and NULLs
##################################

Missing<-function(data){
  
library(tidyverse)
miss_pct <- sapply(data, function(x) { round(((sum(is.na(x))+sum(is.null(x))) / length(x)) * 100, 1) })

miss_pct <- miss_pct[miss_pct > 0]

graph<-data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1)) # hjust adjust the text to the graph

miss_var<-names(miss_pct)

my_list<-list("graph"=graph, "miss_var"=miss_var)

return(my_list)

}