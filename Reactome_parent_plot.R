library(ggplot2)
library(dplyr)

parent_scatterplot <- function (testdata, Pathway.FDR){
  testdata <- testdata[testdata$Pathway.FDR < Pathway.FDR,] 
  testdata <- testdata %>% group_by(Parent.term) %>% mutate(parent_n = length(Reactome.pathway))
  testdata <- testdata %>% group_by(Parent.term, Reactome.pathway) %>% mutate(group_median = median(Median))
  testdata$amp <- testdata$parent_n*1000
  testdata$hm <- testdata$group_median+testdata$amp
  testdata <- testdata %>% mutate(name = fct_reorder(Reactome.pathway, hm))
  
  
  opposite <- ggplot(testdata, 
                     aes(x = reorder(Reactome.pathway,hm),
                         y = Median,
                         col = reorder(Parent.term,hm),
                         group = Parent.term))
  test <- opposite + 
    geom_point(size = 2) +
    theme_grey(12) +
    labs(x= "Pathway",
         y = "Protein abundance")+
    geom_hline(yintercept = 0)+
    theme(axis.text.x = element_text(size = 9,angle =70 , hjust = 1, face = "bold"),
          legend.title = element_blank(),
          legend.text = element_text(size = 8)) + 
    scale_x_discrete(labels = function(x) str_wrap(x,width = 40))
    print(test)
}

