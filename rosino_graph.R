require(tidyverse)

rosino <- read.csv("rosino_bills.csv")

#Order the levels in descending order
rosino <- within(rosino, 
                   Category <- factor(Category, 
                                      levels=names(sort(table(Category), 
                                                        decreasing=FALSE))))

#Use geom_hist() instead of geom_bar() for a histogram
rosino %>% ggplot(aes(Category)) +
  geom_bar(color = "black", fill = "pink") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5)) +
  ggtitle("Categories of Bills Sponsored") +
  ylab("Frequency") +
  coord_flip()