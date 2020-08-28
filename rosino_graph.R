require(tidyverse)
require(readxl)
setwd("~/Bluebonnet/Rosino Voting")
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

votes <- read_excel("billsvotedyes_count.xlsx")

votes %>%
  ggplot(aes(x = reorder(word,count), y = count)) +
  geom_col(color = "black", fill = "pink") +
  ggtitle("Categories of Bills Voted For") +
  ylab("Number of Bills") +
  xlab("Category") +
  coord_flip()

voteprop <- tibble(senator = c("Rosino","Matthews","Boren","Kirt","Dossett",
                               "Ikley-Freeman","Hicks","Brooks","Floyd","Young",
                               "Bergstrom","Quinn","Shaw","Allen","Silk","Bullard",
                               "Boggs","Thompson","Pemberton","Coleman","Leewright",
                               "McCortney","Simpson","Standridge","Sharp","David",
                               "Pederson","Hall","Dugger","Bice","Paxton","Weaver",
                               "Newhouse","Jech","Murdock","Daniels","Kidd","Montgomery",
                               "Dahm","Stanislawski","Haste","Howard","Rader","Pugh",
                               "Stanley","Scott","Treat"),
                   party = c("R",rep("D",9),rep("R",37)),
                   yea = c(197,161,140,159,177,167,135,171,166,140,198,186,202,148,
                           85,181,194,193,208,196,201,191,203,195,191,199,194,207,
                           208,198,199,200,197,205,188,187,90,205,133,203,204,198,204,
                           195,206,190,173),
                   nay = c(4,45,45,48,34,37,42,38,45,65,9,18,9,21,44,8,10,1,3,4,7,13,
                           5,12,17,4,14,4,3,4,9,8,9,6,7,16,1,6,73,8,3,10,2,10,2,21,2))

voteprop %>% ggplot(aes(yea,nay,color = party)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("dodgerblue3","firebrick2")) +
  geom_point(aes(x=197, y=4), colour="black",size = 4, shape = 21, fill = "red") +
  ggtitle("Senator Votes") +
  xlab("Yea") +
  ylab("Nay")

voteprop %>% group_by(party) %>% 
  summarise(proportion = mean(yea/(yea+nay)))

voteprop <- voteprop %>% mutate(prop = yea/(yea+nay))

fake_df <- tibble(party = c("Democrat Average","Republican Average","Rosino"),prop = c(77.9,94.4, 98))
fake_df %>% ggplot(aes(party,prop, fill = party)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("dodgerblue3","firebrick2","pink")) +
  ggtitle("Rate of Voting in Support of Bills") +
  xlab("") +
  ylab("Percentage") +
  theme(legend.position = "none")
  
