
library(ggplot2)
library(scales)

setwd("C:/Users/Jingrong/Desktop/MSSP/676/Project/children lying")
#graph created for experiment 1 
#without considering hiding location
prob1 <- readRDS("p1.rds")
ggplot(prob1, aes(x=age_gp, y=value, fill = variable)) +
    geom_bar(position = "fill", stat = "identity", width=.5) +
    scale_y_continuous(labels = percent)+
    facet_grid(gender ~ condition)+coord_flip()

#graph created for experiment 2 
#without considering hiding location
table_polr2 <- read_csv("C:/Users/Jingrong/Desktop/MSSP/676/Project/children lying/table.polr2.csv")
table_polr2$X1<-c(1:12)
prob2 <- melt(table_polr2,id.vars=c("X1","agegroup","condition","gender"))

ggplot(prob2, aes(x=agegroup, y=value, fill = variable)) +
  geom_bar(position = "fill", stat = "identity", width=.5) +
  scale_y_continuous(labels = percent)+
  facet_grid(gender ~ condition)+coord_flip()


