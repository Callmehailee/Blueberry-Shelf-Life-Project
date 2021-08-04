myblueberries <- read.csv("C:/Users/17038/Desktop/STAT5020W/Project/dataset/blueberry_allyearfirstaverages.csv", na = "NA", fill = TRUE)

View(myblueberries) #no missing values anymore

#Remove all missing values:
#TA and PH are related, so that is why they are both missed the values
rmv <- which(myblueberries$pH == "#DIV/0!") #Get the index of the missing value:
#class(rmv)
myblueberries <- myblueberries[-rmv,]

View(myblueberries)
library(ggplot2)
library("ggpubr")

#IQR(myblueberries$pH)

#Filter by 3days group:
library(dplyr)
ph_3days <- filter(myblueberries, PostharvestStage == "3")
IQR(ph_3days$pH)
mean(ph_3days$pH)
median(ph_3days$pH)


ph_4days <- filter(myblueberries, PostharvestStage == "4")
IQR(ph_4days$pH)
mean(ph_4days$pH)
median(ph_4days$pH)

ph_5days <- filter(myblueberries, PostharvestStage == "5")
IQR(ph_5days$pH)
mean(ph_5days$pH)
median(ph_5days$pH)

#suziblue







p5 <- ggboxplot(myblueberries, x = "PostharvestStage" , y = "pH", 
                color = "PostharvestStage", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("3", "4", "5"),
                ylab = "pH", xlab = "Days After Harvest") + labs(title = "Missing values had simply been removed")
p5 + geom_hline(yintercept = mean(myblueberries$pH), linetype = 2, group = "supp")+annotate("text", x = 4, y = 3.6, label = "Grand Mean = 3.633")

myblueberries[,11] <- as.numeric(myblueberries[,11])
myblueberries[,12] <- as.numeric(myblueberries[,12])



blueberries <- read.csv("C:/Users/17038/Desktop/STAT5020W/Project/dataset/FirstYearBlueberry_Complete_Updated.csv", na = "NA", fill = TRUE)

ph_3days_new <- filter(blueberries, PostharvestStage == "3")
IQR(ph_3days_new$pH)
mean(ph_3days_new$pH)
median(ph_3days_new$pH)

p5_new <- ggboxplot(blueberries, x = "PostharvestStage" , y = "pH", 
                color = "PostharvestStage", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("3", "4", "5"),
                ylab = "pH", xlab = "Days After Harvest")+labs(title = "Missing values have been filled in")
p5_new + geom_hline(yintercept = mean(blueberries$pH), linetype = 2, group = "supp")+annotate("text", x = 4, y = 3.58, label = "Grand Mean = 3.613")




TA_3days <- filter(myblueberries, PostharvestStage == "3")
IQR(TA_3days$TA)
mean(TA_3days$TA)
median(TA_3days$TA)

TA_4days <- filter(myblueberries, PostharvestStage == "4")
IQR(TA_4days$TA)
mean(TA_4days$TA)
median(TA_4days$TA)

TA_5days <- filter(myblueberries, PostharvestStage == "5")
IQR(TA_5days$TA)
mean(TA_5days$TA)
median(TA_5days$TA)

p6 <- ggboxplot(myblueberries, x = "PostharvestStage" , y = "TA", 
                color = "PostharvestStage", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("3", "4", "5"),
                ylab = "TA (% of juice)", xlab = "Days After Harvest")+ labs(title = "Missing values had simply been removed")
p6 + geom_hline(yintercept = mean(myblueberries$TA), linetype = 2, group = "supp") +annotate("text", x = 4, y = 0.35, label = "Grand Mean = 0.386")


TA_3days_new <- filter(blueberries, PostharvestStage == "3")
IQR(TA_3days_new$TA)
mean(TA_3days_new$TA)
median(TA_3days_new$TA)

p6_new <- ggboxplot(blueberries, x = "PostharvestStage" , y = "TA", 
                color = "PostharvestStage", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                order = c("3", "4", "5"),
                ylab = "TA (% of juice)", xlab = "Days After Harvest") +labs(title = "Missing values have been filled in")
p6_new + geom_hline(yintercept = mean(blueberries$TA), linetype = 2, group = "supp") +annotate("text", x = 4, y = 0.35, label = "Grand Mean = 0.381")

