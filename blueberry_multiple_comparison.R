##Blueberry Projects --- Multiple Comparisons on Farm types and Cultivar types

#Read in the dataset (for multiple comparison):
my_dat <- read.csv("C:/Users/17038/Desktop/STAT5020W/Project/dataset/BlueberryComplete_Reps_AllDates_MC_simplify.csv")
head(my_dat)
dim(my_dat)
#View(my_dat)

#Response variable: (for all)
all_df <- my_dat$DefectFree


#Farm Types:
unique(my_dat$Farm) #2 types of farms


#-----------ANOVA--------------#
#Build the ANOVA model:
options(contrasts=c("contr.sum", "contr.poly"))   # zero-sum constraint

#Build the model:
mdl_farm = lm(all_df ~ factor(my_dat$Farm))
summary(mdl_farm)

#Get the Anova Table:
anova(mdl_farm) 
#Yes, we should do the multiple comparison as the p-values indicates the treatment farm type is significant/effective

trt_farm <- my_dat$Farm
#Tukey Method:
g_farm = aov(all_df ~ factor(trt_farm))
TukeyHSD(g_farm, 'factor(trt_farm)', conf.level=0.95)
plot(TukeyHSD(g_farm, 'factor(trt_farm)', conf.level=0.95), xlim = c(0,18), col = "red")





#Year:
unique(my_dat$Year) #4 types
#change year column name:
all_years <- c(rep("15", 32), rep("16", 32), rep("17", 44), rep("18", 39))
my_dat$Year <- all_years
#-----------ANOVA--------------#
#Build the ANOVA model:
options(contrasts=c("contr.sum", "contr.poly"))   # zero-sum constraint

#Build the model:
mdl_year = lm(all_df ~ factor(my_dat$Year))
summary(mdl_year)

#Get the Anova Table:
anova(mdl_year) 
#Yes, we should do the multiple comparison as the p-values indicates the treatment farm type is significant/effective

trt_year <- my_dat$Year
#Tukey Method:
g_year = aov(all_df ~ factor(trt_year))
TukeyHSD(g_year, 'factor(trt_year)', conf.level=0.95)
summary_tbl2 <- TukeyHSD(g_year, 'factor(trt_year)', conf.level=0.95)
all_decisions2 <- as.data.frame(summary_tbl2$`factor(trt_year)`)
colnames(all_decisions2) <- c("diff", "lwr", "upr", "pvalues")
my_col2 <- as.numeric((all_decisions2$pvalues < 0.05)) + 1
plot(TukeyHSD(g_year, 'factor(trt_year)', conf.level=0.95), col = my_col2)



#Blueberry Type:
unique(my_dat$Type) #2 types


#-----------ANOVA--------------#
#Build the ANOVA model:
options(contrasts=c("contr.sum", "contr.poly"))   # zero-sum constraint

#Build the model:
mdl_type = lm(all_df ~ factor(my_dat$Type))
summary(mdl_type)

#Get the Anova Table:
anova(mdl_type) 
#Yes, we should do the multiple comparison as the p-values indicates the treatment farm type is significant/effective

trt_type <- my_dat$Type
#Tukey Method:
g_type = aov(all_df ~ factor(trt_type))
TukeyHSD(g_type, 'factor(trt_type)', conf.level=0.95)
plot(TukeyHSD(g_type, 'factor(trt_type)', conf.level=0.95), xlim = c(0,18), col = "red")



#Cultivar Type:

unique(my_dat$Cultivar) #16 types of farms


#-----------ANOVA--------------#
#Build the ANOVA model:
options(contrasts=c("contr.sum", "contr.poly"))   # zero-sum constraint

#Build the model:
mdl_cultivar = lm(all_df ~ factor(my_dat$Cultivar))
summary(mdl_cultivar)

#Get the Anova Table:
anova(mdl_cultivar) 
#Yes, we should do the multiple comparison as the p-values indicates the treatment farm type is significant/effective

trt_cultivar <- my_dat$Cultivar
#Tukey Method:
g_cultivar = aov(all_df ~ factor(trt_cultivar))
TukeyHSD(g_cultivar, 'factor(trt_cultivar)', conf.level=0.95)
summary_tbl <- TukeyHSD(g_cultivar, 'factor(trt_cultivar)', conf.level=0.95)
class(summary_tbl)
all_decisions <- as.data.frame(summary_tbl$`factor(trt_cultivar)`)
colnames(all_decisions) <- c("diff", "lwr", "upr", "pvalues")
my_col <- as.numeric((all_decisions$pvalues < 0.05)) + 1
plot(TukeyHSD(g_cultivar, 'factor(trt_cultivar)', conf.level=0.95), col = my_col, cex.axis=.8)

#All significant
library(dplyr)
all_significant <- filter(all_decisions, pvalues < 0.05)
all_significant[,c(1,4)]



#colnames(all_pvalues_results) <- "Significant Difference Between Pairs?"
#View the results:
#all_pvalues_results
#plot(TukeyHSD(g_cultivar, 'factor(trt_cultivar)', conf.level=0.95))


