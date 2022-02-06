#first  we will check the summary of the data called'SCMP_TAB'where mean, median, mode will be explained  to us.
summary(SCMP_TAB)
## it is determining the dependence of alkalinity_kit on the other three variables
## scatterplot matrix is showing the relationship of the various variables in the dataset from which we can deduce the results on the covaraince
pairs(~COPPER+NITRATE+NITRITE+IRON+PH,data = SCMP_TAB,
      main = "Scatterplot Matrix")
## we prepared the regression analysis model using multiple regression analysis 
model_dm <- lm(TURBIDITY_IN_SITU ~ DO_IN_SITU + CO2_KIT+WATER_TEMP_IN_SITU,data = SCMP_TAB )
summary(model_dm)
confint(model_dm)
model_dm2 <- lm(SPC_IN_SITU ~SALINITY_IN_SITU,data=SCMP_TAB)
summary(model_dm2)
confint(model_dm2)
model <- lm (HARDNESS_KIT~ ALKALINITY_KIT+COPPER + NITRATE+ NITRITE+IRON+PH, data = SCMP_TAB)
summary(model)
summary(model)$coefficient
confint(model)
anova(model)
install.packages("ggpubr")
library("ggpubr")
install.packages("ggplot2")
library(ggplot2)
ggscatter(SCMP_TAB, x = "COPPER", y = "ALKALINITY_KIT", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ALKALNITY_KIT_RESULTS", ylab = "COPPER CONTENT")
shapiro.test(SCMP_TAB$COPPER) 
# Shapiro-Wilk normality test for ALKALINITY_KIT
shapiro.test(SCMP_TAB$ALKALINITY_KIT) 
install.packages("ggpubr")
library("ggpubr")
# COPPER
ggqqplot(SCMP_TAB$COPPER, ylab = "COPPER")
# ALKALINITY_KIT
ggqqplot(SCMP_TAB$ALKALINITY_KIT, ylab = "ALKALINITY_KIT")
res <- cor.test(SCMP_TAB$ALKALINITY_KIT, SCMP_TAB$COPPER, 
                method = "pearson")
res
# Extract the p.value
res$p.value
# Extract the correlation coefficient
res$estimate

