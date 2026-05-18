#####################################################
# GxE paper                                         #
#                                                   #
# DATA PROCESSING                                   #
# Date:         21/04/2023                          #
# Main Authors: Yayouk Willems                      #
# Analys plan: see https://osf.io/b3w8s/.           #
#                                                   #
#####################################################

#empty environment
rm(list = ls())

#setwd
setwd("/Users/willems/Desktop/Projects/G x E/Analyses")

############# Install & Open Packages #############

#install and load packages if necessary
install.packages("dplyr")
install.packages("psych")
install.packages("data.table")
install.packages("haven")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("sjlabelled")
install.packages("haven")

library(sjlabelled)
library(dplyr)
library(psych)
library(data.table)
library(haven)
library(tidyr)
library(ggplot2)
library(haven)

######### Read in the Data ###########
# Read the SPSS files 
#Self-control scale
data_ph <- read_sav("/Users/willems/Desktop/Projects/G x E/Analyses/PhenotypicData_GxE_R.sav")

#Polygenic scores
data_g_agg <- read_sav("/Users/willems/Desktop/Projects/G x E/Analyses/PRSs_NTR-DSR-3860/NTR-DSR-0328_Aggression_DOI10.1101_FSLSH_854927_MRG16_PedMergedWithScores.sav")
data_g_adhd <- read_sav("/Users/willems/Desktop/Projects/G x E/Analyses/PRSs_NTR-DSR-3860/NTR-DSR-0328_ADHD_PMID36702997_MRG16_PedMergedWithScores.sav")
data_g_ef <- read_sav("/Users/willems/Desktop/Projects/G x E/Analyses/PRSs_NTR-DSR-3860/NTR-DSR-0328_ExecutiveFunctioning_PMID36150907_MRG16_PedMergedWithScores.sav")


#create datafiles to merge
colnames(data_g_agg)
colnames(data_g_adhd)
colnames(data_g_ef)

adhd <- data_g_adhd[,c("FISnumber",
                     "P_0_01_SCORE_ADHD_MRG16_LDp1",
                     "P_0_05_SCORE_ADHD_MRG16_LDp1",
                     "P_0_1_SCORE_ADHD_MRG16_LDp1",
                     "P_0_2_SCORE_ADHD_MRG16_LDp1",
                     "P_0_3_SCORE_ADHD_MRG16_LDp1",
                     "P_0_5_SCORE_ADHD_MRG16_LDp1",
                     "P_inf_SCORE_ADHD_MRG16_LDp1" )]

ef <- data_g_ef[,c("FISnumber",
                       "P_0_01_SCORE_ExecutiveFunctioning_MRG16_LDp1",
                       "P_0_05_SCORE_ExecutiveFunctioning_MRG16_LDp1",
                       "P_0_1_SCORE_ExecutiveFunctioning_MRG16_LDp1",
                       "P_0_2_SCORE_ExecutiveFunctioning_MRG16_LDp1", 
                       "P_0_3_SCORE_ExecutiveFunctioning_MRG16_LDp1",
                       "P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1", 
                       "P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1" )]
#merge datafiles
colnames(data_ph)[colnames(data_ph) == "FISNumber"] <- "FISnumber"
colnames(data_ph)
colnames(data_g_adhd)
colnames(data_g_agg)
Data_0 <- left_join(data_ph, data_g_agg, by = "FISnumber")
Data_1 <- left_join(Data_0, adhd , by = "FISnumber")
Data_2 <- left_join(Data_1, ef , by = "FISnumber")

colnames(Data_2)
describe(Data_2$ASR_ASCS_nohan)

######### Create clean dataset ###########

#omit everyone with Eur outlier 
table(Data_2$handc8)
table(Data_2$EUR_1KG_Outlier)
describe(Data_2$P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1)

# Filter out rows for non-european ancestry using dplyr
filtered_data <- Data_2  %>%
  filter(EUR_1KG_Outlier != 1)

# View the filtered dataset
filtered_data

# Create the SC_count variable based on the ASR condition
describe(filtered_data$NLE_lastyear)
describe(filtered_data$NLE_lifetime)

filtered_data$NLE_lastyear_count <- ifelse(filtered_data$NLE_lastyear >= 0, 1, 0)
filtered_data$NLE_lifetime_count <- ifelse(filtered_data$NLE_lifetime >= 0, 1, 0)
filtered_data$SC_count <- ifelse(filtered_data$ASR_ASCS_nohan >= 0, 1, 0)
filtered_data$SC_count_han <- ifelse(filtered_data$ASR_ASCS >= 0, 1, 0)
table(filtered_data$SC_count)
table(filtered_data$SC_count_han, filtered_data$NLE_lastyear_count )

#create dataset with participants with SC measure and 18 plus years old
#n=7090
data <- filter(filtered_data, SC_count == 1)
data <- filter(data, age8>17 )

table(data$NLE_lastyear)
table(data$NLE_lifetime)
table(data$sex)
describe(data$ASR_ASCS_nohan)
describe(data$P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1)
describe(data$age8)
describe(data$NLE_lastyear)
describe(data$NLE_lifetime)

#check distribution of the data

#selfcontrol
hist(data$ASR_ASCS_nohan)

#NLE last year
counts <- table(data$NLE_lastyear)

# Bar plot
barplot(counts, 
        col = "skyblue",           # Bar color
        main = "Counts of NLE Lifetime",   # Title of the plot
        xlab = "NLE Last Year",     # Label for the x-axis
        ylab = "Frequency")        # Label for the y-axis

#NLE lifetime
counts <- table(data$NLE_lifetime)

# Bar plot
barplot(counts, 
        col = "skyblue",           # Bar color
        main = "Counts of NLE Lifetime",   # Title of the plot
        xlab = "NLE Lifetime",     # Label for the x-axis
        ylab = "Frequency")        # Label for the y-axis


#transform data and see whether it improves the distribution
data$NLE_lastyear_sqrt <- sqrt(data$NLE_lastyear)
data$NLE_lifetime_sqrt <- sqrt(data$NLE_lifetime)
data$ASR_ASCS_nohan_sqrt<- sqrt(data$ASR_ASCS_nohan)
data$ASR_ASCS_nohan_log <- log(data$ASR_ASCS_nohan + 1) 
data$NLE_lastyear_log <- log(data$NLE_lastyear + 1) 
data$NLE_lifetime_log <- log(data$NLE_lifetime + 1)
data$age_sq<- data$age8^2

## check distribution after correction
hist(data$ASR_ASCS_nohan)
hist(data$ASR_ASCS_nohan_sqrt)
hist(data$ASR_ASCS_nohan_log)

#NLE last year
counts <- table(data$NLE_lastyear_sqrt)
counts <- table(data$NLE_lastyear_log)
counts <- table(data$NLE_lifetime_sqrt)
counts <- table(data$NLE_lifetime_log)


# Bar plot
barplot(counts, 
        col = "skyblue",           # Bar color
        main = "Counts of NLE Lifetime",   # Title of the plot
        xlab = "NLE ",     # Label for the x-axis
        ylab = "Frequency")        # Label for the y-axis

### check sample

#are there gender differences? 
# Separate the data into two groups based on gender
boys <- data$ASR_ASCS_nohan[data$SEX == 1]
girls <- data$ASR_ASCS_nohan[data$SEX == 2]

sd(boys)
sd(girls)
# Perform a t-test
t_test_result <- t.test(boys, girls)

# Print the t-test result
print(t_test_result)

#check the effect of age
describe(data$ASR_ASCS_nohan)

# Scatter plot
plot(data$age8, data$ASR_ASCS_nohan, 
     xlab = "Age", 
     ylab = "Self-Control",
     main = "Scatter Plot of Age vs. Self-Control")

# Calculate correlation
cor.test(data$age8, data$ASR_ASCS_nohan)

# Print the correlation coefficient
cat("Correlation coefficient:", correlation, "\n")

# Optional: Fit a linear regression model
model <- lm(data$ASR_ASCS_nohan ~ data$age8)
summary(model)


#are there gender differences in life events? 
describe(data$NLE_lastyear)
describe(data$NLE_lifetime)

# Separate the data into two groups based on gender
boys <- data$NLE_lastyear[data$SEX == 1]
girls <- data$NLE_lastyear[data$SEX == 2]

sd(boys)
sd(girls)
# Perform a t-test
t_test_result <- t.test(boys, girls)

describe(data$NLE_lifetime)
# Separate the data into two groups based on gender
boys <- data$NLE_lifetime[data$SEX == 1]
girls <- data$NLE_lifetime[data$SEX == 2]

sd(boys)
sd(girls)
# Perform a t-test
t_test_result <- t.test(boys, girls)

# Print the t-test result
print(t_test_result)


#see if age is related to life events
cor.test(data$age8, data$NLE_lastyear)
cor.test(data$age8, data$NLE_lifetime)



###### Regressions #############

#fullmodel PGI predicting Health in all data
install.packages("geepack")
library(geepack)
install.packages("gee")
library(gee)

colnames(data)

data_pheno <- data %>%
  filter(complete.cases(ASR_ASCS_nohan, ASR_ASCS_nohan_log,NLE_lastyear_log,NLE_lifetime_log, NLE_lifetime, NLE_lastyear, age8 , SEX , age8*SEX)) 
                       

data_pgi <- data %>%
  filter(complete.cases(ASR_ASCS_nohan,ASR_ASCS_nohan_log,NLE_lastyear_log, NLE_lastyear_log, P_0_5_SCORE_ADHD_MRG16_LDp1, NLE_lastyear, age8 , SEX , age8*SEX , PC1_1KG , PC2_1KG , 
                          PC3_1KG , PC4_1KG , PC5_1KG , PC6_1KG , PC7_1KG , PC8_1KG , PC9_1KG , PC10_1KG , PLATFORM))

############################ life events
#untransformed
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(NLE_lastyear) + age8 + age_sq + SEX + age8*SEX, data=data_pheno, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

#transformed (shows similar results)
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(NLE_lastyear_log) + age8 + age_sq + SEX + age8*SEX , data=data_pheno, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


#untransformed data
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(NLE_lifetime) + age8 + age_sq + SEX + age8*SEX, data=data_pheno, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

#transformed data (shows similar results)
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(NLE_lifetime_log) + age8 + age_sq + SEX + age8*SEX , data=data_pheno, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)



############################ ADHD 

#1) main effects PGI ADHD on Self-control

#PGI 0.5 
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_0_5_SCORE_ADHD_MRG16_LDp1) + age8 + age_sq  + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

#PGI inf
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_ADHD_MRG16_LDp1) + age8 + age_sq  + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                       id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


#2) interaction effect with life events
#effect of PGI, life events last year, and PGI*life events last year
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_0_5_SCORE_ADHD_MRG16_LDp1) + scale(NLE_lastyear_log) + scale(P_0_5_SCORE_ADHD_MRG16_LDp1)*scale(NLE_lastyear_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_ADHD_MRG16_LDp1) + scale(NLE_lastyear_log) + scale(P_inf_SCORE_ADHD_MRG16_LDp1)*scale(NLE_lastyear_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


#effect of PGI, life events life time, and PGI*life events life time
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(P_0_5_SCORE_ADHD_MRG16_LDp1) + scale(NLE_lifetime) + 
                  scale(P_0_5_SCORE_ADHD_MRG16_LDp1)*scale(NLE_lifetime) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_0_5_SCORE_ADHD_MRG16_LDp1) + scale(NLE_lifetime_log) + 
                  scale(P_0_5_SCORE_ADHD_MRG16_LDp1)*scale(NLE_lifetime_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(P_inf_SCORE_ADHD_MRG16_LDp1) + scale(NLE_lifetime_log) + 
                  scale(P_inf_SCORE_ADHD_MRG16_LDp1)*scale(NLE_lifetime_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)



############################ Aggression

#1) main effects PGI Agg on Self-control
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_0_5_SCORE_Aggression_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_Aggression_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


#2) interaction effect with life events
#effect of PGI, life events last year, and PGI*life events last year
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(P_0_5_SCORE_Aggression_MRG16_LDp1) + scale(NLE_lastyear) + 
                  scale(P_0_5_SCORE_Aggression_MRG16_LDp1)*scale(NLE_lastyear) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_Aggression_MRG16_LDp1) + scale(NLE_lastyear_log) + 
                  scale(P_inf_SCORE_Aggression_MRG16_LDp1)*scale(NLE_lastyear_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)



#effect of PGI, life events lifetime, and PGI*life events lifetime
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(P_0_5_SCORE_Aggression_MRG16_LDp1) + scale(NLE_lifetime_log) + 
                  scale(P_0_5_SCORE_Aggression_MRG16_LDp1)*scale(NLE_lifetime_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


data_pgi$scaled_ADHD <- scale(data_pgi$P_inf_SCORE_ADHD_MRG16_LDp1)
describe(data_pgi$scaled_ADHD)

desc <- describe(data_pgi$P_inf_SCORE_Aggression_MRG16_LDp1)
desc$stats <- round(desc$stats, digits = 10)  # Round to 10 decimal places
print(desc)


mean(data_pgi$P_inf_SCORE_Aggression_MRG16_LDp1)
sd(data_pgi$P_inf_SCORE_Aggression_MRG16_LDp1)

mean(data_pgi$P_0_5_SCORE_Aggression_MRG16_LDp1)
sd(data_pgi$P_0_5_SCORE_Aggression_MRG16_LDp1)

mean(data_pgi$P_inf_SCORE_ADHD_MRG16_LDp1)
sd(data_pgi$P_inf_SCORE_ADHD_MRG16_LDp1)




# Now, column B in your data frame 'df' contains the standardized values of column A




############################ Executive functioning (EF)

#1) main effects PGI EF on Self-control
Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)


#2) interaction effect with life events
#effect of PGI, life events last year, and PGI*life events last year
Model <- geeglm(scale(ASR_ASCS_nohan) ~ scale(P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1) + scale(NLE_lastyear) + 
                  scale(P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1)*scale(NLE_lastyear) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

Model <- geeglm(scale(ASR_ASCS_nohan_log) ~ scale(P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1) + scale(NLE_lastyear_log) + 
                  scale(P_inf_SCORE_ExecutiveFunctioning_MRG16_LDp1)*scale(NLE_lastyear_log) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)






######## Life events & PGI AGG
#1) Life events last year on AGG
Model <- geeglm(scale(NLE_lastyear) ~ scale(P_0_5_SCORE_Aggression_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

#2) Life events lifetime on AGG
Model <- geeglm(scale(NLE_lifetime) ~ scale(P_0_5_SCORE_Aggression_MRG16_LDp1) + age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
                  PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=data_pgi, 
                id=FamilyNumber, family=gaussian, corstr = "exchangeable",na.action=na.omit )
summary(Model)

#correlations between PGI

cor.test(data$P_0_5_SCORE_Aggression_MRG16_LDp1,data$P_0_5_SCORE_ADHD_MRG16_LDp1)
cor.test(data$P_0_5_SCORE_Aggression_MRG16_LDp1,data$P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1)
cor.test(data$P_0_5_SCORE_ADHD_MRG16_LDp1,data$P_0_5_SCORE_ExecutiveFunctioning_MRG16_LDp1)


##### Interaction plots ####

describe(data_pgi$NLE_lifetime)
hist(data_pgi$NLE_lifetime)

# Assuming 'data_pgi' is your data frame
library(dplyr)

data_pgi <- data_pgi %>%
  mutate(NLE_lifetime_lowhigh = case_when(
    NLE_lifetime > 2.5 ~ 1,
    NLE_lifetime < 2.5 ~ 2,
    TRUE ~ NA_real_
  ))


Data_subset <- data_pgi[data_pgi$NLE_lifetime_lowhigh %in% c(1, 2), ]

#create residuals where you regress them out for age

reg= lm(ASR_ASCS_nohan ~ age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
          PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=Data_subset)
Data_subset$ASR_ASCS_nohan_age = residuals(reg)



#### Interaction effects NLE Lifetime

library(ggplot2)
library(patchwork)

# First ggplot
plot1 <- ggplot(Data_subset, aes(x = scale(P_inf_SCORE_ADHD_MRG16_LDp1), y = scale(ASR_ASCS_nohan_age), color = factor(NLE_lifetime_lowhigh))) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "PGI ADHD", y = "Self-control problems") +
  ggtitle("SC PGI ") +
  scale_color_manual(
    values = c("1" = "orange", "2" = "blue"),
    name = "NLE lifetime",
    labels = c("1" = "High stress", "2" = "Low Stress")
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1.5, 1.5))

# Second ggplot
plot2 <- ggplot(Data_subset, aes(x = scale(P_inf_SCORE_Aggression_MRG16_LDp1), y = scale(ASR_ASCS_nohan_age), color = factor(NLE_lifetime_lowhigh))) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "PGI Aggression", y = "Self-control problems") +
  ggtitle("SC PGI Agg") +
  scale_color_manual(
    values = c("1" = "orange", "2" = "blue"),
    name = "NLE Life time",
    labels = c("1" = "High stress", "2" = "Low Stress")
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1.5, 1.5))

# Combine plots side by side
combined_plots <- plot1 + plot2 + plot_layout(ncol = 2)

# Print or save the combined plot
print(combined_plots)








### Interaction effects NLE last year ####

colnames(data_pgi)
describe(data_pgi$NLE_lastyear)

data_pgi <- data_pgi %>%
  mutate(NLE_lastyear_lowhigh = case_when(
    NLE_lastyear > 0.42 ~ 1,
    NLE_lastyear < 0.42 ~ 2,
    TRUE ~ NA_real_
  ))

Data_subset <- data_pgi[data_pgi$NLE_lastyear_lowhigh %in% c(1, 2), ]

#create residuals where you regress them out for age

reg= lm(ASR_ASCS_nohan ~ age8 + age_sq + SEX + age8*SEX + PC1_1KG + PC2_1KG + 
          PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG + PLATFORM, data=Data_subset)
Data_subset$ASR_ASCS_nohan_age = residuals(reg)

ggplot(Data_subset, aes(x = scale(P_inf_SCORE_ADHD_MRG16_LDp1), y = scale(ASR_ASCS_nohan_age), color = factor(NLE_lifetime_lowhigh))) +
  geom_point(alpha = 0.05) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "PGI ADHD", y = "Self-control problems") +
  ggtitle("Regressing self-control problems on PGI ADHD") +
  scale_color_manual(
    values = c("1" = "orange", "2" = "blue"),
    name = "NLE lastyear",
    labels = c("1" = "High stress ", "2" = "Low Stress")
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-2.0, 2.0), ylim = c(-1.5, 1.5))

ggplot(Data_subset, aes(x = scale(P_inf_SCORE_Aggression_MRG16_LDp1), y = scale(ASR_ASCS_nohan_age), color = factor(NLE_lifetime_lowhigh))) +
  geom_point(alpha = 0.05) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "PGI Aggression", y = "Self-control problems") +
  ggtitle("Regressing self-control problems on PGI ADHD") +
  scale_color_manual(
    values = c("1" = "orange", "2" = "blue"),
    name = "NLE last year",
    labels = c("1" = "High stress ", "2" = "Low Stress")
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-2.0, 2.0), ylim = c(-1.5, 1.5))
