
library(dplyr)
#library(car)
library(readxl)
library(stargazer)
library(sandwich)
library(lmtest)
library(writexl)
library(openxlsx)
library(tidyr)
library(dplyr)

#Data Firm Data + Returns 
US_DATA <- read_excel("~/Desktop/DataBA/US_DATA.xlsx", 
                      sheet = "MergedData_US")
ROE<- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                 sheet = "Return_on_Equity")
MCAP <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                   sheet = "MCAP")
DIV <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                  sheet = "DivYield")
BM <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                 sheet = "BM")
GPM <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                  sheet = "GrossProfitMargin_Yearly")
Lev <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                  sheet = "Lev")
Profitability <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                            sheet = "Profitability")
ESG <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                  sheet = "ESG")
Returns <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                      sheet = "Returns")
ESG_high <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                       sheet = "ESG_Group")
E_times_med <- read_excel("Desktop/BA/Bachelorarbeit/US_RegressionData.xlsx", 
                          sheet = "Econ_times_PE_median_right")

#merged_data_manually <- read_excel("~/Desktop/merged_data2.xlsx")
merged_data2 <- read_excel("merged_data2.xlsx")

Transformed_Data <- read_excel("~/Desktop/Transformed_Data.xlsx")
merged_data_3 <- read_excel("~/Desktop/merged_data_3.xlsx")
merged_data_4 <- read_excel("~/Desktop/merged_data_4.xlsx")
data_clean2 <- merged_data_3[complete.cases(merged_data_3), ]
data_clean3 <- merged_data_4[complete.cases(merged_data_4), ]


winsorized_data_clean3 <- data_clean3

# Set the lower and upper percentile levels (5% and 95%)
lower_percentile <- 0.01
upper_percentile <- 0.99

# Calculate the percentiles for each column
ROE_1th_percentile <- quantile(data_clean3$ROE, lower_percentile)
ROE_99th_percentile <- quantile(data_clean3$ROE, upper_percentile)

Lev_1th_percentile <- quantile(data_clean3$Lev, lower_percentile)
Lev_99th_percentile <- quantile(data_clean3$Lev, upper_percentile)

ROA_1th_percentile <- quantile(data_clean3$ROA, lower_percentile)
ROA_99th_percentile <- quantile(data_clean3$ROA, upper_percentile)

BM_1th_percentile <- quantile(data_clean3$BM, lower_percentile)
BM_99th_percentile <- quantile(data_clean3$BM, upper_percentile)

Prof_1th_percentile <- quantile(data_clean3$Prof, lower_percentile)
Prof_99th_percentile <- quantile(data_clean3$Prof, upper_percentile)

# Winsorize the columns in the original data frame
winsorized_data_clean3$ROE <- ifelse(data_clean3$ROE < ROE_1th_percentile, ROE_1th_percentile,
                                     ifelse(data_clean3$ROE > ROE_99th_percentile, ROE_99th_percentile, data_clean3$ROE))

winsorized_data_clean3$Lev <- ifelse(data_clean3$Lev < Lev_1th_percentile, Lev_1th_percentile,
                                     ifelse(data_clean3$Lev > Lev_99th_percentile, Lev_99th_percentile, data_clean3$Lev))

winsorized_data_clean3$ROA <- ifelse(data_clean3$ROA < ROA_1th_percentile, ROA_1th_percentile,
                                     ifelse(data_clean3$ROA > ROA_99th_percentile, ROA_99th_percentile, data_clean3$ROA))

winsorized_data_clean3$BM <- ifelse(data_clean3$BM < BM_1th_percentile, BM_1th_percentile,
                                    ifelse(data_clean3$BM > BM_99th_percentile, BM_99th_percentile, data_clean3$BM))

winsorized_data_clean3$Prof <- ifelse(data_clean3$Prof < Prof_1th_percentile, Prof_1th_percentile,
                                    ifelse(data_clean3$Prof > Prof_99th_percentile, Prof_99th_percentile, data_clean3$Prof))

summary_stats <- function(df, var_name) {
  
  # Extract variable data
  data_vector <- df[[var_name]]
  
  # Coerce the data to numeric, excluding NAs and non-numeric values
  data_vector <- as.numeric(data_vector[!is.na(data_vector) & !is.infinite(data_vector)])
  
  # Compute statistics for the vector
  stats <- data.frame(
    Variable = var_name,
    Median = median(data_vector, na.rm = TRUE),
    Mean = mean(data_vector, na.rm = TRUE),
    Max = max(data_vector, na.rm = TRUE),
    Min = min(data_vector, na.rm = TRUE),
    SD = sd(data_vector, na.rm = TRUE)
  )
  
  return(stats)
}

# List of variables to compute statistics for
variables <- c("ROE", "Log_MCAP", "DIV", "BM", "Lev", "Prof", "ROA", "AbnormalReturns")

# Apply the function to each variable and bind into a table
stats_table <- do.call(rbind, lapply(variables, function(var) summary_stats(winsorized_data_clean3, var)))

# Print the summary statistics table
print(stats_table)



significance_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.1) return(".")
  return("")
}
### mean difference Test
# Teilen Sie die Daten basierend auf den Werten von ESG_Group
group_0 <- subset(winsorized_data_clean3, ESG_Group == 0)
group_1 <- subset(winsorized_data_clean3, ESG_Group == 1)

# Initialisieren Sie eine leere Liste für die Ergebnisse
t_test_results <- list()

# Führen Sie t-Tests für jede gewünschte Variable durch
variables <- c("ROE", "Log_MCAP", "DIV", "BM", "Lev", "Prof", "ROA", "AbnormalReturns")

for (var in variables) {
  result <- t.test(group_0[[var]], group_1[[var]])
  t_test_results[[var]] <- result
}
# Erstellen Sie eine leere Tabelle für die Ergebnisse
t_test_summary <- data.frame(
  Variable = character(),
  Mean_Difference = numeric(),
  t_value = numeric(),
  df = numeric(),
  p_value = numeric(),
  SD_group_0 = numeric(),
  SD_group_1 = numeric(),
  mean_group_0 = numeric(),
  mean_group_1 = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# Füllen Sie die Tabelle mit den Ergebnissen aus t_test_results
for (var in names(t_test_results)) {
  result <- t_test_results[[var]]
  
  mean_diff = result$estimate[1] - result$estimate[2]
  sd_group_0 = sd(group_0[[var]], na.rm = TRUE)
  sd_group_1 = sd(group_1[[var]], na.rm = TRUE)
  
  t_test_summary <- rbind(t_test_summary, data.frame(
    Variable = var,
    Mean_Difference = mean_diff,
    t_value = result$statistic,
    df = result$parameter,
    p_value = result$p.value,
    SD_group_0 = sd_group_0,
    SD_group_1 = sd_group_1,
    mean_group_0 = result$estimate[1],
    mean_group_1 = result$estimate[2],
    Significance = significance_stars(result$p.value)
  ))
}

# Zeigen Sie die Tabelle an
print(t_test_summary)


### T - Test Combined Groups

### mean difference Test für ESG_Group_Combined
# Teilen Sie die Daten basierend auf den Werten von ESG_Group_Combined
group_0_combined <- subset(winsorized_data_clean3, ESG_Group_Combined == 0)
group_1_combined <- subset(winsorized_data_clean3, ESG_Group_Combined == 1)

# Initialisieren Sie eine leere Liste für die Ergebnisse
t_test_results_combined <- list()

# Führen Sie t-Tests für jede gewünschte Variable durch
variables <- c("ROE", "Log_MCAP", "DIV", "BM", "Lev", "Prof", "ROA", "AbnormalReturns")

for (var in variables) {
  result <- t.test(group_0_combined[[var]], group_1_combined[[var]])
  t_test_results_combined[[var]] <- result
}

# Erstellen Sie eine leere Tabelle für die Ergebnisse
t_test_summary_combined <- data.frame(
  Variable = character(),
  Mean_Difference = numeric(),
  t_value = numeric(),
  df = numeric(),
  p_value = numeric(),
  SD_group_0 = numeric(),
  SD_group_1 = numeric(),
  mean_group_0 = numeric(),
  mean_group_1 = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# Füllen Sie die Tabelle mit den Ergebnissen aus t_test_results_combined
for (var in names(t_test_results_combined)) {
  result <- t_test_results_combined[[var]]
  
  mean_diff = result$estimate[1] - result$estimate[2]
  sd_group_0 = sd(group_0_combined[[var]], na.rm = TRUE)
  sd_group_1 = sd(group_1_combined[[var]], na.rm = TRUE)
  
  t_test_summary_combined <- rbind(t_test_summary_combined, data.frame(
    Variable = var,
    Mean_Difference = mean_diff,
    t_value = result$statistic,
    df = result$parameter,
    p_value = result$p.value,
    SD_group_0 = sd_group_0,
    SD_group_1 = sd_group_1,
    mean_group_0 = result$estimate[1],
    mean_group_1 = result$estimate[2],
    Significance = significance_stars(result$p.value)
  ))
}

# Zeigen Sie die Tabelle an
print(t_test_summary_combined)





###### Funktionirende Regression !! 
### EINFAHCE REG
library(stats)

data_clean <- Transformed_Data[complete.cases(Transformed_Data), ]
# Remove columns with NA valu

model1 <- lm(Returns ~ ROE + MCAP + DIV + BM + Lev + Prof + ESG + EconTimesMed + ESG_Group, data=winsorized_data_clean3)
summary(model1)

model2 <- lm(Returns ~ ROE + MCAP + DIV + BM + Lev + Prof + ESG + EconTimesMed + ESG_Group + ESG *EconTimesMed, data=winsorized_data_clean3)
summary(model2)


model1 <- lm(AbnormalReturns ~ Econ_Times_GDP, data=winsorized_data_clean3)
summary(model1)


### Regression: 1 nur Times und ESG gruppe
model1 <- lm(AbnormalReturns ~ EconTimesMed + ESG_Group + EconTimesMed*ESG_Group, data=winsorized_data_clean3)
summary(model1)
model2 <- lm(AbnormalReturns ~ Econ_Times_GDP + ESG_Group + Econ_Times_GDP*ESG_Group, data=winsorized_data_clean3)
summary(model2)
model3 <- lm(AbnormalReturns ~ Econ_Times_GDP + ESG_Group_Combined + Econ_Times_GDP*ESG_Group_Combined, data=winsorized_data_clean3)
summary(model3)
model4 <- lm(AbnormalReturns ~ EconTimesMed + ESG_Group_Combined + EconTimesMed*ESG_Group_Combined, data=winsorized_data_clean3)
summary(model4)
# Annahme: "data" ist Ihr Datensatz und "MCAP" ist die zu transformierende Variable

# Logarithmus von "MCAP" berechnen
winsorized_data_clean3$Log_MCAP <- log(winsorized_data_clean3$MCAP)
data_clean3$Log_MCAP <- log(data_clean3$MCAP)
data_clean_sector$Log_MCAP <- log(data_clean_sector$MCAP)

# Die transformierte Variable "Log_MCAP" ist jetzt im Datensatz "data" verfügbar


## Regression 2 
# mit log(MCAP)
model3 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group + ESG_Group * EconTimesMed, data=winsorized_data_clean3)
summary(model3)
model4 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group + ESG_Group * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model4)
model5 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group_Combined + ESG_Group_Combined * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model5)
model6 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group_Combined + ESG_Group_Combined * EconTimesMed, data=winsorized_data_clean3)
summary(model6)
## including financials, no FE  
model3 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group + ESG_Group * EconTimesMed, data=winsorized_data_clean3)
summary(model3)
model4 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group + ESG_Group * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model4)
model5 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group_Combined + ESG_Group_Combined * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model5)
model6 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group_Combined + ESG_Group_Combined * EconTimesMed, data=winsorized_data_clean3)
summary(model6)

### Reg 3 - 3 Single pillars - als Gruppe ? 

model7 <- lm(AbnormalReturns ~ EconTimesMed + governance + Social + Envirormental, data=winsorized_data_clean3)
summary(model7)
model8 <- lm(AbnormalReturns ~ EconTimesMed + governance + Social + Envirormental + EconTimesMed * governance + EconTimesMed* Social + EconTimesMed* Envirormental, data=winsorized_data_clean3)
summary(model8)
model9 <- lm(AbnormalReturns ~ Econ_Times_GDP + governance + Social + Envirormental + Econ_Times_GDP * governance + Econ_Times_GDP* Social + Econ_Times_GDP* Envirormental, data=winsorized_data_clean3)
summary(model9)
model10 <- lm(AbnormalReturns ~ Econ_Times_GDP + governance + Social + Envirormental, data=winsorized_data_clean3)
summary(model10)

## EVTL Reg 4: immer einzelen SIngle pillaars - wahrscheinlich nicht nehmen 

model11 <- lm(AbnormalReturns ~ EconTimesMed + governance + governance * EconTimesMed, data=data_clean3)
summary(model11)
model12 <- lm(AbnormalReturns ~ Econ_Times_GDP + governance + governance * Econ_Times_GDP, data=data_clean3)
summary(model12)

model13 <- lm(AbnormalReturns ~ EconTimesMed + Social + Social * EconTimesMed, data=data_clean3)
summary(model13)
model14 <- lm(AbnormalReturns ~ Econ_Times_GDP + Social + Social * Econ_Times_GDP, data=data_clean3)
summary(model14)

model15 <- lm(AbnormalReturns ~ EconTimesMed + Envirormental + Envirormental * EconTimesMed, data=data_clean3)
summary(model15)
model16 <- lm(AbnormalReturns ~ Econ_Times_GDP + Envirormental + Envirormental * Econ_Times_GDP, data=data_clean3)
summary(model16)

######### Reg 5: single pillars + financial metrics 

model17 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + governance + Social + Envirormental, data=data_clean3)
summary(model17)
model18 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + governance + Social + Envirormental, data=data_clean3)
summary(model18)
model19 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + governance *EconTimesMed + Social *EconTimesMed + Envirormental*EconTimesMed, data=data_clean3)
summary(model19)
model20 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + governance *Econ_Times_GDP + Social *Econ_Times_GDP + Envirormental*Econ_Times_GDP, data=data_clean3)
summary(model20)
## mit log MCAP
model17 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + governance + Social + Envirormental, data=data_clean3)
summary(model17)
model18 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + governance + Social + Envirormental, data=data_clean3)
summary(model18)
model19 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + governance *EconTimesMed + Social *EconTimesMed + Envirormental*EconTimesMed, data=data_clean3)
summary(model19)
model20 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + governance *Econ_Times_GDP + Social *Econ_Times_GDP + Envirormental*Econ_Times_GDP, data=data_clean3)
summary(model20)
#############



# Industry FE
merged_data_4_with_sectors <- read_excel("Desktop/merged_data_4_with_sectors.xlsx")
data_clean_sector <- merged_data_4_with_sectors[complete.cases(merged_data_4_with_sectors), ]

data_clean_sector$Sector <- factor(data_clean_sector$Sector)
industry_dummies <- model.matrix(~ Sector - 1, data=data_clean_sector)
data_clean_sector <- cbind(data_clean_sector, industry_dummies)


model21 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group + ESG_Group:EconTimesMed + Sector, data=data_clean_sector)
summary(model21)
model22 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group_Combined + ESG_Group_Combined:EconTimesMed + Sector, data=data_clean_sector)
summary(model22)
model23 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group_Combined + ESG_Group_Combined:Econ_Times_GDP + Sector, data=data_clean_sector)
summary(model23)
model24 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group + ESG_Group:Econ_Times_GDP + Sector, data=data_clean_sector)
summary(model24)
##mit log MCAP
model21 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group + ESG_Group:EconTimesMed + Sector, data=data_clean_sector)
summary(model21)
model22 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group_Combined + ESG_Group_Combined:EconTimesMed + Sector, data=data_clean_sector)
summary(model22)
model23 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group_Combined + ESG_Group_Combined:Econ_Times_GDP + Sector, data=data_clean_sector)
summary(model23)
model24 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group + ESG_Group:Econ_Times_GDP + Sector, data=data_clean_sector)
summary(model24)


model7 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + EconTimesMed + ESG_Group_Combined + ROA + EconTimesMed * ESG_Group_Combined, data=data_clean2)
summary(model7)
model8 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + EconTimesMed + ESG_Group_Combined + ROA + EconTimesMed * ESG_Group_Combined, data=data_clean3)
summary(model8)
model9 <- lm(AbnormalReturns ~ governance + Social + Envirormental + EconTimesMed + governance *EconTimesMed + EconTimesMed * Social + Envirormental * EconTimesMed, data=data_clean3)
summary(model9)

## 25% Test
# Berechne die 25% und 75% Quartile der ESG Scores
esg_25 <- quantile(winsorized_data_clean3$ESG, 0.25)
esg_75 <- quantile(winsorized_data_clean3$ESG, 0.75)

# Erstelle eine neue Spalte "ESG_Category" mit Werten 0 oder 1
winsorized_data_clean3$ESG_Category25 <- ifelse(winsorized_data_clean3$ESG < esg_25, 0, ifelse(winsorized_data_clean3$ESG > esg_75, 1, NA))
# Berechne die 25% und 75% Quartile der ESG Scores
esg_c_25 <- quantile(winsorized_data_clean3$ESG_Combined, 0.25)
esg_c_75 <- quantile(winsorized_data_clean3$ESG_Combined, 0.75)

# Erstelle eine neue Spalte "ESG_Category" mit Werten 0 oder 1
winsorized_data_clean3$ESG_Combined_Category25 <- ifelse(winsorized_data_clean3$ESG_Combined < esg_c_25, 0, ifelse(winsorized_data_clean3$ESG_Combined > esg_c_75, 1, NA))

model25 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Category25 + ESG_Category25 * EconTimesMed, data=winsorized_data_clean3)
summary(model25)
model26 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Category25 + ESG_Category25 * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model26)
model27 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Combined_Category25 + ESG_Combined_Category25 * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model27)
model28 <- lm(AbnormalReturns ~ ROE + Log_MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Combined_Category25 + ESG_Combined_Category25 * EconTimesMed, data=winsorized_data_clean3)
summary(model28)


### invert+

winsorized_data_clean3$ESG_Group_Inverted <- 1 - winsorized_data_clean3$ESG_Group
winsorized_data_clean3$ESG_Combined_Group_Inverted <- 1 - winsorized_data_clean3$ESG_Group_Combined
winsorized_data_clean3$ESG_Category_Inverted <- 1 - winsorized_data_clean3$ESG_Category25
winsorized_data_clean3$ESG_Combined_Category_Inverted <- 1 - winsorized_data_clean3$ESG_Combined_Category25
winsorized_data_clean3$EconTimesMedBad <- 1 - winsorized_data_clean3$EconTimesMed
winsorized_data_clean3$Econ_Times_GDPBad <- 1 - winsorized_data_clean3$Econ_Times_GDP

## Bad Times
## including financials, no FE  
model29 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMedBad + ESG_Group + ESG_Group * EconTimesMedBad, data=winsorized_data_clean3)
summary(model29)
model30 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDPBad + ESG_Group + ESG_Group * Econ_Times_GDPBad, data=winsorized_data_clean3)
summary(model30)
model31 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDPBad + ESG_Group_Combined + ESG_Group_Combined * Econ_Times_GDPBad, data=winsorized_data_clean3)
summary(model31)
model32 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMedBad + ESG_Group_Combined + ESG_Group_Combined * EconTimesMedBad, data=winsorized_data_clean3)
summary(model32)
## schelcghte Firmen 
model33 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Group_Inverted + ESG_Group_Inverted * EconTimesMed, data=winsorized_data_clean3)
summary(model33)
model34 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Group_Inverted + ESG_Group_Inverted * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model34)
model35 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Combined_Group_Inverted + ESG_Combined_Group_Inverted * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model35)
model36 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Combined_Group_Inverted + ESG_Combined_Group_Inverted * EconTimesMed, data=winsorized_data_clean3)
summary(model36)
#schlechte Firmen mit 25% 
model37 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Category_Inverted + ESG_Category_Inverted * EconTimesMed, data=winsorized_data_clean3)
summary(model37)
model38 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + ESG_Category_Inverted + ESG_Category_Inverted * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model38)
model39 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + Econ_Times_GDP + ESG_Combined_Category_Inverted + ESG_Combined_Category_Inverted * Econ_Times_GDP, data=winsorized_data_clean3)
summary(model39)
model40 <- lm(AbnormalReturns ~ ROE + MCAP + DIV + BM + Lev + Prof + ROA + EconTimesMed + ESG_Combined_Category_Inverted + ESG_Combined_Category_Inverted * EconTimesMed, data=winsorized_data_clean3)
summary(model40)

