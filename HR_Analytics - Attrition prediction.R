library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)

hr_data <- read_csv("HR_Analytics.csv")

summary(hr_data)

################################################################################
########################## PRIPREMA PODATAKA ###################################
################################################################################

str(hr_data)

# Funkcije za prikaz
plot_histogram <- function(data, column, binwidth=10) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = binwidth, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = paste("Raspodela za", column), x = column, y = "Broj zaposlenih") +
    theme_minimal()
}

plot_bar <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = paste("Raspodela za", column), x = column, y = "Broj zaposlenih") +
    theme_minimal()
}

plot_boxplot <- function(data, column, name) {
  data %>% ggplot() +
    geom_boxplot(aes(y=column)) + 
    labs(y = name)
}
# Kraj funkcija za prikaz

# Izracunavanje jedinstvenih vrednosti
unique_values_summary <- hr_data %>%
  summarise(across(everything(), 
                   list(unique_values = ~ length(unique(.)), 
                        sample_values = ~ paste(unique(.)[1:5], collapse = ", "))))

# EmpID pocetak
print(unique_values_summary$EmpID_unique_values)

print(hr_data[duplicated(hr_data$EmpID), ]$EmpID)
print(hr_data[duplicated(hr_data), ])

hr_data <- hr_data[!duplicated(hr_data), ]
nrow(hr_data)
# EmpID kraj

# Attrition pocetak
attrition_summary <- hr_data %>%
  group_by(Attrition) %>%
  summarise(Broj = n())

print(attrition_summary)
ggplot(attrition_summary, aes(x = Attrition, y = Broj, fill = Attrition)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = Broj), vjust = -0.5) +
  labs(title = "Raspodela Attrition", x = "Attrition", y = "Broj zaposlenih") +
  theme_minimal()

hr_data$Attrition_binary <- ifelse(hr_data$Attrition == "Yes", 1, 0)
# Attrition kraj

# Age pocetak
summary(hr_data$Age)
plot_histogram(hr_data, "Age", binwidth=2)
# Age kraj

# AgeGroup pocetak
print(unique_values_summary$AgeGroup_unique_values)
print(unique_values_summary$AgeGroup_sample_values)
plot_bar(hr_data, "AgeGroup")

hr_data$AgeGroup = factor(hr_data$AgeGroup)
# AgeGroup kraj

# BusinessTravel pocetka
print(unique_values_summary$BusinessTravel_unique_values)
unique_values_summary$BusinessTravel_sample_values <- strsplit(unique_values_summary$BusinessTravel_sample_values[1], ", ")[1]
print(unique_values_summary$BusinessTravel_sample_values[[1]][1:unique_values_summary$BusinessTravel_unique_values])

hr_data <- hr_data %>%
  mutate(BusinessTravel = ifelse(BusinessTravel == "TravelRarely", "Travel_Rarely", BusinessTravel))

unique_values_summary <- hr_data %>%
  summarise(across(everything(), 
                   list(unique_values = ~ length(unique(.)), 
                        sample_values = ~ paste(unique(.)[1:5], collapse = ", "))))

print(unique_values_summary$BusinessTravel_unique_values)
unique_values_summary$BusinessTravel_sample_values <- strsplit(unique_values_summary$BusinessTravel_sample_values[1], ", ")[1]
print(unique_values_summary$BusinessTravel_sample_values[[1]][1:unique_values_summary$BusinessTravel_unique_values])

plot_bar(hr_data, "BusinessTravel")
# BusinessTravel kraj

# StandardHours pocetak
summary(hr_data$StandardHours)
# Sve vrednosti su jednake, mozemo ukloniti ovu kolonu
hr_data$StandardHours <- NULL
# StandardHours kraj

# HourlyRate pocetak
plot_histogram(hr_data, "HourlyRate", binwidth=5)
plot_boxplot(hr_data, hr_data$HourlyRate, "Hourly Rate")
# HourlyRate kraj

# DailyRate pocetak
plot_boxplot(hr_data, hr_data$DailyRate, "Daily Rate")

hr_data$DailyRate <- NULL
# DailyRate kraj

# MonthlyRate pocetak
plot_boxplot(hr_data, hr_data$MonthlyRate, "Monthly Rate")
hr_data$MonthlyRate <- NULL
# MonthlyRate kraj

# MonthlyIncome pocetak
plot_histogram(hr_data, "MonthlyIncome", binwidth=1000)
plot_boxplot(hr_data, hr_data$MonthlyIncome, "Monthly Income")
ggplot(hr_data, aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot()
# MonthlyIncome kraj

# SalarySlab pocetak
plot_bar(hr_data, "SalarySlab")
# bolje je kreirati nasu podelu kako bismo imali uniformnije podatke pa cemo ovu kolonu obrisati
hr_data$SalarySlab <- NULL
# SalarySlab kraj

# OverTime pocetak
plot_bar(hr_data, "OverTime")
hr_data$OverTime <- ifelse(hr_data$OverTime == "Yes", 1, 0)
# OverTime kraj

# Department pocetak
plot_bar(hr_data, "Department")

unique(hr_data$Department)
table(hr_data$Department)
hr_data$Department <- factor(hr_data$Department)
# Department kraj

# DistanceFromHome pocetak
plot_histogram(hr_data, "DistanceFromHome", binwidth=2)
plot_boxplot(hr_data, hr_data$DistanceFromHome, "Distance From Home")

hr_data$DistanceFromHomeGroup <- ifelse(hr_data$DistanceFromHome <= 1, "Jako blizu", #mozda spojiti 1 i 2
                                        ifelse(hr_data$DistanceFromHome == 2, "Blizu",
                                               ifelse(hr_data$DistanceFromHome <= 6, "Srednje",
                                                      ifelse(hr_data$DistanceFromHome <= 10, "Daleko", "Jako daleko"))))
plot_bar(hr_data, "DistanceFromHomeGroup")
hr_data$DistanceFromHomeGroup <- factor(hr_data$DistanceFromHomeGroup)
# DistanceFromHome kraj

# Education pocetak
hr_data$EducationText <- factor(hr_data$Education, levels = c(1,2,3,4,5),
                            labels = c("Primary", "Secondary", "Tertiary", "Masters", "Doctorate"))
plot_bar(hr_data, "EducationText")
# Education kraj

# EducationField pocetak
plot_bar(hr_data, "EducationField")
# Zasto ??? 
hr_data$EducationField <- NULL
# EducationField kraj

# EnvironmentSatisfaction pocetak
plot_bar(hr_data, "EnvironmentSatisfaction")
# EnvironmentSatisfaction kraj

# Gender pocetak
plot_bar(hr_data, "Gender")
# Gender kraj

# JobInvolvement pocetak
hr_data$JobInvolvementText <- factor(hr_data$JobInvolvement, levels = c(1,2,3,4),
                                 labels = c("Low", "Medium", "High", "Very High"))
plot_bar(hr_data, "JobInvolvementText")
# JobInvolvement kraj

# JobLevel pocetak
hr_data$JobLevelText <- factor(hr_data$JobLevel, levels = c(1, 2, 3, 4, 5), 
                           labels = c("Entry Level", "Junior", "Mid Level", "Senior", "Executive"))
plot_bar(hr_data, "JobLevelText")
# JobLevel kraj

# JobRole pocetak
plot_bar(hr_data, "JobRole")
hr_data$JobRole = factor(hr_data$JobRole)
# JobRole kraj

# JobSatisfaction pocetak
hr_data$JobSatisfaction <- factor(hr_data$JobSatisfaction, levels = c(1, 2, 3, 4), 
                                  labels = c("Very Dissatisfied", "Dissatisfied", "Satisfied", "Very Satisfied"))
plot_bar(hr_data, "JobSatisfaction")

hr_data$JobSatisfaction = factor(hr_data$JobSatisfaction)
# JobSatisfaction kraj

# MaritalStatus pocetak
plot_bar(hr_data, "MaritalStatus")
# MaritalStatus kraj

# NumCompaniesWorked pocetak
plot_histogram(hr_data, "NumCompaniesWorked", binwidth=1)
plot_boxplot(hr_data, hr_data$NumCompaniesWorked, "Number of Companies Worked")

hr_data$NumCompaniesWorked = factor(hr_data$NumCompaniesWorked)
# NumCompaniesWorked kraj

# PercentSalaryHike pocetak
summary(hr_data$PercentSalaryHike)
plot_boxplot(hr_data, hr_data$PercentSalaryHike, "Salary Hike in percentage")
# PercentSalaryHike kraj

# PerformanceRating pocetak
rating_data <- hr_data %>%
  group_by(PerformanceRating) %>%
  summarise(Count = n())

ggplot(rating_data, aes(x = "", y = Count, fill = factor(PerformanceRating))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Raspodela ocena performansi zaposlenih", fill = "Ocena performansi") +
  theme_minimal()
# PerformanceRating kraj

# RelationshipSatisfaction pocetak
hr_data$RelationshipSatisfaction <- factor(hr_data$RelationshipSatisfaction, levels = c(1, 2, 3, 4), 
                                           labels = c("Very Dissatisfied", "Dissatisfied", "Satisfied", "Very Satisfied"))
plot_bar(hr_data, "RelationshipSatisfaction")
# RelationshipSatisfaction kraj

# StockOptionLevel pocetak
hr_data$StockOptionLevelText <- factor(hr_data$StockOptionLevel, levels = c(0, 1, 2, 3), 
                                   labels = c("No Stock Options", "Minimal Stock Options", 
                                              "Moderate Stock Options", "Substantial Stock Options"))
plot_bar(hr_data, "StockOptionLevelText")
# StockOptionLevel kraj 

# TotalWorkingYears pocetak
plot_histogram(hr_data, "TotalWorkingYears", binwidth=3)
plot_boxplot(hr_data, hr_data$TotalWorkingYears, "TotalWorkingYears")

hr_data$TotalWorkingYearsGroup <- ifelse(hr_data$TotalWorkingYears <= 1, "0-1",
                                         ifelse(hr_data$TotalWorkingYears <= 5, "2-5",
                                                ifelse(hr_data$TotalWorkingYears <= 10, "5-10",
                                                       ifelse(hr_data$TotalWorkingYears <= 20, "10-20", "20+"))))
plot_bar(hr_data, "TotalWorkingYearsGroup") 
# TotalWorkingYears kraj

# TrainingTimesLastYear pocetak
plot_histogram(hr_data, "TrainingTimesLastYear", binwidth=1)
# TrainingTimesLastYear kraj

# WorkLifeBalance
plot_bar(hr_data, "WorkLifeBalance")
# WorkLifeBalance

# YearsAtCompany pocetak
plot_histogram(hr_data, "YearsAtCompany", binwidth=5)
plot_boxplot(hr_data, hr_data$YearsAtCompany, "YearsAtCompany")
hr_data$YearsAtCompanyGroup <- ifelse(hr_data$YearsAtCompany <= 2, "0-2",
                                      ifelse(hr_data$YearsAtCompany <= 5, "2-3",
                                             ifelse(hr_data$YearsAtCompany <= 10, "5-10",
                                                    ifelse(hr_data$YearsAtCompany <= 20, "10-20", "20+"))))
# YearsAtCompany kraj

# YearsInCurrentRole pocetak
plot_histogram(hr_data, "YearsInCurrentRole", binwidth=3)
# YearsInCurrentRole kraj

# YearsSinceLastPromotion pocetak
plot_histogram(hr_data, "YearsSinceLastPromotion", binwidth=5)
# YearsSinceLastPromotion kraj

# YearsWithCurrentManager pocetak
plot_histogram(hr_data, "YearsWithCurrManager", binwidth=2)
summary(hr_data$YearsWithCurrManager)
#YearsWithCurrManager ima NA vrednosti treba ih popuniti jer ih ima samo 57 od ukupnih 1473 podatka

#Popunjavamo sa Mean

#hr_data <- hr_data %>%  mutate(YearsWithCurrManager = if_else(
#  is.na
#  (YearsWithCurrManager), mean(YearsWithCurrManager, na.rm = TRUE), YearsWithCurrManager))

#Popunjavanje na osnovu AgeGroup kojoj pripada radnik, bolje resenje

hr_data <- hr_data %>%  group_by(AgeGroup) %>%  
                        mutate(YearsWithCurrManager = if_else(is.na(YearsWithCurrManager), 
                                                               mean(YearsWithCurrManager, na.rm = TRUE), 
                                                               YearsWithCurrManager)) %>% 
            ungroup()

summary(hr_data$YearsWithCurrManager)
plot_histogram(hr_data, "YearsWithCurrManager", binwidth=5)
# YearsWithCurrentManager kraj

#isti podaci za sve moze da se brise
hr_data$Over18 <- NULL
hr_data$EmployeeCount <- NULL
hr_data$EmpID <- NULL

#Feature engineering

hr_data$YearsInCurrentRolePerAge <- hr_data$YearsInCurrentRole / hr_data$Age

hr_data$ExperienceBeforeCurrentRole = hr_data$TotalWorkingYears - hr_data$YearsInCurrentRole


################################################################################
############################### ANALIZA ########################################
################################################################################

ggplot(hr_data, aes(x = Attrition, fill = 
                      factor(Gender))) +
  geom_bar(position = "dodge") +
  ylab("Gender") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

gender.attrition <- xtabs(~ Gender + Attrition, data = hr_data)
gender.attrition

gender.attrition <- as.data.frame(gender.attrition) %>%
  group_by(Gender) %>%
  mutate(Percentage = (Freq / sum(Freq)) * 100) %>%
  ungroup()

gender.attrition

#nebitna kolona Gender brisemo je 
#podjednako napustaju i muskarci i zene
hr_data$Gender <- NULL

ggplot(hr_data, aes(x = factor(Attrition), fill = AgeGroup)) +
  geom_bar(position = "dodge") +
  ylab("Age Group") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

ageGroup.attrition <- xtabs(~ AgeGroup + Attrition, data = hr_data)
ageGroup.attrition

ageGroup.attrition <- as.data.frame(ageGroup.attrition) %>%
  group_by(AgeGroup) %>%
  mutate(Percentage = (Freq / sum(Freq)) * 100) %>%
  ungroup()
ageGroup.attrition

#Procentualno najvise naputaju ljudi od 18 do 25 god, dok najveci broj ljudi 
#napusta kompaniju izmedju 26 i 35 godina


ggplot(hr_data, aes(x = factor(Attrition), fill = Department)) +
  geom_bar(position = "dodge") +
  ylab("Department") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

department.attrition <- xtabs(~ Department + Attrition, data = hr_data)
department.attrition

department.attrition.prop <- prop.table(department.attrition, margin = 1)
department.attrition.prop
#primecujemo da je mala razlika u procentima prema odeljenjima ali da 
#Sales ima najveci procenat ljudi koji su napustili


ggplot(hr_data, aes(x = factor(Attrition), fill = JobSatisfaction)) +
  geom_bar(position = "dodge") +
  ylab("JobSatisfaction") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

jobSatisfaction.attrition <- xtabs(~ JobSatisfaction + Attrition, data = hr_data)
jobSatisfaction.attrition

jobSatisfaction.attrition.prop <- prop.table(jobSatisfaction.attrition, margin = 1)
jobSatisfaction.attrition.prop

#ljdui koji su najmanje zadovoljni poslom najvise napustaju firmu sto je i logicno ali to nije neki prevelika razlika


ggplot(hr_data, aes(x = factor(Attrition), fill = NumCompaniesWorked)) +
  geom_bar(position = "dodge") +
  ylab("NumCompaniesWorked") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

numCompaniesWorkedn.attrition <- xtabs(~ NumCompaniesWorked + Attrition, data = hr_data)
numCompaniesWorkedn.attrition

numCompaniesWorkedn.attrition.prop <- prop.table(numCompaniesWorkedn.attrition, margin = 1)
numCompaniesWorkedn.attrition.prop

#ljudi koji su radili u 5+ firmi cesce napustaju posao sto je i logicno



data <- hr_data %>%
  group_by(JobSatisfaction, DistanceFromHomeGroup) %>%
  summarise(attrition_count = sum(Attrition_binary))

bar_plot <- ggplot(data, aes(x = DistanceFromHomeGroup, y = attrition_count, fill = JobSatisfaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Attrition by DistanceFromHomeGroup and JobSatisfcation",
       x = "DistanceFromHomeGroup",
       y = "Attrition - Yes",
       fill = "JobSatisfaction") +
  theme(axis.text.x = element_text(hjust = 1),legend.position = 
          "bottom",
        panel.grid.major.x = element_blank())
bar_plot
#Vidimo da najvise ljudi napusta firmu ukoliko se nalaze daleko od firme,
#moze nam biti vrlo bitan prediktor


ggplot(hr_data, aes(x = factor(Attrition), fill = JobRole)) +
  geom_bar(position = "dodge") +
  ylab("JobRole") +
  xlab("Attrition") +
  scale_x_discrete(labels = c("No", "Yes"))

jobRole.attrition <- xtabs(~ JobRole + Attrition, data = hr_data)
jobRole.attrition

jobRole.attrition.prop <- prop.table(jobRole.attrition, margin = 1)
jobRole.attrition.prop
#pozicija koja najcesce napusta firmu jeste   Sales Representative zatim Laboratory Technician i Human Resources odnosno najnize pozicije i one kojih ima najvise
#najmanja sansa da napuste firmu jeste za pozicije Manager, Research Director i Manufacturing Director odnosno najvise pozicije


ggplot(hr_data, aes(x = JobRole, y = PercentSalaryHike)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "PercentSalaryHike prema JobRole", 
       x = "JobRole", 
       y = "PercentSalaryHike") +
  theme_minimal()

#slicna povecanja plata u svim rolama, mozda malo veci Sales Representive i Manufacturing Director


ggplot(hr_data, aes(x = AgeGroup, y = PercentSalaryHike)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "PercentSalaryHike prema AgeGroup", 
       x = "AgeGroup", 
       y = "PercentSalaryHike") +
  theme_minimal()

#podjednaka povecavanja plata imaju sve starosne grupe


########################################
data <- hr_data %>%
  pivot_longer(cols = c("YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager"),
               names_to = "variable", values_to = "value")

ggplot(data, aes(x = Department, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Department Analysis", x = "Department", y = "Years") +
  theme_minimal()

#######################################
data <- hr_data %>%
  pivot_longer(cols = c("YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager"),
               names_to = "variable", values_to = "value")

# Kreirajte boxplot za različite varijable po JobRole
ggplot(data, aes(x = JobRole, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Job Role Analysis", x = "Job Role", y = "Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################

ggplot(hr_data, aes(x = JobRole, y = Age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Age by Job Role", x = "Job Role", y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#za bolje pozicije koje smo utvrdili da ljudi skoro ne napustaju potrebne su starije osobe, mladje osobe su na pozicijama koje se pre napustaju


ggplot(hr_data, aes(x = as.factor(JobSatisfaction), y = MonthlyIncome, fill = Attrition)) +
  geom_violin(trim = FALSE, adjust = 1.5) +
  scale_fill_manual(values = c("#397AAD", "#F11111")) +  # Prilagođene boje za Attrition
  labs(title = "Distribucija Monthly Income prema Job Satisfaction i Attrition", 
       x = "Job Satisfaction (1-4)", 
       y = "Monthly Income") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# heat mapa za podatke o Mesecnim prihodima i nivou obrazovanja
heatmap_data <- hr_data %>%
  group_by(Education, Attrition) %>%
  summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE)) %>%
  ungroup()

ggplot(heatmap_data, aes(x = Education, y = Attrition, fill = AverageIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap-a prosecnog Monthly Income-a prema Education koloni podeljena prema Attrition-u",
       x = "Education",
       y = "Attrition") +
  theme_minimal()
 
# Mozemo zakljuciti da kompaniju uglavnom napustaju ljudi koji imaju manji mesecni prihod a vece obrazovanje

# MartialStatus i BusinessTravel zajedno sa Attrition

summary_data <- hr_data %>%
  group_by(MaritalStatus, BusinessTravel, Attrition) %>%
  summarise(Count = n(), .groups = 'drop')

summary_data %>% ggplot(aes(x = MaritalStatus, y = Count, fill = factor(Attrition))) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked position
  labs(title = "Udeo Attrition naspram Marital Status i Business Travel kolona",
       x = "Marital Status",
       y = "Udeo",
       fill = "Attrition") +
  scale_fill_manual(values = c("Yes" = "#F11111", "No" = "#397AAD"),
                    labels = c("Yes" = "Yes", "No" = "No")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ BusinessTravel)

# mozemo uvideti da posao najcesce menjaju ljudi koji cesto putuju bez obzira na njihov bracni status

# WorkLifeBalance, MonthlyIncome i Attrition
summary_data <- hr_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE), .groups = 'drop')

ggplot(summary_data, aes(x = WorkLifeBalance, y = factor(Attrition), fill = AverageIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2171b5") +
  labs(title = "Heatmap of Average Monthly Income by Work-Life Balance and Attrition",
       x = "Work-Life Balance",
       y = "Attrition Status",
       fill = "Average Monthly Income") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# WorkLifeBalance, YearsAtCompany i Attrition
summary_data <- hr_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(AverageYears = mean(YearsAtCompany, na.rm = TRUE), .groups = 'drop')

ggplot(summary_data, aes(x = WorkLifeBalance, y = factor(Attrition), fill = AverageYears)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2171b5") +
  labs(title = "Heatmap of Average Years at Company by Work-Life Balance and Attrition",
       x = "Work-Life Balance",
       y = "Attrition Status",
       fill = "Average Years at Company") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# Vidimo da zaposleni koji su pronasli dobar balans duze ostaju i firme i manje su verovatnoce da ce je napustiti



# WorkLifeBalance, YearsAtCompany i Attrition
summary_data <- hr_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(AverageYears = mean(YearsWithCurrManager, na.rm = TRUE), .groups = 'drop')

ggplot(summary_data, aes(x = WorkLifeBalance, y = factor(Attrition), fill = AverageYears)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2171b5") +
  labs(title = "Heatmap of Average Years with Current Manager by Work-Life Balance and Attrition",
       x = "Work-Life Balance",
       y = "Attrition Status",
       fill = "Average Years with Current Manager") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
# Vidimo da zaposleni koji su pronasli dobar balans i dugo su pod istim menadzerom
# duze ostaju u firmi i manje su verovatnoce da ce je napustiti

heatmap_data <- hr_data %>%
  group_by(JobInvolvement, Attrition) %>%
  summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE)) %>%
  ungroup()

ggplot(heatmap_data, aes(x = JobInvolvement, y = Attrition, fill = AverageIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap-a prosecnog Monthly Income-a prema JobInvolvement koloni podeljena prema Attrition-u",
       x = "JobInvolvement",
       y = "Attrition") +
  theme_minimal()

cor(hr_data$JobInvolvement, hr_data$MonthlyIncome, method = "spearman")
# Vidimo da JobInvolvment i MonthlyIncome nemaju jaku korelaciju i da najveci prosecni income imaju
# zaposleni koji nisu na rukovodecim pozicijama

ggplot(hr_data, aes(x = TotalWorkingYears, y = MonthlyIncome, color = YearsAtCompany)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Veza između ukupnih radnih godina i plate",
       x = "Total Working Years",
       y = "Monthly Income") +
  theme_minimal()

heatmap_data <- hr_data %>%
  group_by(JobLevel  , Attrition) %>%
  summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE)) %>%
  ungroup()

ggplot(heatmap_data, aes(x = JobLevel  , y = Attrition, fill = AverageIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heatmap-a prosecnog Monthly Income-a prema JobLevel koloni podeljena prema Attrition-u",
       x = "JobLevel  ",
       y = "Attrition") +
  theme_minimal()

cor(hr_data$JobLevel, hr_data$MonthlyIncome, method = "spearman")
# Vidimo da ovde raste Monthly Income proporcionalno prema JobLevelu i da ove dve kolone imaju veliku korelaciju

hr_data$JobLevel <- NULL

cor(hr_data$TotalWorkingYears, hr_data$MonthlyIncome, method = "spearman")
# Vidimo da ovde raste Monthly Income proporcionalno prema JobLevelu i da ove dve kolone imaju veliku korelaciju

hr_data$TotalWorkingYears <- NULL

ggplot(hr_data, aes(x = factor(StockOptionLevel), y = JobLevel, color = factor(StockOptionLevel))) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, color = "black") +
  labs(
    title = "Veza između broja godina u kompaniji i nivoa opcija za kupovinu akcija",
    x = "StockOptionLevel",
    y = "YearsAtCompany",
    color = "StockOptionLevel"
  ) +
  theme_minimal()

ggplot(hr_data, aes(x = factor(StockOptionLevel), y = MonthlyIncome, fill = factor(StockOptionLevel))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Месечна плата по нивоу могућности куповине акција",
    x = "Stock Option Level",
    y = "Monthly Income"
  ) +
  theme_minimal()

data <- hr_data %>%
  group_by(StockOptionLevel, Attrition) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(data %>% filter(Attrition == "Yes"), aes(x = factor(StockOptionLevel), y = percentage, fill = factor(StockOptionLevel))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Проценат запослених који су напустили компанију по нивоу StockOptionLevel",
    x = "Stock Option Level",
    y = "Проценат напуштања (%)"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal()



################################################################################
################################ MODELI ########################################
################################################################################

hr_data.final <- select(hr_data, MonthlyIncome, Attrition_binary, DistanceFromHome, Age, OverTime, StockOptionLevel, WorkLifeBalance, JobInvolvement, Education, YearsInCurrentRole, ExperienceBeforeCurrentRole) #YearsInCurrentRolePerAge)
summary(hr_data.final)

# Korelaciona matrica
cor_matrix <- cor(hr_data.final, use = "complete.obs")

# HeatMapa
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("yellow", "green", "blue"))(200), 
         title = "Correlation before Oversampling", 
         tl.cex = 0.8,
         number.cex = 0.7,
         addCoef.col = "black",
         mar = c(0,0,2,0))

hr_data.final$Attrition_binary <- factor(hr_data$Attrition_binary)
# Podela podataka na trening i test set

set.seed(21)

train_index <- createDataPartition(hr_data.final$Attrition_binary, 
                                   p = 0.8, 
                                   list = FALSE)
train_data <- hr_data.final[train_index, ]
test_data <- hr_data.final[-train_index, ]

prop.table(table(hr_data.final$Attrition_binary))
prop.table(table(train_data$Attrition_binary))
prop.table(table(test_data$Attrition_binary))

test_plot <- ggplot(test_data, aes(x = factor(Attrition_binary))) + 
  geom_bar() + 
  labs(title = "Raspodela za Attrition_binary u test skupu podataka", 
       x = "Attrition_binary", 
       y = "Broj instanci") 

print(test_plot)

########## Logisticka regresija ##########

model1_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
log_reg_model <- glm(model1_formula, data = train_data, family = binomial)

y_pred_model1 <- predict(log_reg_model, test_data, type = "response")
y_pred_model1_class <- ifelse(y_pred_model1 > 0.5, 1, 0)

confusionMatrix(as.factor(y_pred_model1_class), as.factor(test_data$Attrition_binary))

# Poboljsanje modela


control <- trainControl(method = "cv", number = 10, sampling = "up")

model1_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
logistic_model <- train(model1_formula, data = train_data, method = "glm", family = "binomial", trControl = control)
y_pred_model1 <- predict(logistic_model, test_data)
confusionMatrix(y_pred_model1, as.factor(test_data$Attrition_binary))

#Probali smo i sa YearsInCurrentRolePerAge i dobili iste rezultate

control <- trainControl(method = "cv", number = 10, sampling = "up")

model1_formula <- Attrition_binary ~ OverTime + YearsInCurrentRolePerAge
logistic_model <- train(model1_formula, data = train_data, method = "glm", family = "binomial", trControl = control)
y_pred_model1 <- predict(logistic_model, test_data)
confusionMatrix(y_pred_model1, as.factor(test_data$Attrition_binary))



############ Decision tree ############
model2_formula <- Attrition_binary ~ OverTime + MonthlyIncome + JobInvolvement
decision_tree_model <- train(model2_formula, data = train_data, method = "rpart")
y_pred_model2 <- predict(decision_tree_model, test_data)
confusionMatrix(y_pred_model2, as.factor(test_data$Attrition_binary))


model2_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
decision_tree_model <- train(model2_formula, data = train_data, method = "rpart")
y_pred_model2 <- predict(decision_tree_model, test_data)
confusionMatrix(y_pred_model2, as.factor(test_data$Attrition_binary))

# Poboljsanje modela

control <- trainControl(method = "cv", number = 10, sampling = "up")
model2_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole 
decision_tree_model <- train(model2_formula, data = train_data, method = "rpart", trControl = control)
y_pred_model2 <- predict(decision_tree_model, test_data)
confusionMatrix(y_pred_model2, as.factor(test_data$Attrition_binary))



############ Random Forest ###########

model3_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
random_forest_model <- randomForest(model3_formula, data = train_data)
y_pred_model3 <- predict(random_forest_model, test_data)
confusionMatrix(y_pred_model3, as.factor(test_data$Attrition_binary))

# Poboljsanje modela

control <- trainControl(method = "cv", number = 10, sampling = "up")
model3_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
random_forest_model <- train(model3_formula, data = train_data, method = "rf", trControl = control)
y_pred_model3 <- predict(random_forest_model, test_data)
confusionMatrix(y_pred_model3, as.factor(test_data$Attrition_binary))


############## NN #################

library("nnet")

control <- trainControl(method = "cv", number = 6, sampling = "up") ## 10, 8 

grid <- expand.grid(
  size = c(2, 4, 6),      
  decay = c(0.1, 0.01, 0.001)
)

set.seed(123)
nn_model <- train(
  Attrition_binary ~ .,     
  data = train_data,       
  method = "nnet",         
  trControl = control,
  tuneGrid = grid,         
  trace = FALSE         
)

print(nn_model)

predictions <- predict(nn_model, newdata = test_data)

confusionMatrix(as.factor(predictions), as.factor(test_data$Attrition_binary))

library(DescTools)
library(NeuralNetTools)

plotnet(mod_in = nn_model, # nnet object
        pos_col = "darkgreen", # positive weights are shown in green
        neg_col = "darkred", # negative weights are shown in red
        bias = FALSE, # do not plot bias
        circle_cex = 4, # reduce circle size (default is 5)
        cex_val = 0.6)

#najbolji rezultati za sad, bez over/undersampled podataka

#oversampling train_data

train_data <- hr_data.final[train_index, ]

table(train_data$Attrition_binary)

library("ROSE")

train_data <- ROSE(Attrition_binary ~ ., data = train_data, seed = 43)$data

#provera odnosa nakon oversampling-a
table(train_data$Attrition_binary)

#undersampling train_data

#190 manjinskih, 400 neka bude ukupno
train_data <- ovun.sample(Attrition_binary ~ ., data = train_data, method = "under", N = 380)$data

#provera odnosa nakon undersampling-a
table(train_data$Attrition_binary)

##nisam zadovoljan kako utice na modele over i under

train_data <- ovun.sample(Attrition_binary ~ ., data = train_data, method = "both", p=0.5, N=1289, seed = 1)$data
table(train_data$Attrition_binary)

#0.7007 losije


################################# FE - Dodatno
hr_data.final <- select(hr_data, MonthlyIncome, Attrition_binary, DistanceFromHome, Age, OverTime, StockOptionLevel, WorkLifeBalance, JobInvolvement, Education, YearsInCurrentRole, ExperienceBeforeCurrentRole) #YearsInCurrentRolePerAge)

hr_data.final$LogMonthlyIncome <- log(hr_data.final$MonthlyIncome + 1)
#poboljsalo NN preciznost i senzitivnost, ali je opala spcificnost, kapa skoro identicna

hr_data.final$JobWorkBalanceScore <- hr_data.final$JobInvolvement + hr_data.final$WorkLifeBalance

hr_data.final$IncomePerDistance <- hr_data.final$MonthlyIncome / (hr_data.final$DistanceFromHome + 1)




