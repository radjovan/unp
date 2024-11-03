library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)

#hr_data <- read.csv("C:\\Users\\HP ELITEBOOK 840 G7\\Desktop\\Seminarski UNP\\HR_Analytics.csv", header = TRUE)
#hr_data <- read_csv("C:\\Users\\Nmnj\\Desktop\\UNP projekat\\HR_Analytics.csv")

hr_data <- read_csv("HR_Analytics.csv")

summary(hr_data)

##########################################
#PRIPREMA I ANALIZA

str(hr_data)

unique_values_summary <- hr_data %>%
  summarise(across(everything(), 
                   list(unique_values = ~ length(unique(.)), 
                        sample_values = ~ paste(unique(.)[1:5], collapse = ", "))))

print(unique_values_summary$EmpID_unique_values)

print(hr_data[duplicated(hr_data$EmpID), ]$EmpID)
print(hr_data[duplicated(hr_data), ])

hr_data <- hr_data[!duplicated(hr_data), ]
nrow(hr_data)

summary(hr_data$Age)

summary(hr_data$YearsWithCurrManager)

#YearsWithCurrManager ima NA vrednosti treba ih popuniti jer ih ima samo 57 od ukupnih 1473 podatka

#Popunjavamo sa Mean

#hr_data <- hr_data %>%  mutate(YearsWithCurrManager = if_else(
#  is.na
#  (YearsWithCurrManager), mean(YearsWithCurrManager, na.rm = TRUE), YearsWithCurrManager))

#mozemo preciznije popuniti probacemo na drugaciji nacin

summary(hr_data$YearsWithCurrManager)

#Popunjavanje na osnovu AgeGroup kojoj pripada radnik

hr_data <- hr_data %>%  group_by(AgeGroup) %>%  mutate(YearsWithCurrManager = if_else(is.na(YearsWithCurrManager), mean(YearsWithCurrManager, na.rm = TRUE), YearsWithCurrManager)) %>%  ungroup()

summary(hr_data$YearsWithCurrManager)

print(unique_values_summary$AgeGroup_unique_values)
print(unique_values_summary$AgeGroup_sample_values)

attrition_summary <- hr_data %>%
  group_by(Attrition) %>%
  summarise(Broj = n())

print(attrition_summary)

hr_data <- hr_data %>%
  mutate(BusinessTravel = ifelse(BusinessTravel == "TravelRarely", "Travel_Rarely", BusinessTravel))

hr_data$DistanceFromHomeGroup <- ifelse(hr_data$DistanceFromHome <= 1, "Jako blizu", #mozda spojiti 1 i 2
                                        ifelse(hr_data$DistanceFromHome == 2, "Blizu",
                                               ifelse(hr_data$DistanceFromHome <= 6, "Srednje",
                                                      ifelse(hr_data$DistanceFromHome <= 10, "Daleko", "Jako daleko"))))
#isti podaci za sve moze da se brise
hr_data$Over18 <- NULL
hr_data$EmployeeCount <- NULL
hr_data$EmpID <- NULL
hr_data$EmployeeNumber <- NULL
hr_data$StandardHours <- NULL

hr_data$TotalWorkingYearsGroup <- ifelse(hr_data$TotalWorkingYears <= 1, "0-1",
                                         ifelse(hr_data$TotalWorkingYears <= 5, "2-5",
                                                ifelse(hr_data$TotalWorkingYears <= 10, "5-10",
                                                       ifelse(hr_data$TotalWorkingYears <= 20, "10-20", "20+"))))

hr_data$YearsAtCompanyGroup <- ifelse(hr_data$YearsAtCompany <= 2, "0-2",
                                      ifelse(hr_data$YearsAtCompany <= 5, "2-3",
                                             ifelse(hr_data$YearsAtCompany <= 10, "5-10",
                                                    ifelse(hr_data$YearsAtCompany <= 20, "10-20", "20+"))))

hr_data$AgeGroup = factor(hr_data$AgeGroup)

hr_data$Department = factor(hr_data$Department)

hr_data$JobSatisfaction = factor(hr_data$JobSatisfaction)


hr_data$DistanceFromHomeGroup <- factor(hr_data$DistanceFromHomeGroup)

hr_data$Attrition_binary <- ifelse(hr_data$Attrition == "Yes", 1, 0)

hr_data$NumCompaniesWorked = factor(hr_data$NumCompaniesWorked)

hr_data$JobRole = factor(hr_data$JobRole)

summary(hr_data)

hr_data$EducationField <- NULL

hr_data$DailyRate <- NULL

hr_data$MonthlyRate <- NULL

hr_data$SalarySlab <- NULL

#hr_data$StockOptionLevel <- NULL

################################################################################
#ANALIZA

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
  labs(title = "Attrition prema DistanceFromHomeGroup i JobSatisfcation",
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

# Kreiramo boxplot za različite varijable po JobRole
ggplot(data, aes(x = JobRole, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Job Role Analysis", x = "Job Role", y = "Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################

ggplot(hr_data, aes(x = JobRole, y = Age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Age prema Job Role", x = "Job Role", y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#za bolje pozicije koje smo utvrdili da ljudi skoro ne napustaju potrebne su starije osobe, mladje osobe su na pozicijama koje se pre napustaju

ggplot(hr_data, aes(x = as.factor(JobSatisfaction), y = MonthlyIncome, fill = Attrition)) +
  geom_violin(trim = FALSE, adjust = 1.5) +
  scale_fill_manual(values = c("#397AAD", "#F11111")) +
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
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Udeo Attrition naspram Marital Status i Business Travel kolona",
       x = "Marital Status",
       y = "Udeo",
       fill = "Attrition") +
  scale_fill_manual(values = c("Yes" = "#F11111", "No" = "#397AAD"),
                    labels = c("Yes" = "Attrition", "No" = "No Attrition")) +
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

#ne svidja mi se grafik WorkLifeBalance, MonthlyIncome i Attrition

summary_data <- hr_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE), .groups = 'drop')

ggplot(summary_data, aes(x = WorkLifeBalance, y = factor(Attrition), fill = AverageIncome)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2171b5") +
  labs(title = "Heatmap prosecne mesecne zarade prema WorkLifeBalance i Attrition",
       x = "WorkLifeBalance",
       y = "Attrition",
       fill = "Prosecna mesecna zarada") +
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
  labs(title = "Heatmap AverageYears prema WorkLifeBalance i Attrition",
       x = "Work-Life Balance",
       y = "Attrition",
       fill = "AverageYears") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Vidimo da zaposleni koji su pronasli dobar balans duze ostaju i firme i manje su verovatnoce da ce je napustiti

ggplot(hr_data, aes(x = Attrition_binary, fill = OverTime)) +
  geom_bar(position = "fill") +
  labs(title = "Distribucija OverTime u odnosu na Attrition",
       x = "Attrition",
       y = "Overtime") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

pivot_table <- as.data.frame(table(hr_data$OverTime, hr_data$Attrition))
colnames(pivot_table) <- c("OverTime", "Attrition", "Broj")

#HM
ggplot(pivot_table, aes(x = OverTime, y = Attrition, fill = Broj)) +
  geom_tile() +
  labs(title = "Heatmap",
       x = "OverTime",
       y = "Attrition") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()

#samo OverTime nam nista ne govori, probacemo da napravimo kombinaciju sa kolonom Age

ggplot(hr_data, aes(x = interaction(OverTime, Attrition), y = Age, fill = interaction(OverTime, Attrition))) +
  geom_boxplot() +
  labs(title = "Distribucija starosti po kombinacijama OverTime-a i Attrition-a",
       x = "Kombinacija (OverTime i Attrition)",
       y = "Starost") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14, face = "bold"))

#vidimo da zaposleni koji rade prekovremeno a mladi su, cesce napustaju firmu
#takodje zaposleni koji ne naposutaju firmu a rade prekovremeno su stariji najcesce

# WorkLifeBalance, YearsAtCompany i Attrition

summary_data <- hr_data %>%
  group_by(WorkLifeBalance, Attrition) %>%
  summarise(AverageYears = mean(YearsWithCurrManager, na.rm = TRUE), .groups = 'drop')

ggplot(summary_data, aes(x = WorkLifeBalance, y = factor(Attrition), fill = AverageYears)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2171b5") +
  labs(title = "Heatmap AverageYearsWithCurrManagerr prema Work-Life Balance i Attrition",
       x = "WorkLifeBalance",
       y = "Attrition",
       fill = "AverageYearsWithCurrManagerr") +
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

##########################################
#MODEL

hr_data$OverTime <- ifelse(hr_data$OverTime == "Yes", 1, 0)

hr_data.final <- select(hr_data, MonthlyIncome, Attrition_binary, DistanceFromHome, Age, OverTime, StockOptionLevel, WorkLifeBalance, JobInvolvement, Education, YearsInCurrentRole)

cor_matrix <- cor(hr_data.final, use = "complete.obs")

corrplot(cor_matrix, method = "color", col = colorRampPalette(c("yellow", "green", "blue"))(200), 
         title = "Korelacija", 
         tl.cex = 0.8, 
         number.cex = 0.7,  
         addCoef.col = "black",  
         mar = c(0,0,2,0)) 

hr_data.final$Attrition_binary <- factor(hr_data$Attrition_binary)

# Podela podataka na trening i test skup

set.seed(21)

train_index <- createDataPartition(hr_data.final$Attrition_binary, 
                                   p = 0.6, 
                                   list = FALSE)

train_data <- hr_data.final[train_index, ]
test_data <- hr_data.final[-train_index, ]

##########################################
########## Logisticka regresija ##########
##########################################

model1_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
log_reg_model <- glm(model1_formula, data = train_data, family = binomial)

y_pred_model1 <- predict(log_reg_model, test_data, type = "response")
y_pred_model1_class <- ifelse(y_pred_model1 > 0.5, 1, 0)

confusionMatrix(as.factor(y_pred_model1_class), as.factor(test_data$Attrition_binary))

########## Poboljsanje modela ##########

control <- trainControl(method = "cv", number = 10, sampling = "up")

model1_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
logistic_model <- train(model1_formula, data = train_data, method = "glm", family = "binomial", trControl = control)
y_pred_model1 <- predict(logistic_model, test_data)
confusionMatrix(y_pred_model1, as.factor(test_data$Attrition_binary))

###################################
########## Decision tree ##########
###################################

model2_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
decision_tree_model <- train(model2_formula, data = train_data, method = "rpart")
y_pred_model2 <- predict(decision_tree_model, test_data)
confusionMatrix(y_pred_model2, as.factor(test_data$Attrition_binary))

########## Poboljsanje modela ##########

control <- trainControl(method = "cv", number = 10, sampling = "up")

model2_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole 
decision_tree_model <- train(model2_formula, data = train_data, method = "rpart", trControl = control)
y_pred_model2 <- predict(decision_tree_model, test_data)
confusionMatrix(y_pred_model2, as.factor(test_data$Attrition_binary))

###################################
########## Random Forest ##########
###################################

model3_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
random_forest_model <- randomForest(model3_formula, data = train_data)
y_pred_model3 <- predict(random_forest_model, test_data)
confusionMatrix(y_pred_model3, as.factor(test_data$Attrition_binary))

########## Poboljsanje modela ##########

control <- trainControl(method = "cv", number = 10, sampling = "up")

model3_formula <- Attrition_binary ~ OverTime + Age + YearsInCurrentRole
random_forest_model <- train(model3_formula, data = train_data, method = "rf", trControl = control)
y_pred_model3 <- predict(random_forest_model, test_data)
confusionMatrix(y_pred_model3, as.factor(test_data$Attrition_binary))
