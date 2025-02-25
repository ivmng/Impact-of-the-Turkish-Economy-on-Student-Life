################DATA STRUCTURE

library(readxl)
surveyz <- read_excel("Surveyz.xlsx")
View(surveyz)

surveyz$`1. Your gender. (Cinsiyetiniz.)` <- ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)` == "Male ( Erkek)", 1, 
                               ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)` == "Female ( Kad�n)", 2,
                                      ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)`=="Prefer not to say ( Belirtmek �stemiyorum.)",3,NA)))

surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)` <- ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Preparatory student. (Haz�rl�k ��rencisi)",0,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Freshman (1.s�n�f)",1,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Sophomore (2.s�n�f)",2,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Junior (3.s�n�f)",3,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Senior (4.s�n�f)",4,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Graduate Student (Mezun)",5,
                                                                            ifelse(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`=="Other (Di�er)",6,NA)))))))
library(dplyr)

surveyz <- surveyz %>%
  mutate(`3. What is your cGPA? (Ortalaman�z ka�)` = case_when(
    `3. What is your cGPA? (Ortalaman�z ka�)` == "0-0.99" ~ runif(304, min = 0, max = 0.99),
    `3. What is your cGPA? (Ortalaman�z ka�)` == "1-1.99" ~ runif(304, min = 1, max = 1.99),
    `3. What is your cGPA? (Ortalaman�z ka�)` == "2-2.99" ~ runif(304, min = 2, max = 2.99),
    `3. What is your cGPA? (Ortalaman�z ka�)` == "3-4" ~ runif(304, min = 3, max = 4),
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` = case_when(
    `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` == "0-8000 TL" ~ runif(304, min = 0, max = 8000),
    `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` == "8001-17000 TL" ~ runif(304, min = 8001, max = 17000),
    `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` == "17001+ TL" ~ runif(304, min = 17001, max = 30000),
    TRUE ~ NA_real_
  ))

surveyz$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`<- ifelse(surveyz$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`=="Hay�r",0,
                                                                                                                      ifelse(surveyz$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`=="Evet",1,NA))

surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`<- ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`=="Never (Hi�bir zaman)",1,
                                                                                                                                                                            ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`=="Occasionally (Ara s�ra)",2,
                                                                                                                                                                                   ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`=="Often (S�k s�k)",3,
                                                                                                                                                                                   ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`=="Always ( Her zaman)",4,NA))))

surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`<- ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`=="No idea.  Fikrim yok)",1,
                                                                                                                                                                 ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`=="No (Hay�r)",0,
                                                                                                                                                                        ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`=="Yes (Evet)",2,NA)))

surveyz$`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)` <- ifelse(surveyz$`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)`=="No (Hay�r)",0,
                                                                                                               ifelse(surveyz$`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)`=="Part-time (Yar� Zamanl� i�)",1,
                                                                                                                      ifelse(surveyz$`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)`=="Yes (Evet)",2,NA)))

surveyz <- surveyz %>%
  mutate(`13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)` = case_when(
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)` == "No (Hay�r)" ~ 0,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)` == "No idea (Fikrim Yok)" ~ 1,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)` == "Yes, negatively (Evet, olumsuz)" ~ 2,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)` == "Yes, positively (Evet, olumlu)"~3,
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)` = case_when(
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)` == "No (Hay�r)" ~ 0,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)` == "No idea (Fikrim Yok)" ~ 1,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)` == "Yes, negatively (Evet, olumsuz)" ~ 2,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)` == "Yes, positively (Evet, olumlu)"~3,
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)` = case_when(
    `17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)` == "%0-24.99" ~ runif(304, min = 0, max = 24.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)` == "%25-49.99" ~ runif(304, min = 25, max = 49.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)` == "%50-74.99" ~ runif(304, min = 50, max = 74.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)` == "%75-100" ~ runif(304, min = 75, max = 100),
    TRUE ~ NA_real_
  ))

#surveyz$`6. How many languages ??????do you know that are suitable for daily use? (G�nl�k kullan�ma uygun ka� dil biliyorsunuz?)` <- ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (G�nl�k kullan�ma uygun ka� dil biliyorsunuz?)`== "1",1,
                                                                                                                                        #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (G�nl�k kullan�ma uygun ka� dil biliyorsunuz?)`=="2",2,
                                                                                                                                               #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (G�nl�k kullan�ma uygun ka� dil biliyorsunuz?)`=="3",3,
                                                                                                                                                      #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (G�nl�k kullan�ma uygun ka� dil biliyorsunuz?)`=="4+",4,NA))))
########DATA ANALYSIS

library(ggplot2)
library(dplyr)

#cgpa vs fee school
aovcgpa_fee <- aov(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`~surveyz$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`,data = surveyz)
summary(aovcgpa_fee)


ggplot(surveyz,aes(x=as.factor(surveyz$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`),y=surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`))+
  geom_boxplot(fill="darkgreen")+
  labs(title = "GPA by Fee School",x="Have you studied in fee-based schools before? [0:NO | 1:YES]",y="GPA")

aovcgpa_incm <- aov(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`~surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`,data = surveyz)
summary(aovcgpa_incm)

modelofgpa_incm <- lm(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`~surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)
modelofgpa_incm
summary(modelofgpa_incm)

####wilcox test cgpa and fee based school realtionship
wilcox.test(survey$`3. What is your cGPA? (Ortalaman�z ka�)` ~ survey$`5. Have you studied in fee-based schools before? (Daha �nce �cretli okullarda e�itim g�rd�n�z m�?)`)
#
lm12 <- lm(data$`3. What is your cGPA? (Ortalaman�z ka�)`~data$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)
summary(lm12)

#income vs cgpa vs financial concern
ggplot(surveyz, aes(x = `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`, y = `3. What is your cGPA? (Ortalaman�z ka�)`)) +
  geom_point(aes(color = as.factor(`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`)), alpha = 0.5) +
  labs(title = "GPA vs Income Colored by Financial Concerns", x = "Income (TL)", y = "GPA", color = "Financial Concerns") +
  scale_color_manual(values = c("green", "blue", "red"))+
  geom_smooth(method = "lm", color = "red")

lmcgpa_inc <- lm(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`~surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)
summary(lmcgpa_inc)
maov1 <- aov(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`~surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`+surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler y�z�nden b�l�m�n�z� de�i�tirmeyi d���nd�n�z m�?)`)
summary(maov1)

survey <- na.omit(surveyz)

table(surveyz$`1. Your gender. (Cinsiyetiniz.)`)
##some frequencies tables

barplot(table(surveyz$`1. Your gender. (Cinsiyetiniz.)`), col = "skyblue", main = "Gender Distribution", xlab = "Gender", ylab = "Frequency")
barplot(table(surveyz$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`), col = "lightgreen", main = "Class Level Distribution", xlab = "Class Level", ylab = "Frequency")
income_aov <- aov(survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` ~ survey$`1. Your gender. (Cinsiyetiniz.)`)
summary(income_aov)


# Boxplot
boxplot(survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` ~ survey$`1. Your gender. (Cinsiyetiniz.)`,
        col = "pink", main = "Monthly Income by Gender", xlab = "Gender", ylab = "Monthly Income")
# Linear Regression Model of income and cgpa relationship
income_lm <- lm(data$`3. What is your cGPA? (Ortalaman�z ka�)`~data$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`+data$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` , data = survey)
summary(income_lm)

maov2 <- aov(data$`3. What is your cGPA? (Ortalaman�z ka�)`~data$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`+data$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` , data = survey)
summary(maov2)
#futureconcern and gpa 

fgpa <- lm(data$`3. What is your cGPA? (Ortalaman�z ka�)`~data$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)`)
summary(fgpa)
#
plot(survey$`3. What is your cGPA? (Ortalaman�z ka�)`, survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`,
     main = "GPA vs Monthly Income", xlab = "cGPA", ylab = "Monthly Income", col = "blue", pch = 16)
abline(lm(`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` ~ `3. What is your cGPA? (Ortalaman�z ka�)`, data = survey), col = "red")


# Cross-tabulation (CLASS LEVEL AND FINANCIAL DIFFICULTIES)
class_difficulties <- table(survey$`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`, 
                            survey$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`)
class_difficulties

# Visualize with Mosaic Plot
mosaicplot(class_difficulties, main = "Class Level vs Financial Difficulties", 
           col = c("lightblue", "lightgreen", "orange", "pink"), las = 1)

#### GPA and monthly income by gender
manova_results <- manova(cbind(`3. What is your cGPA? (Ortalaman�z ka�)`, 
                               `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`) ~ 
                           `1. Your gender. (Cinsiyetiniz.)`, data = survey)
summary(manova_results)
#####income vs academic resources
income_access_ct <- table(survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`, 
                          survey$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne s�kl�kla eri�emiyorsunuz?)`)
income_access_ct

# Chi-Square Test
chisq_income_access <- chisq.test(income_access_ct)
chisq_income_access
# income vs anxiety about future income

# Linear Regression
lm_model <- lm(survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)` ~ 
                 survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)
summary(lm_model)
# ANOVA mobthly income vs future �ncome concern
income_future_concern_aov <- aov(survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` ~ 
                                   survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)`)
summary(income_future_concern_aov)
boxplot(survey$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` ~ 
          survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)`, 
        main = "Monthly Income vs. Future Income Concern", xlab = "Future Income Concern", ylab = "Monthly Income", 
        col = c("lightblue", "pink", "orange", "green"))

#cgpa vs income vs family suppport vs eco infl on students
mv1 <- manova(cbind(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`,surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)~surveyz$`15. Scale the advantage financial support from family to the student during the process of going to university. (�niversiteye giden s�re�te aileden gelen maddi s�recin ��renciye avantaj�n� �l�eklendirin.)`+surveyz$`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)`,data=surveyz)
summary(mv1)

#
library(dplyr)
install.packages("ggdist")
library(ggdist)

# surveyz verisi ile �al��t���m�z� varsay�yoruz
data <- surveyz %>%
  mutate(
    Income = as.numeric(`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`),
    GPA = as.numeric(`3. What is your cGPA? (Ortalaman�z ka�)`),
    FutureConcerns = as.factor(`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)`)
  ) %>%
  na.omit()

# Raincloud plot: GPA by Future Concerns
ggplot(data, aes(x = FutureConcerns, y = GPA, fill = FutureConcerns)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    aes(color = FutureConcerns),
    width = 0.1,
    alpha = 0.5
  ) +
  labs(
    title = "Raincloud Plot of GPA by Future Concerns",
    x = "Future Concerns",
    y = "GPA",
    fill = "Future Concerns",
    color = "Future Concerns"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("green", "blue", "red", "yellow", "pink")) +
  scale_color_manual(values = c("green", "blue", "red", "yellow", "pink"))

#gpa vs student teacher productivity
aov2 <- aov(`3. What is your cGPA? (Ortalaman�z ka�)`~`13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma ��retim �yelerinin performans�n� etkiledi mi?)`*`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma ��rencilerin performans�n� etkiledi mi ?)`,data=surveyz)
summary(aov2)

# 
library(cluster)
library(factoextra)
library(dplyr)

#clustering analysis
# K�t�phaneleri aktif etme
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

data <- surveyz %>%
  mutate(
    Income = as.numeric(`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`),
    GPA = as.numeric(`3. What is your cGPA? (Ortalaman�z ka�)`),
    FutureConcerns = as.factor(surveyz$`9. The main reason for future anxiety is economic uncertainty. (Gelecek kayg�s�n�n temel nedeni ekonomik belirsizliktir.)`)
  ) %>%
  na.omit()

# 2. K�meleme Analizi (Clustering) of future anxiety
# �lgili numerik de�i�kenleri se�me
clustering_data <- data %>%
  select(Income, GPA) %>%
  scale()  

# Optimal k�me say�s�n� belirleme
fviz_nbclust(clustering_data, kmeans, method = "wss")

# K-Means k�meleme (�rnek: 5 k�me)
kmeans_result <- kmeans(clustering_data, centers = 5, nstart = 25)

# K�meleme sonu�lar�n� g�rselle�tirme
fviz_cluster(kmeans_result, data = clustering_data, geom = "point", ellipse.type = "convex") +
  labs(title = "K-Means Clustering of Survey Data", x = "Income", y = "GPA")

#hotelling
library(Hotelling)
# Convert relevant columns to numeric and remove missing values
surveyz$`1. Your gender. (Cinsiyetiniz.)` <- as.numeric(surveyz$`1. Your gender. (Cinsiyetiniz.)`)
surveyz$`3. What is your cGPA? (Ortalaman�z ka�)` <- as.numeric(surveyz$`3. What is your cGPA? (Ortalaman�z ka�)`)
surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)` <- as.numeric(surveyz$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)


# Subset data for each group (Gender 1 and Gender 2)
group1 <- subset(surveyz,
                 `1. Your gender. (Cinsiyetiniz.)` == 1,
                 select = c(`3. What is your cGPA? (Ortalaman�z ka�)`,
                            `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`))
group2 <- subset(surveyz,
                 `1. Your gender. (Cinsiyetiniz.)` == 2,
                 select = c(`3. What is your cGPA? (Ortalaman�z ka�)`,
                            `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`))

# Convert to matrices
group1_matrix <- as.matrix(group1)
group2_matrix <- as.matrix(group2)

# Check for singularity (remove variables with zero variance if needed)
group1_matrix <- group1_matrix[, apply(group1_matrix, 2, var) > 0]
group2_matrix <- group2_matrix[, apply(group2_matrix, 2, var) > 0]

# Run Hotelling's T-squared test
if (ncol(group1_matrix) == ncol(group2_matrix) && ncol(group1_matrix) > 0) {
  hotelling_result <- hotelling.test(group1_matrix, group2_matrix)
  print(hotelling_result)
} else {
  cat("Unable to run Hotelling's T-squared test: insufficient variation or incompatible group dimensions.")
}


#pca

# PCA Analizi i�in yaln�zca say�sal s�tunlar� se�iyoruz
pca_data <- data[, sapply(data, is.numeric)]  # Say�sal s�tunlar

# PCA i�in veri �l�eklendirme
pca_data_scaled <- scale(pca_data)

# PCA Modeli olu�turma
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# PCA Sonu�lar�n� �zetleme
summary(pca_result)

# Eigenvalues (�zde�erler) analizi
fviz_eig(pca_result)

# PCA bile�enlerini g�rselle�tirme
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
  labs(title = "PCA Variable Contributions", x = "PC1", y = "PC2")

# PCA bireylerin grafi�i
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = "#00AFBB", repel = TRUE) +
  labs(title = "PCA Individuals Plot", x = "PC1", y = "PC2")

# Biplot (Bireyler ve De�i�kenler)
fviz_pca_biplot(pca_result, repel = TRUE, geom.ind = "point", pointsize = 2, fill.ind = "#00AFBB", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(title = "PCA Biplot", x = "PC1", y = "PC2")

library(readxl)
library(ggplot2)
# Generate visualizations using the transformed data
# Bubble chart: GPA vs Income by Gender and Class Level
bubble_chart <- ggplot(surveyz, aes(x = `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`,
                                    y = `3. What is your cGPA? (Ortalaman�z ka�)`,
                                    size = `17. Approximately what percantage of your money do you spend on your education ? (Yakla��k olarak paran�z�n y�zde ka��n� e�itime harc�yorsunuz?)`,
                                    color = factor(`1. Your gender. (Cinsiyetiniz.)`, levels = c(1, 2, 3), 
                                                   labels = c("Male", "Female", "Prefer not to say")))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ `2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)` ,
             labeller = as_labeller(c(
               `0` = "Prep",
               `1` = "Freshman",
               `2` = "Sophomore",
               `3` = "Junior",
               `4` = "Senior",
               `5` = "Graduate",
               `6` = "Other"
             ))) +
  scale_size_continuous(range = c(2, 10)) +
  theme_minimal() +
  labs(title = "Bubble Chart: GPA vs Income by Gender and Class Level",
       x = "Monthly Income",
       y = "GPA",
       size = "% Money on Education",
       color = "Gender") +
  scale_color_brewer(palette = "Set2")
bubble_chart
#
# Generate new visualization
# Stacked Bar Chart: Gender Distribution by Class Level
stacked_bar_chart <- ggplot(surveyz, aes(x = factor(`2. Your current class level is (  Ka��nc� s�n�fs�n�z ?)`),
                                         fill = factor(`1. Your gender. (Cinsiyetiniz.)`,
                                                       levels = c(1, 2, 3),
                                                       labels = c("Male", "Female", "Prefer not to say")))) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gender Distribution by Class Level",
       x = "Class Level",
       y = "Proportion",
       fill = "Gender") +
  theme_minimal()
stacked_bar_chart
#
# Scatter Plot: Income vs. GPA Colored by Gender
scatter_plot <- ggplot(surveyz, aes(x = `4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`,
                                    y = `3. What is your cGPA? (Ortalaman�z ka�)`,
                                    color = factor(`1. Your gender. (Cinsiyetiniz.)`,
                                                   levels = c(1, 2, 3),
                                                   labels = c("Male", "Female", "Prefer not to say")))) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Scatter Plot: Income vs. GPA by Gender",
       x = "Average Monthly Income",
       y = "GPA",
       color = "Gender") +
  theme_minimal()
scatter_plot
# Bar Chart: Average Income by Job Type
bar_chart_data <- surveyz %>%
  group_by(`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)`) %>%
  summarise(Average_Income = mean(`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`, na.rm = TRUE)) %>%
  mutate(Job_Type = factor(`12. Do you have a job other than being a student? ( ��rencilik d���nda bir i�iniz var m�?)`,
                           levels = c(0, 1, 2),
                           labels = c("No Job", "Part-time", "Full-time")))

bar_chart_plot <- ggplot(bar_chart_data, aes(x = Job_Type, y = Average_Income, fill = Job_Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Bar Chart: Average Income by Job Type",
       x = "Job Type",
       y = "Average Monthly Income") +
  theme_minimal()

as1 <- aov(data$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geli�tirirsem geli�tireyim ileride iyi bir gelire sahip olaca��m� d���nm�yorum.)`~ data$`3. What is your cGPA? (Ortalaman�z ka�)`+data$`4. What is your average monthly income? (Ayl�k ortalama geliriniz nedir?)`)
summary(as1)

as2 <- aov(data$`3. What is your cGPA? (Ortalaman�z ka�)`~data$`15. Scale the advantage financial support from family to the student during the process of going to university. (�niversiteye giden s�re�te aileden gelen maddi s�recin ��renciye avantaj�n� �l�eklendirin.)`+data$`11. If I am going to experience a brain drain in the future, one of the most important reasons for this is the economy. (Gelecekte beyin g��� yapacaksam bunun en �nemli nedenlerinden biri ekonomidir.)`)
summary(as2)
