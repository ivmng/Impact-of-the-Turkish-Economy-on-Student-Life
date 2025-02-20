################DATA STRUCTURE

library(readxl)
surveyz <- read_excel("Surveyz.xlsx")
View(surveyz)

surveyz$`1. Your gender. (Cinsiyetiniz.)` <- ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)` == "Male ( Erkek)", 1, 
                               ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)` == "Female ( Kadýn)", 2,
                                      ifelse(surveyz$`1. Your gender. (Cinsiyetiniz.)`=="Prefer not to say ( Belirtmek Ýstemiyorum.)",3,NA)))

surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)` <- ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Preparatory student. (Hazýrlýk öðrencisi)",0,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Freshman (1.sýnýf)",1,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Sophomore (2.sýnýf)",2,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Junior (3.sýnýf)",3,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Senior (4.sýnýf)",4,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Graduate Student (Mezun)",5,
                                                                            ifelse(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`=="Other (Diðer)",6,NA)))))))
library(dplyr)

surveyz <- surveyz %>%
  mutate(`3. What is your cGPA? (Ortalamanýz kaç)` = case_when(
    `3. What is your cGPA? (Ortalamanýz kaç)` == "0-0.99" ~ runif(304, min = 0, max = 0.99),
    `3. What is your cGPA? (Ortalamanýz kaç)` == "1-1.99" ~ runif(304, min = 1, max = 1.99),
    `3. What is your cGPA? (Ortalamanýz kaç)` == "2-2.99" ~ runif(304, min = 2, max = 2.99),
    `3. What is your cGPA? (Ortalamanýz kaç)` == "3-4" ~ runif(304, min = 3, max = 4),
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` = case_when(
    `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` == "0-8000 TL" ~ runif(304, min = 0, max = 8000),
    `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` == "8001-17000 TL" ~ runif(304, min = 8001, max = 17000),
    `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` == "17001+ TL" ~ runif(304, min = 17001, max = 30000),
    TRUE ~ NA_real_
  ))

surveyz$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`<- ifelse(surveyz$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`=="Hayýr",0,
                                                                                                                      ifelse(surveyz$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`=="Evet",1,NA))

surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`<- ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`=="Never (Hiçbir zaman)",1,
                                                                                                                                                                            ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`=="Occasionally (Ara sýra)",2,
                                                                                                                                                                                   ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`=="Often (Sýk sýk)",3,
                                                                                                                                                                                   ifelse(surveyz$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`=="Always ( Her zaman)",4,NA))))

surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`<- ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`=="No idea.  Fikrim yok)",1,
                                                                                                                                                                 ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`=="No (Hayýr)",0,
                                                                                                                                                                        ifelse(surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`=="Yes (Evet)",2,NA)))

surveyz$`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)` <- ifelse(surveyz$`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)`=="No (Hayýr)",0,
                                                                                                               ifelse(surveyz$`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)`=="Part-time (Yarý Zamanlý iþ)",1,
                                                                                                                      ifelse(surveyz$`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)`=="Yes (Evet)",2,NA)))

surveyz <- surveyz %>%
  mutate(`13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)` = case_when(
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)` == "No (Hayýr)" ~ 0,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)` == "No idea (Fikrim Yok)" ~ 1,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)` == "Yes, negatively (Evet, olumsuz)" ~ 2,
    `13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)` == "Yes, positively (Evet, olumlu)"~3,
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)` = case_when(
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)` == "No (Hayýr)" ~ 0,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)` == "No idea (Fikrim Yok)" ~ 1,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)` == "Yes, negatively (Evet, olumsuz)" ~ 2,
    `14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)` == "Yes, positively (Evet, olumlu)"~3,
    TRUE ~ NA_real_
  ))

surveyz <- surveyz %>%
  mutate(`17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)` = case_when(
    `17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)` == "%0-24.99" ~ runif(304, min = 0, max = 24.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)` == "%25-49.99" ~ runif(304, min = 25, max = 49.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)` == "%50-74.99" ~ runif(304, min = 50, max = 74.99),
    `17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)` == "%75-100" ~ runif(304, min = 75, max = 100),
    TRUE ~ NA_real_
  ))

#surveyz$`6. How many languages ??????do you know that are suitable for daily use? (Günlük kullanýma uygun kaç dil biliyorsunuz?)` <- ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (Günlük kullanýma uygun kaç dil biliyorsunuz?)`== "1",1,
                                                                                                                                        #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (Günlük kullanýma uygun kaç dil biliyorsunuz?)`=="2",2,
                                                                                                                                               #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (Günlük kullanýma uygun kaç dil biliyorsunuz?)`=="3",3,
                                                                                                                                                      #ifelse(surveyz$`6. How many languages ??????do you know that are suitable for daily use? (Günlük kullanýma uygun kaç dil biliyorsunuz?)`=="4+",4,NA))))
########DATA ANALYSIS

library(ggplot2)
library(dplyr)

#cgpa vs fee school
aovcgpa_fee <- aov(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`~surveyz$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`,data = surveyz)
summary(aovcgpa_fee)


ggplot(surveyz,aes(x=as.factor(surveyz$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`),y=surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`))+
  geom_boxplot(fill="darkgreen")+
  labs(title = "GPA by Fee School",x="Have you studied in fee-based schools before? [0:NO | 1:YES]",y="GPA")

aovcgpa_incm <- aov(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`~surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`,data = surveyz)
summary(aovcgpa_incm)

modelofgpa_incm <- lm(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`~surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)
modelofgpa_incm
summary(modelofgpa_incm)

####wilcox test cgpa and fee based school realtionship
wilcox.test(survey$`3. What is your cGPA? (Ortalamanýz kaç)` ~ survey$`5. Have you studied in fee-based schools before? (Daha önce ücretli okullarda eðitim gördünüz mü?)`)
#
lm12 <- lm(data$`3. What is your cGPA? (Ortalamanýz kaç)`~data$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)
summary(lm12)

#income vs cgpa vs financial concern
ggplot(surveyz, aes(x = `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`, y = `3. What is your cGPA? (Ortalamanýz kaç)`)) +
  geom_point(aes(color = as.factor(`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`)), alpha = 0.5) +
  labs(title = "GPA vs Income Colored by Financial Concerns", x = "Income (TL)", y = "GPA", color = "Financial Concerns") +
  scale_color_manual(values = c("green", "blue", "red"))+
  geom_smooth(method = "lm", color = "red")

lmcgpa_inc <- lm(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`~surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)
summary(lmcgpa_inc)
maov1 <- aov(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`~surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`+surveyz$`8. Have you considered changing your department due to financial reasons? (Ekonomik nedenler yüzünden bölümünüzü deðiþtirmeyi düþündünüz mü?)`)
summary(maov1)

survey <- na.omit(surveyz)

table(surveyz$`1. Your gender. (Cinsiyetiniz.)`)
##some frequencies tables

barplot(table(surveyz$`1. Your gender. (Cinsiyetiniz.)`), col = "skyblue", main = "Gender Distribution", xlab = "Gender", ylab = "Frequency")
barplot(table(surveyz$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`), col = "lightgreen", main = "Class Level Distribution", xlab = "Class Level", ylab = "Frequency")
income_aov <- aov(survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` ~ survey$`1. Your gender. (Cinsiyetiniz.)`)
summary(income_aov)


# Boxplot
boxplot(survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` ~ survey$`1. Your gender. (Cinsiyetiniz.)`,
        col = "pink", main = "Monthly Income by Gender", xlab = "Gender", ylab = "Monthly Income")
# Linear Regression Model of income and cgpa relationship
income_lm <- lm(data$`3. What is your cGPA? (Ortalamanýz kaç)`~data$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`+data$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` , data = survey)
summary(income_lm)

maov2 <- aov(data$`3. What is your cGPA? (Ortalamanýz kaç)`~data$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`+data$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` , data = survey)
summary(maov2)
#futureconcern and gpa 

fgpa <- lm(data$`3. What is your cGPA? (Ortalamanýz kaç)`~data$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)`)
summary(fgpa)
#
plot(survey$`3. What is your cGPA? (Ortalamanýz kaç)`, survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`,
     main = "GPA vs Monthly Income", xlab = "cGPA", ylab = "Monthly Income", col = "blue", pch = 16)
abline(lm(`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` ~ `3. What is your cGPA? (Ortalamanýz kaç)`, data = survey), col = "red")


# Cross-tabulation (CLASS LEVEL AND FINANCIAL DIFFICULTIES)
class_difficulties <- table(survey$`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`, 
                            survey$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`)
class_difficulties

# Visualize with Mosaic Plot
mosaicplot(class_difficulties, main = "Class Level vs Financial Difficulties", 
           col = c("lightblue", "lightgreen", "orange", "pink"), las = 1)

#### GPA and monthly income by gender
manova_results <- manova(cbind(`3. What is your cGPA? (Ortalamanýz kaç)`, 
                               `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`) ~ 
                           `1. Your gender. (Cinsiyetiniz.)`, data = survey)
summary(manova_results)
#####income vs academic resources
income_access_ct <- table(survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`, 
                          survey$`7. How often are you unable to access academic resources because of their cost? (Maliyetleri nedeniyle akademik kaynaklara ne sýklýkla eriþemiyorsunuz?)`)
income_access_ct

# Chi-Square Test
chisq_income_access <- chisq.test(income_access_ct)
chisq_income_access
# income vs anxiety about future income

# Linear Regression
lm_model <- lm(survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)` ~ 
                 survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)
summary(lm_model)
# ANOVA mobthly income vs future ýncome concern
income_future_concern_aov <- aov(survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` ~ 
                                   survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)`)
summary(income_future_concern_aov)
boxplot(survey$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` ~ 
          survey$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)`, 
        main = "Monthly Income vs. Future Income Concern", xlab = "Future Income Concern", ylab = "Monthly Income", 
        col = c("lightblue", "pink", "orange", "green"))

#cgpa vs income vs family suppport vs eco infl on students
mv1 <- manova(cbind(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`,surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)~surveyz$`15. Scale the advantage financial support from family to the student during the process of going to university. (Üniversiteye giden süreçte aileden gelen maddi sürecin öðrenciye avantajýný ölçeklendirin.)`+surveyz$`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)`,data=surveyz)
summary(mv1)

#
library(dplyr)
install.packages("ggdist")
library(ggdist)

# surveyz verisi ile çalýþtýðýmýzý varsayýyoruz
data <- surveyz %>%
  mutate(
    Income = as.numeric(`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`),
    GPA = as.numeric(`3. What is your cGPA? (Ortalamanýz kaç)`),
    FutureConcerns = as.factor(`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)`)
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
aov2 <- aov(`3. What is your cGPA? (Ortalamanýz kaç)`~`13. From your perspective, has the economic fluctuation affected the performance of faculty members? ( Sizce ekonomideki dalgalanma öðretim üyelerinin performansýný etkiledi mi?)`*`14. From your perspective, has the economic fluctation affected the performance of students ? (Sizce ekonomideki dalgalanma öðrencilerin performansýný etkiledi mi ?)`,data=surveyz)
summary(aov2)

# 
library(cluster)
library(factoextra)
library(dplyr)

#clustering analysis
# Kütüphaneleri aktif etme
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

data <- surveyz %>%
  mutate(
    Income = as.numeric(`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`),
    GPA = as.numeric(`3. What is your cGPA? (Ortalamanýz kaç)`),
    FutureConcerns = as.factor(surveyz$`9. The main reason for future anxiety is economic uncertainty. (Gelecek kaygýsýnýn temel nedeni ekonomik belirsizliktir.)`)
  ) %>%
  na.omit()

# 2. Kümeleme Analizi (Clustering) of future anxiety
# Ýlgili numerik deðiþkenleri seçme
clustering_data <- data %>%
  select(Income, GPA) %>%
  scale()  

# Optimal küme sayýsýný belirleme
fviz_nbclust(clustering_data, kmeans, method = "wss")

# K-Means kümeleme (örnek: 5 küme)
kmeans_result <- kmeans(clustering_data, centers = 5, nstart = 25)

# Kümeleme sonuçlarýný görselleþtirme
fviz_cluster(kmeans_result, data = clustering_data, geom = "point", ellipse.type = "convex") +
  labs(title = "K-Means Clustering of Survey Data", x = "Income", y = "GPA")

#hotelling
library(Hotelling)
# Convert relevant columns to numeric and remove missing values
surveyz$`1. Your gender. (Cinsiyetiniz.)` <- as.numeric(surveyz$`1. Your gender. (Cinsiyetiniz.)`)
surveyz$`3. What is your cGPA? (Ortalamanýz kaç)` <- as.numeric(surveyz$`3. What is your cGPA? (Ortalamanýz kaç)`)
surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)` <- as.numeric(surveyz$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)


# Subset data for each group (Gender 1 and Gender 2)
group1 <- subset(surveyz,
                 `1. Your gender. (Cinsiyetiniz.)` == 1,
                 select = c(`3. What is your cGPA? (Ortalamanýz kaç)`,
                            `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`))
group2 <- subset(surveyz,
                 `1. Your gender. (Cinsiyetiniz.)` == 2,
                 select = c(`3. What is your cGPA? (Ortalamanýz kaç)`,
                            `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`))

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

# PCA Analizi için yalnýzca sayýsal sütunlarý seçiyoruz
pca_data <- data[, sapply(data, is.numeric)]  # Sayýsal sütunlar

# PCA için veri ölçeklendirme
pca_data_scaled <- scale(pca_data)

# PCA Modeli oluþturma
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# PCA Sonuçlarýný özetleme
summary(pca_result)

# Eigenvalues (Özdeðerler) analizi
fviz_eig(pca_result)

# PCA bileþenlerini görselleþtirme
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
  labs(title = "PCA Variable Contributions", x = "PC1", y = "PC2")

# PCA bireylerin grafiði
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, pointsize = 2, fill.ind = "#00AFBB", repel = TRUE) +
  labs(title = "PCA Individuals Plot", x = "PC1", y = "PC2")

# Biplot (Bireyler ve Deðiþkenler)
fviz_pca_biplot(pca_result, repel = TRUE, geom.ind = "point", pointsize = 2, fill.ind = "#00AFBB", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(title = "PCA Biplot", x = "PC1", y = "PC2")

library(readxl)
library(ggplot2)
# Generate visualizations using the transformed data
# Bubble chart: GPA vs Income by Gender and Class Level
bubble_chart <- ggplot(surveyz, aes(x = `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`,
                                    y = `3. What is your cGPA? (Ortalamanýz kaç)`,
                                    size = `17. Approximately what percantage of your money do you spend on your education ? (Yaklaþýk olarak paranýzýn yüzde kaçýný eðitime harcýyorsunuz?)`,
                                    color = factor(`1. Your gender. (Cinsiyetiniz.)`, levels = c(1, 2, 3), 
                                                   labels = c("Male", "Female", "Prefer not to say")))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ `2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)` ,
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
stacked_bar_chart <- ggplot(surveyz, aes(x = factor(`2. Your current class level is (  Kaçýncý sýnýfsýnýz ?)`),
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
scatter_plot <- ggplot(surveyz, aes(x = `4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`,
                                    y = `3. What is your cGPA? (Ortalamanýz kaç)`,
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
  group_by(`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)`) %>%
  summarise(Average_Income = mean(`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`, na.rm = TRUE)) %>%
  mutate(Job_Type = factor(`12. Do you have a job other than being a student? ( Öðrencilik dýþýnda bir iþiniz var mý?)`,
                           levels = c(0, 1, 2),
                           labels = c("No Job", "Part-time", "Full-time")))

bar_chart_plot <- ggplot(bar_chart_data, aes(x = Job_Type, y = Average_Income, fill = Job_Type)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Bar Chart: Average Income by Job Type",
       x = "Job Type",
       y = "Average Monthly Income") +
  theme_minimal()

as1 <- aov(data$`10. No matter how much I improve myself, I don't think I will have a good income in the future. (Kendimi ne kadar geliþtirirsem geliþtireyim ileride iyi bir gelire sahip olacaðýmý düþünmüyorum.)`~ data$`3. What is your cGPA? (Ortalamanýz kaç)`+data$`4. What is your average monthly income? (Aylýk ortalama geliriniz nedir?)`)
summary(as1)

as2 <- aov(data$`3. What is your cGPA? (Ortalamanýz kaç)`~data$`15. Scale the advantage financial support from family to the student during the process of going to university. (Üniversiteye giden süreçte aileden gelen maddi sürecin öðrenciye avantajýný ölçeklendirin.)`+data$`11. If I am going to experience a brain drain in the future, one of the most important reasons for this is the economy. (Gelecekte beyin göçü yapacaksam bunun en önemli nedenlerinden biri ekonomidir.)`)
summary(as2)
