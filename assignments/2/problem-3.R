library(MASS)
library(hash)
library(dplyr)
library(ggplot2)
library(oddsratio)
library(fastDummies)

# Problem 3.1
graph_plot <- function (df)
{
  tmp_df = filter(df, Survived==1)
  num_survivors = nrow(tmp_df)
  # Survival based on sex
  num_women_survived = nrow(filter(tmp_df, Sex=="female"))
  num_men_survived = nrow(filter(tmp_df, Sex=="male"))
  
  total_women = nrow(filter(df, Sex=="female")) 
  total_men = nrow(filter(df, Sex=="male")) 
  
  jpeg("3a_survivors_based_on_sex.jpg")
  barplot(c(round(100.0*num_women_survived/total_women, 2), 
            round(100.0*num_men_survived/total_men, 2)), 
          names.arg = c("Women", "Men"), 
          ylab="Percentage survived", ylim=c(0, 100))
  
  # Survival based on passenger class
  num_first_class_survived = nrow(filter(tmp_df, PClass=="1st"))
  num_second_class_survived = nrow(filter(tmp_df, PClass=="2nd"))
  num_third_class_survived = nrow(filter(tmp_df, PClass=="3rd"))
  
  num_first_class = nrow(filter(df, PClass=="1st"))
  num_second_class = nrow(filter(df, PClass=="2nd"))
  num_third_class = nrow(filter(df, PClass=="3rd"))
  
  jpeg("3a_survivors_based_on_class.jpg")
  barplot(c(round(100.0*num_first_class_survived/num_first_class, 2), 
            round(100.0*num_second_class_survived/num_second_class, 2), 
            round(100.0*num_third_class_survived/num_third_class, 2)), 
          names.arg = c("First", "Second", "Third"), 
          xlab="Passenger Class", ylab="Percentage survived", ylim=c(0,100))
  
  jpeg("3a_survivors_based_on_age.jpg")
  hist_info <- hist(df$Age, , breaks=8, plot = FALSE)
  hist_info$density <- 100.0* hist_info$counts/sum(hist_info$counts)
  plot(hist_info, freq = FALSE, ylim = c(0, 40), main="", 
       xlab="Survivor Age", ylab="Percentage survived")
  
  
}

p3a_logit <- function(df)
{
  mod_df <- dummy_cols(df, select_columns = c('Sex', 'PClass'), 
                       remove_selected_columns=TRUE)
  
  fit_model = glm(formula=Survived~Age+Sex_female+Sex_male+PClass_1st+PClass_2nd+PClass_3rd,
                  data=mod_df, family=binomial())
  
  odds = or_glm(data=df, model=fit_model, incr=list(Age=10))
  print(anova(fit_model, test="Chisq"))
  print("_______________")
  print(odds)
}

p3b_testing_model <- function(df)
{
  mod_df <- dummy_cols(df, select_columns = c('Sex', 'PClass'), 
                       remove_selected_columns=TRUE)
  
  fit_model = glm(formula=Survived~Age*Sex_female+Age*Sex_male+
                    Age*PClass_1st+Age*PClass_2nd+Age*PClass_3rd, 
                  data=mod_df, family=binomial())
  
  print(anova(fit_model, test="Chisq"))
}

p3b_final_model <- function(df)
{
  mod_df <- dummy_cols(df, select_columns = c('Sex', 'PClass'), 
                       remove_selected_columns=TRUE)
  
  fit_model = glm(formula=Survived~Sex_female+Sex_male+PClass_1st+
                    PClass_2nd+PClass_3rd+Age:Sex_female+Age:Sex_male, 
                  data=mod_df, family=binomial())
  # print(anova(fit_model, test="Chisq"))
  
  ## Creating dataset for prediction
  
  class = rep(c("1st", "2nd", "3rd"), times=2)
  sex = rep(c("female", "male"), each=3)
  age = rep(c(55), times=6)
  test_data = data.frame(class, sex, age)
  names(test_data) = c("PClass", "Sex", "Age")

  mod_test_data = dummy_cols(test_data, select_columns = c("Sex", "PClass"),
                             remove_selected_columns = TRUE)

  predictions = predict(fit_model, mod_test_data, type="response",
                        interval="predict")
  mod_test_data$probs = predictions
  print(mod_test_data)
}

p3d <- function(df)
{
  # Contingency table for PClass and Survived
  pclass_con_table = table(df$PClass, df$Survived)
  print(addmargins(pclass_con_table))
  
  # Contingency table for Sex and Survived
  sex_con_table = table(df$Sex, df$Survived)
  print(addmargins(sex_con_table))
  
  #Chi-Square test
  print("Chi square test for PClass and Survived")
  print(chisq.test(pclass_con_table))
  
  print("Chi Square test for Sex and Survived")
  print(chisq.test(sex_con_table))
  
  
}
  
df = read.csv("/home/ankur/Ankur/CLS/Y2/P4/EDDA/assignments/2/titanic.txt",
              header=TRUE, sep="\t")

# graph_plot(df)
p3a_logit(df)
# print("____________________")
# p3b_testing_model(df)
# p3b_final_model(df)
# p3d(df)

