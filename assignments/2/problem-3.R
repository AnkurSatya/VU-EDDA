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
  
  jpeg("3a_survivors_based_on_sex.jpg")
  barplot(c(round(100.0*num_women_survived/num_survivors, 2), 
            round(100.0*num_men_survived/num_survivors, 2)), 
          names.arg = c("Women", "Men"), 
          ylab="Number survived", ylim=c(0, 100))
  
  # Survival based on passenger class
  num_first_class = nrow(filter(tmp_df, PClass=="1st"))
  num_second_class = nrow(filter(tmp_df, PClass=="2nd"))
  num_third_class = nrow(filter(tmp_df, PClass=="3rd"))
  
  jpeg("3a_survivors_based_on_class.jpg")
  barplot(c(round(100.0*num_first_class/num_survivors, 2), 
            round(100.0*num_second_class/num_survivors, 2), 
            round(100.0*num_third_class/num_survivors, 2)), 
          names.arg = c("First", "Second", "Third"), 
          ylab="Number survived", ylim=c(0,100))
  
  jpeg("3a_survivors_based_on_age.jpg")
  hist_info <- hist(tmp_df$Age, , breaks=8, plot = FALSE)
  hist_info$density <- 100.0* hist_info$counts/sum(hist_info$counts)
  plot(hist_info, freq = FALSE, ylim = c(0, 30), main="", 
       xlab="Survivor Age", ylab="Percentage")
  
  
}

p3a_logit <- function(df)
{
  mod_df <- dummy_cols(df, select_columns = c('Sex', 'PClass'), 
                       remove_selected_columns=TRUE)
  
  fit_model = glm(formula=Survived~Age+Sex_female+Sex_male+PClass_1st+PClass_2nd+PClass_3rd,
                  data=mod_df, family=binomial())
  
  odds = or_glm(data=df, model=fit_model, incr=list(Age=5))
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
  
df = read.csv("/home/ankur/Ankur/CLS/Y2/P4/EDDA/assignments/2/titanic.txt",
              header=TRUE, sep="\t")

# graph_plot(df)
# p3a_logit(df)
# print("____________________")
# p3b_testing_model(df)
p3b_final_model(df)