library(MASS)
library(hash)
library(dplyr)
library(ggplot2)

# Problem 3.1
graph_plot <- function (df)
{
  # Survival based on sex
  num_women_survived = nrow(filter(df, Sex=="female", Survived==1))
  num_men_survived = nrow(filter(df, Sex=="male", Survived==1))
  
  # Survival based on passenger class
  tmp_df = filter(df, Survived==1)
  num_first_class = nrow(filter(tmp_df, PClass=="1st"))
  num_second_class = nrow(filter(tmp_df, PClass=="2nd"))
  num_third_class = nrow(filter(tmp_df, PClass=="3rd"))
  
}

df = read.csv("/home/ankur/Ankur/CLS/Y2/P4/EDDA/assignments/2/titanic.txt",
              header=TRUE, sep="\t")

graph_plot(df)
