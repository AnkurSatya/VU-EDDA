library(MASS)
library(hash)
library(dplyr)
library(ggplot2)

# Problem 4.1
randomise_additives <- function (num_blocks, num_plots)
{
  data = hash()
  data["block"] = c();
  data["N"] = c();
  data["P"] = c();
  data["K"] = c();
  
  for (p in 1:num_blocks)
  {
    plots_for_n = sample(c(1:4), 2);
    plots_for_p = sample(c(1:4), 2);
    plots_for_k = sample(c(1:4), 2);
    
    list_ = list(c(rep(0,4)), c(rep(0,4)), c(rep(0,4)));
    
    for (b in 1:2)
    {
      list_[[1]][plots_for_n[b]] = 1;
      list_[[2]][plots_for_p[b]] = 1;
      list_[[3]][plots_for_k[b]] = 1;
    }
    
    data[["block"]] = c(data[["block"]], c(rep(p,4)));
    data[["N"]] = c(data[["N"]], list_[[1]]);
    data[["P"]] = c(data[["P"]], list_[[2]]);
    data[["K"]] = c(data[["K"]], list_[[3]]);
  }
  
  dataframe = data.frame(Block = data[["block"]], N=data[["N"]], P=data[["P"]], K=data[["K"]])
  return (dataframe)
}


# num_blocks = 6;
# num_plots = 4;
# 
# rand_dataframe = randomise_additives(num_blocks, num_plots)
# write.csv(rand_dataframe, "tmp.csv")

# Problem 4.2

plot_yield <- function(){
  df = npk
  nitrogen_present_yield = sum(filter(df, N==1)$yield)
  nitrogen_absent_yield  = sum(filter(df, N==0)$yield)
  
  num_blocks = length(unique(npk$block))
  
  avg_yield_with_nitrogen = round(nitrogen_present_yield/num_blocks, 2)
  avg_yield_wo_nitrogen = round(nitrogen_absent_yield/num_blocks, 2)
  
  jpeg("avg_yield.jpg")
  barplot(c(avg_yield_with_nitrogen, avg_yield_wo_nitrogen), names.arg = c("With Nitrogen", "Without Nitrogen"), 
          ylab="Average yield per block")
}

plot_yield()


# Problem 4.3
anova_block_and_nitrogen <- function(dataset){
  dataset$block = as.factor(dataset$block)
  dataset$N = as.factor(dataset$N)
  
  model = lm(yield~block*N, data=dataset)
  return (anova(model))
}

# anova_block_and_nitrogen(npk)

# Problem 4.4
anova_all <- function(dataset){
    # dataset$pair <- as.integer(dataset$block) * (as.numeric(dataset[, col]) -1)
    
  for (col2 in c("block", "N", "P", "K")){
    dataset[, col2] = as.factor(dataset[, col2])
  }
  model_n = aov(lm(yield~N*block+P+K, data=dataset))
  model_p = aov(lm(yield~P*block+N+K, data=dataset))
  model_k = aov(lm(yield~K*block+N+P, data=dataset))
  model_no_interaction = aov(lm(yield~block+N+P+K, data=dataset))
  model_without_block = aov(lm(yield~N+P+K, data=dataset))
  print(summary(model_no_interaction))
}

anova_all(npk)



