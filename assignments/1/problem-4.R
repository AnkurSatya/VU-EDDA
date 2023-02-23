library(MASS)
library(hash)

num_blocks = 6;
num_plots = 4;

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
print(dataframe)
write.csv(dataframe, "tmp.csv")

