# Find the distribution parameters using mixture model. 
# Based on code by James Weedon.

library(data.table)

args = commandArgs(trailingOnly=TRUE) #if slicing needed
slice <- args[1]

filename <- paste0("result_",slice,".rds")
df_results <- as.data.frame(readRDS(filename)) # at this point, still also has sds

n_rows <- nrow(df_results)

df_sds <- df_results[seq(2, 200, by=2)]
df_means <- df_results[-seq(2, 200, by=2)]

n <- 100

mixture <- function(samp){
  sampmean <- samp[-seq(2, 200, by=2)]
  sampsd <- samp[seq(2, 200, by=2)]
  mxt <- sqrt(((1/n) * sum(sampmean^2+sampsd^2))-mean(sampmean)^2)
  return(mxt)
}

mxt <- apply(FUN=mixture, MARGIN=1, df_results)

totmean <- rowMeans(df_means)
sd_of_means <- apply(df_means, 1, sd)


df_mxt_res <- data.frame(mean=totmean, mixed_sd=mxt, sd_of_means=sd_of_means)
filename <- paste0("mxt_res_",slice,".rds")
saveRDS(df_mxt_res, file=filename)
