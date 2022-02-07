## Comparison
# Fixed the factor as many as possible

df<-read.csv("df_clean.csv")

# Example 1
gd1<-daisy(df, metric = "gower")
set.seed(123)
km1<-kmeans(gd1, 2)

df2<-df %>% select(-current)
gd2<-daisy(df2, metric = "gower")
km2<-kmeans(gd2, 2)

## km1
# input
# output

# km2
# input
# output

# Example 2
df_num<-df %>% select(age, balance, duration, campaign, previous, pdays)
eu<-dist(scale(df_num))

set.seed(123456)
for (i in 1:10)
{
  sw_temp<-numeric()
  ch_temp<-numeric()
  for (j in 2:5)
  {
    km<-kmeans(scale(df_num), j)
    sw_temp<-c(sw_temp, sw(km$cluster, eu))
    ch_temp<-c(ch_temp, fpc::calinhara(scale(df_num), pm$clustering))
  }
  suggest_k_sw<-c(suggest_k_sw, which.max(sw_temp)+1)
  suggest_k_ch<-c(suggest_k_ch, which.max(ch_temp)+1)
}

set.seed(123)
for (i in 1:10)
{
  sw_temp<-numeric()
  ch_temp<-numeric()
  for (j in 2:5)
  {
    pm<-pam(scale(df_num), j)
    sw_temp<-c(sw_temp, sw(pm$clustering, eu))
    ch_temp<-c(ch_temp, fpc::calinhara(scale(df_num), pm$clustering))
  }
  suggest_k_sw<-c(suggest_k_sw, which.max(sw_temp)+1)
  suggest_k_ch<-c(suggest_k_ch, which.max(ch_temp)+1)
}

## for loop 1
# input
# output

# for loop 2
# input
# output

# Example 3
gd1<-daisy(df, metric = "gower")
set.seed(123)
pm1<-pam(gd1, 2, diss = TRUE)
sw(km1$cluster, gd1)

df_num<-df %>% select(age, balance, duration, campaign, previous, pdays)
eu<-dist(scale(df_num))
km1<-kmeans(eu, 2)
fpc::calinhara(scale(df_num), km1$cluster)

# what should I do if I want to compare the effect of using different clustering methods
