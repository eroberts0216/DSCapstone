library(readxl)
library(cluster)
library(factoextra)

data_Pysch = read_excel("OneDrive - Quinnipiac University/24-25/Spring/DS480/Project/DSCapstone/RankingTables/Psych_ranking.xlsx")

df_Pysch = scale(data_Pysch)

fviz_nbclust(df_Pysch, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_Pysch <- kmeans(data_Pysch, 4, nstart = 25)

fviz_cluster(km_Pysch,df_Pysch,main = "Psychologist Education k-Means", xlab = "Scale 1 - 5", ylab = "Sum of Minutes")

print(km_Pysch)

#plot(data_Pysch$scale_1_5,data_Pysch$min_sum)