library(readxl)
library(cluster)
library(factoextra)

data_Counselors = read_excel("~/Library/CloudStorage/OneDrive-QuinnipiacUniversity/24-25/Spring/DS480/Project/DSCapstone/RankingTables/Counselors_ranking.xlsx")

df_Counselors = scale(data_Counselors)

fviz_nbclust(df_Counselors, kmeans, method = "wss", k.max = 5)

set.seed(123)
km_Counselors <- kmeans(data_Counselors, 4, nstart = 25)

fviz_cluster(km_Counselors,df_Counselors)

print(km_Counselors)

#plot(data_Counselors$scale_1_5,data_Counselors$min_sum)