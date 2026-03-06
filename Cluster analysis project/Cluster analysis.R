getwd()

data=read.csv("houseprice.csv")
head(data)

library(DataExplorer)
plot_intro(data)

summary(data)

zero_var_cols = data %>%
  summarise(across(everything(), ~ var(. , na.rm = TRUE) == 0)) %>%
  select(where(~ .)) %>%
  names()

if (length(zero_var_cols) > 0) {
  cat('Variables with variability equal to 0: \n', paste(zero_var_cols, collapse = ','))
} else {
  cat("There aren't any variable which has variability equal to 0")
}

library(corrplot)
corr_matrix=cor(data, use="complete.obs")
head(corr_matrix)

corrplot.mixed(corr_matrix, lower='number', upper='circle', tl.pos='lt',
               tl.col='black', tl.srt=45, diag='u', title='Corrlation plot')

corr_LA_B=corr_matrix['Living.Area', 'Bathrooms']
if (abs(corr_LA_B) < 0.5) {
  print('Living area and bathrooms have a weak or no linear relationship')
} else {
  print('Living area and bathrooms have moderately or strongly relationship')
}

library(psych)
pairs.panels(data)

#Cluster analysis

data_st=scale(data)

library(factoextra)
library(FactoMineR)
fviz_nbclust(data_st, kmeans, method="wss", nstart=10)
fviz_nbclust(data_st, kmeans, method="wss", nstart=10) +
  geom_vline(xintercept = 2, linetype=2)

#alternative algorithm k-methods
fviz_nbclust(data_st, pam, method="silhouette", nstart=10)

dato.km=kmeans(data_st, 2, nstart=10)
print(dato.km)
aggregate(data, by=list(dato.km$cluster), mean)

data$Cluster <- dato.km$cluster
print("Cluster Sizes:")
print(table(data$Cluster))

library(ggplot2)
library(tidyr)
library(dplyr)
data_long <- data %>%
  pivot_longer(
    cols = -Cluster,                
    names_to = "Characteristic",    
    values_to = "Value"  
  )

ggplot(data_long, aes(x = Characteristic, y = Value, fill = as.factor(Cluster))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +  # Media per cluster
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cluster Characteristics Comparison",
       x = "Characteristic", 
       y = "Mean Value", 
       fill = "Cluster") +
  geom_text(stat = "summary", fun = "mean",
            aes(label = sprintf("%.2f", ..y..)),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3)

library(cluster)
fviz_cluster(dato.km, data=data_st,geom='point', repel=F, show.clust.cent=T)

#Euclidea
datos.dist.eucl=dist(data_st, method='euclidean')
fviz_dist(datos.dist.eucl)

#Ward.D2
dato.hc=hclust(d=datos.dist.eucl, method='ward.D2')
fviz_dend(dato.hc, k = 2, cex = 0.5, rect = TRUE, rect_border = "blue", rect_fill = FALSE)
res.coph=cophenetic(dato.hc)
cor(datos.dist.eucl, res.coph)

#Media
dato.md = hclust(d=datos.dist.eucl, method='average')
fviz_dend(dato.md, k = 2, cex = 0.5, rect = TRUE, rect_border = "blue", rect_fill = FALSE)
res.coph=cophenetic(dato.md)
cor(datos.dist.eucl, res.coph)

#Single
dato.sg = hclust(d=datos.dist.eucl, method='single')
fviz_dend(dato.sg, k = 2, cex = 0.5, rect = TRUE, rect_border = "blue", rect_fill = FALSE)
res.coph=cophenetic(dato.sg)
cor(datos.dist.eucl,res.coph)



