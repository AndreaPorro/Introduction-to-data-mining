#Part A
getwd() 

#EDA part A
data=read.csv("Life Expectancy Data.csv")
NewData= subset(data, Year==2015)
head(NewData)

library(DataExplorer)
plot_intro(NewData)
plot_missing(NewData)

summary(NewData)

library(dplyr)
NewData_numeric = NewData %>% select_if(is.numeric)
plot_intro(NewData_numeric)

NewData_NA = NewData_numeric %>% select_if(~all(!is.na(.)))
plot_intro(NewData_NA)

zero_var_cols <- NewData_NA %>%
  summarise(across(everything(), ~ var(., na.rm = TRUE) == 0)) %>%
  select(where(~ .)) %>%
  names()


if (length(zero_var_cols) > 0) {
  cat("Variables with variability equal to 0 are:\n", paste(zero_var_cols, collapse = ", "))
} else {
  cat("There are no variables with variability equal to 0 in the dataset.")
}

DataFinal = NewData_NA %>% select(! Year)

library(corrplot)
corr_matrix=cor(DataFinal, use = "complete.obs") 
print(corr_matrix)

corrplot.mixed(corr_matrix,lower='number',upper="circle", tl.pos="lt", tl.col="black",
               tl.srt=45, diag="u", title='Correlation plot')

corr_AM_LE=corr_matrix["Life.expectancy", "Adult.Mortality"]
if (abs(corr_AM_LE) < 0.5) {
  print("Adult mortality and life expectancy have a weak or no linear relationship.")
} else {
  print("Adult mortality and life expectancy have moderately or strongly relationship")
}

library(psych)
pairs.panels(DataFinal)

#PCA part A

prcomp(DataFinal) 
summary(prcomp(DataFinal))

data_scaled=scale(DataFinal)
data_pca=prcomp(data_scaled)
print(data_pca)

summary(data_pca)

data_pca$rotation

data_pca$x 

library(ggplot2)
library(factoextra)
fviz_pca_ind(data_pca, title="PCA - datos life expectation ") 
fviz_pca_biplot(data_pca, title="PCA -Life expectation data")

fviz_pca_biplot(data_pca,
                col.ind = "cos2",
                gradient.cols = c("mistyrose", "#8B0000"),
                repel = TRUE,
                geom = "point",
                title="PCA -Life expectation data"
)

eig.val = get_eigenvalue(data_pca)
eig.val
fviz_eig(data_pca, addlabels = TRUE)

var=get_pca_var(data_pca)
var
round(var$cor,2)
corrplot(var$cor, is.corr = FALSE)

round(var$cos2,2)
fviz_cos2(data_pca, choice = "var", axes = 1)
fviz_cos2(data_pca, choice = "var", axes = 2)
fviz_cos2(data_pca, choice = "var", axes = 3)
fviz_cos2(data_pca, choice = "var", axes = 1:3)

round(var$contrib,2)
corrplot(var$contrib,is.corr = F)
fviz_contrib(data_pca, choice = "var", axes = 1)
fviz_contrib(data_pca, choice = "var", axes = 2)
fviz_contrib(data_pca, choice = "var", axes = 3)
fviz_contrib(data_pca, choice = "var", axes = 1:3)



library(factoextra)

nrow(var$coord)

fviz_nbclust(var$coord, kmeans, method = "wss", k.max = nrow(var$coord) - 1) +
  ggtitle("Elbow method")

res.km = kmeans(var$coord, centers = 3, nstart = 25)
grp = as.factor(res.km$cluster)

fviz_pca_var(data_pca, col.var = grp,
             palette = c("red","blue","green"),
             legend.title = "Cluster",
             labelsize = 2,
             repel=T
)

ind = get_pca_ind(data_pca)
ind

res.km = kmeans(ind$coord, centers = 3, nstart = 25)
grp2 = as.factor(res.km$cluster)
fviz_pca_ind(data_pca,
             geom.ind = "point", 
             col.ind = grp2, 
             palette = c("red", "blue", "green"),
             addEllipses = TRUE, 
             legend.title = "Groups"
)

fviz_pca_biplot(data_pca,
                repel=T,
                geom="point",
                col.var = "darkblue",
                col.ind = grp2,
                palette = c("red", "blue", "green"),
                addEllipses = T
)


###############################################################################

#Part B

Star=read.csv("Starbucks satisfactory survey encode cleaned.csv")
head(Star)

#EDA part B

plot_intro(Star)


zero_var_cols_star <- Star %>%
  summarise(across(everything(), ~ var(., na.rm = TRUE) == 0)) %>%
  select(where(~ .)) %>%
  names()


if (length(zero_var_cols_star) > 0) {
  cat("Variables with variability equal to 0 are:\n", paste(zero_var_cols, collapse = ", "))
} else {
  cat("There are no variables with variability equal to 0 in the dataset.")
}

nomi_variabili_da_eliminare = c("itemPurchaseCoffee", "itempurchaseCold", "itemPurchasePastries", 
                                "itemPurchaseJuices", "itemPurchaseSandwiches", "itemPurchaseOthers", 
                                "promoMethodApp", "promoMethodSoc", "promoMethodEmail", "promoMethodDeal", 
                                "promoMethodFriend", "promoMethodDisplay", "promoMethodBillboard")

StarFinal = Star %>% select(-c(nomi_variabili_da_eliminare))

summary(StarFinal)

corr_matrix_star=cor(StarFinal, use = "complete.obs") 
head(corr_matrix_star)

corrplot.mixed(corr_matrix_star,lower='number',upper="circle", tl.pos="lt", tl.col="black",
               tl.srt=45, diag="u", title='Correlation plot')

corr_WF_M=corr_matrix_star["wifiRate", "serviceRate"]
if (abs(corr_WF_M) <0.5) {
  print("wife rate and service rate have a weak or no linear relationship.")
} else {
  print("wife rate and service rate have moderately or strongly relationship")
}

#PCA PART
prcomp(StarFinal) 
summary(prcomp(StarFinal))

Star_scaled=scale(StarFinal)
Star_pca=prcomp(Star_scaled)
summary(Star_pca)

Star_pca$rotation

head(Star_pca$x)

fviz_pca_ind(Star_pca, title="PCA - Starbucks satisfactory survey") 

fviz_pca_biplot(Star_pca, title="PCA - Starbucks satisfactory survey")

fviz_pca_biplot(Star_pca,
                col.ind = "cos2",
                gradient.cols = c("mistyrose", "#8B0000"),
                repel = TRUE,
                geom = "point",
                title="PCA -Life expectation data"
)

eig.val = get_eigenvalue(Star_pca)
eig.val
fviz_eig(Star_pca, addlabels = TRUE)

var_star=get_pca_var(Star_pca)
var_star

round(var_star$cor,2)
corrplot(var_star$cor, is.corr = FALSE)

round(var_star$cos2,2)
fviz_cos2(Star_pca, choice = "var", axes = 1)
fviz_cos2(Star_pca, choice = "var", axes = 1:10)

round(var_star$contrib,2)
corrplot(var_star$contrib,is.corr = F)
fviz_contrib(Star_pca, choice = "var", axes = 1)
fviz_contrib(Star_pca, choice = "var", axes = 8 )
fviz_contrib(Star_pca, choice = "var", axes = 1:10)


nrow(var_star$coord)
fviz_nbclust(var_star$coord, kmeans, method = "wss", k.max = nrow(var_star$coord) - 1) +
  ggtitle("Elbow method")

res.km = kmeans(var_star$coord, centers = 4, nstart = 25)
grp = as.factor(res.km$cluster)

fviz_pca_var(Star_pca, col.var = grp,
             palette = c("red","blue","green", "purple"),
             legend.title = "Cluster",
             labelsize = 2,
             repel=T
)

ind_star = get_pca_ind(Star_pca)
ind_star

res.km = kmeans(ind_star$coord, centers = 3, nstart = 25)
grp2_star = as.factor(res.km$cluster)
fviz_pca_ind(Star_pca,
             geom.ind = "point", 
             col.ind = grp2_star, 
             palette = c("red", "blue", "green"),
             addEllipses = TRUE, 
             legend.title = "Groups"
)

fviz_pca_biplot(Star_pca,
                repel=T,
                geom="point",
                col.var = "darkblue",
                col.ind = grp2_star,
                palette = c("red", "blue", "green"),
                addEllipses = T
)

