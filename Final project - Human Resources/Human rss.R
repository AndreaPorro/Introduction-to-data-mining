data=read.csv("HRDataset_v14.csv")

head(data)

#EDA

dim(data)

library(DataExplorer)
plot_intro(data)

plot_missing(data)

data = na.omit(data)
plot_intro(data)

summary(data)

library(ggplot2)

ggplot(data, aes(x = Salary)) +
  geom_density(fill = "lightblue", alpha = 0.5) + 
  geom_vline(aes(xintercept = mean(Salary, na.rm = TRUE)), 
             color = "red", linetype = "dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = median(Salary, na.rm = TRUE)), 
             color = "blue", linetype = "dashed", linewidth = 1) + 
  labs(title = "Density Plot of Salary", 
       x = "Salary", 
       y = "Density") +
  theme_minimal()

ggplot(data, aes(x = Sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Gender Distribution", 
       x = "Gender", 
       y = "Count") +
  theme_minimal()

ggplot(data, aes(x = PerfScoreID)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Perf Score Distribution", 
       x = "Perf Score", 
       y = "Count") +
  theme_minimal()

library(dplyr)

zero_var_cols <- data %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ var(., na.rm = TRUE) == 0)) %>%
  select(where(~ .)) %>%
  names()

if (length(zero_var_cols) > 0) {
  cat("Variables with variability equal to 0:\n", paste(zero_var_cols, collapse = ", "), "\n")
} else {
  cat("There aren't any variables with variability equal to 0.\n")
}

library(tidyr)

high_cardinality_vars <- data %>%
  select(where(~ is.factor(.) || is.character(.))) %>%
  summarise(across(everything(), ~ n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "cardinality") %>%
  filter(cardinality > 200) %>%
  pull(variable) 

if (length(high_cardinality_vars) > 0) {
  cat("These variables have high cardinality and must be deleted from the study:\n", 
      paste(high_cardinality_vars, collapse = ", "), "\n")
} else {
  cat("No variables have high cardinality.\n")
}

FinalData <- data %>%
  select(-Employee_Name, -DOB)

numerical_cardinality <- data %>%
  select(where(is.numeric)) %>%      
  summarise(across(everything(), ~ n_distinct(.)))

print(numerical_cardinality)

FinalData <- FinalData %>%
  select(-EmpID)

Data_Num = FinalData %>% select_if(is.numeric)
corr_matrix = cor(Data_Num, use='complete.obs')
head(corr_matrix)

library(corrplot)
corrplot.mixed(corr_matrix, lower='number', upper='circle', tl.pos="lt",
               tl.col="black", tl.srt=45, diag='u', title='Correlation plot')

corr_EMP_TermD=corr_matrix["EmpStatusID", "Termd"]
if (abs(corr_EMP_TermD) < 0.5) {
  cat("There is a weak or no linear relationship. \n")
} else {
  cat("There is a moderately or strongly correlation. \n")
}

library(psych)
pairs.panels(Data_Num)

#There is any relationship between salary and special project?
library(ggplot2)

ggplot(FinalData, aes(x = SpecialProjectsCount, y = Salary)) +
  geom_point(color = "blue", size = 2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Scatterplot with Regression Line",
       x = "Special Projects Count",
       y = "Salary") +
  theme_minimal()

ggplot(FinalData, aes(x = SpecialProjectsCount, y = Salary, color = as.factor(DeptID))) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Scatterplot with Regression Line",
       x = "Special Projects Count",
       y = "Salary",
       color = "Department ID") + # Adds a legend title for DeptID
  theme_minimal()

#There is any relationship between who a person works for and their performance score?

ggplot(FinalData, aes(x = ManagerID, y = PerfScoreID)) +
  geom_point(color = "blue", size = 2) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Scatterplot with Regression Line: ManagerID vs PerfScoreID",
       x = "ManagerID",
       y = "PerfScoreID") +
  theme_minimal()

#Does gender influence the relationship between salary and performance score

ggplot(FinalData, aes(x = PerfScoreID, y = Salary, color = Sex)) +
  geom_point(size = 2) + 
  labs(title = "Scatterplot of Salary vs PerfScoreID by Gender",
       x = "Performance Score ID",
       y = "Salary",
       color = "Gender") + 
  theme_minimal()

FinalData %>%
  group_by(Sex, Position) %>%
  summarize(Mean_Salary = mean(Salary, na.rm = TRUE)) %>%
  arrange(Position, Sex)

ggplot(FinalData, aes(x = PerfScoreID, y = Salary, color = Sex)) +
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE, aes(color = Sex)) + 
  labs(title = "Scatterplot of Salary vs PerfScoreID by Gender",
       x = "Performance Score ID",
       y = "Salary",
       color = "Gender") + 
  theme_minimal()

#Each manager how many workers have?

manager_count <- FinalData %>%
  group_by(ManagerName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

ggplot(manager_count, aes(x = reorder(ManagerName, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Employees per Manager",
       x = "Manager Name",
       y = "Number of Employees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

manager_count <- manager_count %>%
  mutate(Group = case_when(
    Count > 20 ~ ">20",
    Count >= 10 & Count <= 20 ~ "10-20",
    Count < 10 ~ "<10"
  ))

ggplot(manager_count, aes(x = reorder(ManagerName, -Count), y = Count, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Employees per Manager",
       x = "Manager Name",
       y = "Number of Employees") +
  scale_fill_manual(values = c(">20" = "red", "10-20" = "yellow", "<10" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#There is any discrimination on the salary based on the race?

ggplot(FinalData, aes(x = Position, y = Salary)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Salary by Position",
       x = "Position",
       y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(FinalData, aes(x = Position, y = Salary, color = RaceDesc)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Salary by Position",
       x = "Position",
       y = "Salary",
       color = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#The number of absences is related to the salary?

ggplot(FinalData, aes(x = Absences, y = Salary)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", se = TRUE) + 
  labs(title = "Scatter Plot of Salary vs Absences with Regression Line",
       x = "Absences",
       y = "Salary") +
  theme_minimal()

#What are our best recruiting sources

ggplot(FinalData, aes(x = RecruitmentSource, fill = PerformanceScore)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Performance Scores by Recruitment Source",
       x = "Recruitment Source",
       y = "Count",
       fill = "Performance Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

performance_table <- FinalData %>%
  group_by(RecruitmentSource, PerformanceScore) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(RecruitmentSource) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

performance_pivot <- performance_table %>%
  select(RecruitmentSource, PerformanceScore, Percentage) %>%
  pivot_wider(names_from = PerformanceScore, values_from = Percentage, values_fill = 0)

print(performance_pivot)

ggplot(FinalData, aes(x = RecruitmentSource, fill = Sex)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Performance Scores by Recruitment Source",
       x = "Recruitment Source",
       y = "Count",
       fill = "Performance Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(FinalData, aes(x = RecruitmentSource, fill = RaceDesc)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Performance Scores by Recruitment Source",
       x = "Recruitment Source",
       y = "Count",
       fill = "Performance Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# There is a relationship between who a person works for and their performance score?

table_manager_performance <- table(FinalData$ManagerName, FinalData$PerformanceScore)
print(table_manager_performance)

chisq_test <- chisq.test(table_manager_performance)
print(chisq_test)

ggplot(FinalData, aes(x = ManagerName, fill = PerformanceScore)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Performance Scores by Manager",
       x = "Manager Name",
       y = "Proportion",
       fill = "Performance Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PCA 
Data_Num <- Data_Num %>%
  select(-EmpID)

summary(prcomp(Data_Num))

data_scaled=scale(Data_Num)
data_pca=prcomp(data_scaled)
summary(data_pca)

head(data_pca$rotation)

head(data_pca$x)

library(factoextra)
fviz_pca_ind(data_pca, title="PCA - human resources")

fviz_pca_biplot(data_pca, title="PCA - human resources")

fviz_pca_biplot(data_pca,
                col.ind='cos2',
                gradient.cols = c('mistyrose', '#8B0000'),
                repel= T,
                geon='point',
                title= 'PCA - human resources')

## EIGENVALUES

eig.val = get_eigenvalue(data_pca)
eig.val

fviz_eig(data_pca, addlabels=T)

## ANALYSIS RESULTS FOR VARIABLES

var = get_pca_var(data_pca)
var

round(var$cor, 2)
corrplot(var$cor, is.corr=F)

round(var$contrib, 2)
corrplot(var$contrib, is.corr=F)
fviz_contrib(data_pca, choice = 'var', axes=1)
fviz_contrib(data_pca, choice = 'var', axes=5)
fviz_contrib(data_pca, choice = 'var', axes=8)

round(var$cos2, 2)
fviz_cos2(data_pca, choice='var', axes=1)
fviz_cos2(data_pca, choice='var', axes=5)
fviz_cos2(data_pca, choice='var', axes=8)
fviz_cos2(data_pca, choice='var', axes=1:8)

fviz_nbclust(var$coord, kmeans, method='wss', k.max=nrow(var$coord) -1) +
  ggtitle('Elbow method')

res.km=kmeans(var$coord, center = 3 , nstart =25)
grp= as.factor(res.km$cluster)

fviz_pca_var(data_pca,
             col.var=grp,
             palette=c('red','blue','green'),
             legend.title='Cluster',
             labelsize=2,
             repel=T
             )
##  ANALYSIS RESULTS FOR INDIVIDUALS

ind = get_pca_ind(data_pca)
ind

res.km=kmeans(ind$coord, centers = 3, nstart=25)
grp2=as.factor(res.km$cluster)
fviz_pca_ind(data_pca,
             geom.in="point",
             col.ind=grp2,
             palette=c('red','blue','green'),
             legend.title='Cluster',
             labelsize=2,
             repel=T,
             addEllipses = T
)

fviz_pca_biplot(data_pca,
                repel=T,
                geom='point',
                col.var='darkblue',
                col.ind=grp2,
                palette=c('red','blue','green'),
                addEllipses = T)

# CLUSTER ANALYZE

library(FactoMineR)
data_st=scale(Data_Num)

fviz_nbclust(data_st,
             kmeans,
             method='wss',
             nstart=10) +
  geom_vline(xintercept = 3, linetype=2)

library(cluster)
fviz_nbclust(data_st, pam, method='silhouette', nstart=10)

data.km=kmeans(data_st, 3, nstart=10)
print(data.km)

aggregate(Data_Num, by=list(data.km$cluster), mean)

Data_Num$Cluster = data.km$cluster
print('Cluster sizes:')
print(table(Data_Num$Cluster))

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_normalized <- as.data.frame(lapply(Data_Num, normalize))

data_normalized$Cluster <- data.km$cluster

data_long_normalized <- data_normalized %>%
  pivot_longer(
    cols = -Cluster, # All columns except Cluster
    names_to = 'Characteristic',
    values_to = 'Value'
  )


ggplot(data_long_normalized, aes(x = Characteristic, y = Value, fill = as.factor(Cluster))) +
  geom_bar(stat = 'summary', fun = 'mean', position = 'dodge') +
  theme_minimal() +
  labs(title = 'Cluster Characteristics Comparison (Normalized Data)',
       x = 'Characteristic',
       y = 'Mean Value (Normalized)',
       fill = 'Cluster') +
  stat_summary(
    fun = 'mean',
    aes(label = sprintf("%.2f", after_stat(y))),
    geom = 'text',
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  )

fviz_cluster(data.km, data=data_st, geom='point', repel=F, show.clust.cent = T)

## Choice of distance
datos.dist.eucl=dist(data_st, method='euclidean')
fviz_dist(datos.dist.eucl)
### WARD METHOD
dato.hc=hclust(d=datos.dist.eucl, method='ward.D2')
fviz_dend(dato.hc, k=3, cex=0.5, rect=T, rect_border = 'blue', rect_fill =F)

res.coph=cophenetic(dato.hc)
cor(datos.dist.eucl, res.coph)

###AVERAGE METHOD
dato.md=hclust(d=datos.dist.eucl, method='average')
fviz_dend(dato.md, k=3, cex=0.5, rect=T, rect_border = 'blue', rect_fill =F)

res.coph=cophenetic(dato.md)
cor(datos.dist.eucl, res.coph)

###SINGLE METHOD
dato.sg=hclust(d=datos.dist.eucl, method='single')
fviz_dend(dato.sg, k=3, cex=0.5, rect=T, rect_border = 'blue', rect_fill =F)

res.coph=cophenetic(dato.sg)
cor(datos.dist.eucl, res.coph)

#Can we predict who is going to terminate and who isn't? What level of accuracy can we achieve on this?

library(rpart.plot)
library(rpart)
library(caret)

Data_Num <- na.omit(Data_Num)

set.seed(123)

train_index <- createDataPartition(Data_Num$Termd, p = 0.7, list = FALSE)
train_data <- Data_Num[train_index, ]
test_data <- Data_Num[-train_index, ]

train_data$Termd <- factor(train_data$Termd, levels = c(0, 1))
test_data$Termd <- factor(test_data$Termd, levels = c(0, 1))

tree_model <- rpart(Termd ~ ., data = train_data, method = "class")

predictions <- predict(tree_model, test_data, type = "class")

predictions <- factor(predictions, levels = levels(test_data$Termd))

conf_matrix <- confusionMatrix(predictions, test_data$Termd)

print(conf_matrix)

rpart.plot(tree_model, main = "Decision Tree for Termd (Balanced Training)")

