Data = read.csv("World-happiness-report-updated_2024.csv")
View(Data)

 NewData=subset(Data,year==2023)
View(NewData)

summary(NewData)

library(DataExplorer)
plot_intro(NewData)

plot_missing(NewData)

summary(NewData)

library(dplyr)
NewData_numeric = NewData %>% select_if(is.numeric)
NewData_numeric = NewData_numeric %>% select(-year)
corr_matrix=cor(NewData_numeric, use = "complete.obs")
print(corr_matrix)

library(corrplot)
corrplot.mixed(corr_matrix, lower="number", upper="circle", tl.pos = "lt",
               tl.col="black", tl.srt=45, diag="u", title="Correlation plot")

corr_GPD_Healpth=corr_matrix["Log.GDP.per.capita","Healthy.life.expectancy.at.birth"]

if (abs(corr_GPD_Healpth) < 0.5) {
  cat("Age and balance have a weak or no linear relationship.\n")
} else {
  cat("Age and balance are moderately or strongly correlated.\n")
}

library(ggplot2)
ggplot(na.omit(NewData),aes(x=Log.GDP.per.capita, y=Life.Ladder)) + 
  geom_point(size=2, alpha=0.6, color="blue") +
  geom_smooth(method="lm", color="red",se=F) +
  labs(title="GPD per capita & Happiness level",
       x = "GDP per capita",
       y = "Happiness level") +
  theme_minimal()
  

world_happiness_data=NewData %>% 
  mutate (GDP_levels = cut (Log.GDP.per.capita, 
                            breaks=quantile(Log.GDP.per.capita, 
                                            probs=seq(0,1,0.25), na.rm=T),
                            include.lowest=T,
                            labels=c("Low GDP","Low-Middle GDP","High-Middle GDP","High GDP")))


world_happiness_data = world_happiness_data %>% filter(!is.na(GDP_levels))
world_happiness_data = world_happiness_data %>% filter(!is.na(Life.Ladder))
View(world_happiness_data)

ggplot(world_happiness_data, aes(x=GDP_levels, y=Life.Ladder, fill=GDP_levels)) +
      geom_boxplot() +
      labs(title="Differents levels of GDP",
           x = "GDP level",
           y = "Happiness level",
           fill = "GDP Level legend") +
      theme_minimal()

ggplot(world_happiness_data, aes(x=Log.GDP.per.capita, y=Life.Ladder))+
  geom_point(aes(color=GDP_levels), size=2,alpha=0.6) +
  geom_smooth(method="lm", color="red", se=F) +
  labs(title="GDP per capita & happiness score",
       x = "GDP per capita",
       y = "Happiness level")

--------------------------------------------------------------------------------

ggplot(na.omit(NewData),aes(x=Healthy.life.expectancy.at.birth, y=Life.Ladder)) + 
  geom_point(size=2, alpha=0.6, color="blue") +
  geom_smooth(method="lm", color="red",se=F) +
  labs(title="Healthy life expectancy at birth & Happiness level",
       x = "Healthy life expectancy at birth",
       y = "Happiness level")

world_happiness_data1=NewData %>%
  mutate (Healthy_level = cut(Healthy.life.expectancy.at.birth,
                              breaks=quantile(Healthy.life.expectancy.at.birth,
                                              probs = seq(0,1,0.25),na.rm=T),
                              include.lowest = T,
                              labels=c("Low healthy", "Low-Middle healthy", "High-Middle healthy", "High healthy")))

world_happiness_data1 = world_happiness_data1 %>% filter(!is.na(Healthy.life.expectancy.at.birth))
world_happiness_data1 = world_happiness_data1 %>% filter(!is.na(Life.Ladder))

ggplot(world_happiness_data1, aes(x=Healthy_level, y=Life.Ladder, fill=Healthy_level)) +
  geom_boxplot() +
  labs(title="Differents levels of health",
       x = "Healthy_level",
       y = "Happiness level",
       fill = "Healthy_level legend") +
  theme_minimal()

ggplot(na.omit(world_happiness_data), aes(x=Healthy.life.expectancy.at.birth, y=Life.Ladder))+
  geom_point(aes(color=GDP_levels), size=2,alpha=0.6) +
  geom_smooth(method="lm", color="red", se=F) +
  labs(title="Healthy life expectancy at birth & happiness score",
       x = "Healthy life expectancy at birth",
       y = "Happiness level")

--------------------------------------------------------------------------------

  ggplot(na.omit(NewData),aes(x=Social.support, y=Life.Ladder)) + 
  geom_point(size=2, alpha=0.6, color="blue") +
  geom_smooth(method="lm", color="red",se=F) +
  labs(title="Social support & Happiness level",
       x = "Social support",
       y = "Happiness level")

ggplot(NewData, aes(x = na.omit(Social.support))) +
  geom_histogram(binwidth = 0.1, fill = "orange", alpha = 0.6, color = "darkblue") + 
  labs(title = "Distribution of social support",
       x = "Social support score",
       y = "Frequency") + 
  theme_minimal()
--------------------------------------------------------------------------------
  
ggplot(na.omit(NewData),aes(x=Perceptions.of.corruption, y=Life.Ladder)) + 
  geom_point(size=2, alpha=0.6, color="blue") +
  geom_smooth(method="lm", color="red",se=F) +
  labs(title="Perceptions of corruption & Happiness level",
       x = "Perceptions of corruption",
       y = "Happiness level") +
  theme_minimal()

world_happiness_data3=NewData %>%
  mutate (Corruption_level = cut(Perceptions.of.corruption,
                             breaks=quantile(Perceptions.of.corruption,
                                             probs = seq(0,1,0.25),na.rm=T),
                             include.lowest = T,
                             labels=c("Low perception of corruption", "Low-Middle perception of corruption", "High-Middle perception of corruption", "High perception of corruption")))

world_happiness_data3 = world_happiness_data3 %>% filter(!is.na(Perceptions.of.corruption))
world_happiness_data3 = world_happiness_data3 %>% filter(!is.na(Life.Ladder))

ggplot(world_happiness_data3, aes(x=Corruption_level, y=Life.Ladder, fill=Corruption_level)) +
  geom_boxplot() +
  labs(title="Differents levels of corruption",
       x = "Perception of curruption",
       y = "Happiness level",
       fill = "Corruption level legend") +
  theme_minimal()

ggplot(na.omit(world_happiness_data), aes(x=Perceptions.of.corruption, y=Life.Ladder))+
  geom_point(aes(color=GDP_levels), size=2,alpha=0.6) +
  geom_smooth(method="lm", color="red", se=F) +
  labs(title="Healthy life expectancy at birth & happiness score",
       x = "Healthy life expectancy at birth",
       y = "Happiness level") +
  theme_minimal()

--------------------------------------------------------------------------------
ggplot(NewData, aes(x = na.omit(Life.Ladder))) +
  geom_histogram(binwidth = 0.25, fill = "orange", alpha = 0.6, color = "darkblue") + 
  labs(title = "Distribution of Happiness",
       x = "Happiness Score",
       y = "Frequency") + 
  theme_minimal()

ggplot(NewData, aes(x = na.omit(Life.Ladder))) +
  geom_histogram(binwidth = 0.5, fill = "orange", alpha = 0.6, color = "darkblue") + 
  labs(title = "Distribution of Happiness",
       x = "Happiness Score",
       y = "Frequency") + 
  theme_minimal()

ggplot(NewData, aes(x = na.omit(Life.Ladder))) +
  geom_histogram(binwidth = 0.75, fill = "orange", alpha = 0.6, color = "darkblue") + 
  labs(title = "Distribution of Happiness",
       x = "Happiness Score",
       y = "Frequency") + 
  theme_minimal()

ggplot(NewData, aes(x = na.omit(Life.Ladder))) +
  geom_histogram(binwidth = 1, fill = "orange", alpha = 0.6, color = "darkblue") + 
  labs(title = "Distribution of Happiness",
       x = "Happiness Score",
       y = "Frequency") + 
  theme_minimal()

ggplot(NewData, aes(x = na.omit(Life.Ladder))) +
  geom_density(fill = "green", alpha = 0.6, color = "darkblue") + 
  labs(title = "Density of Happiness Scores",
       x = "Happiness Score",
       y = "Density") + 
  ylim(0,0.5) +
  theme_minimal()

--------------------------------------------------------------------------------
top_5_nations = NewData %>%
  arrange(desc(Life.Ladder)) %>%
  head(5)

ggplot(top_5_nations, aes(x = reorder(Country.name, -Life.Ladder), y = Life.Ladder, fill = Country.name)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = Life.Ladder), vjust = -0.2) +  #
  theme_minimal() + 
  labs(title = "Happiness Levels by Country", 
       x = "Country", 
       y = "Happiness Level") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(top_5_nations)
--------------------------------------------------------------------------------

least_5_nations = NewData %>%
  arrange(Life.Ladder) %>%
  head(5)

ggplot(least_5_nations, aes(x = reorder(Country.name, -Life.Ladder), y = Life.Ladder, fill = Country.name)) + 
  geom_bar(stat = "identity") +  
  geom_text(aes(label = Life.Ladder), vjust = -0.2) + 
  theme_minimal() + 
  labs(title = "Happiness Levels by Country", 
       x = "Country", 
       y = "Happiness Level") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(least_5_nations)

--------------------------------------------------------------------------------

NewDataOrder = NewData %>%
  arrange(desc(Life.Ladder)) 

first_observation = head(NewDataOrder,1)
last_observation = tail(NewDataOrder, 1)

Best_and_worse = rbind(first_observation,last_observation)
print(Best_and_worse)

library(dplyr)

NewDataOrder2 <- NewData %>%
  filter(complete.cases(.)) %>%  
  arrange(desc(Life.Ladder))     

first_observation2 = head(NewDataOrder2,1)
last_observation2 = tail(NewDataOrder2,1)

Best_and_worse2 = rbind(first_observation2,last_observation2)
print(Best_and_worse2)

--------------------------------------------------------------------------------
  
modello = lm(formula= Life.Ladder ~ . -Country.name - year, data=NewData)  
summary(modello)

library(car)
vif(modello)
vif(modello)