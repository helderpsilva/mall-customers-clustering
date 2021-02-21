# Title:   Mall Customers - Clustering
# Authors: Carla M. Lemos e Hélder P. Silva
# Course:  Data Mining I
# Date:    20/11/2020


# Install packages
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("factoextra")
# install.packages("clusterSim")
# install.packages("GGally")
# install.packages("RColorBrewer")
# install.packages("ggplotify")
# install.packages("hrbrthemes")
# install.packages("dendextend")
# install.packages("egg")

# Import packages
library("tidyverse")
library("dplyr")
library("gridExtra")
library("plyr")
library('factoextra')
library("clusterSim")
library("GGally")
library("RColorBrewer")
library("ggplotify")
library("hrbrthemes")
library("dendextend")
library("egg")
library("ggplot2")
library("corrplot")


# Set options
options(warn=-1)
options(tibble.width = Inf)


# Set working directory
setwd("path/to/directory")
 
# Import data
mall_customers <- read_csv2("Mall_Customers.csv")

head(mall_customers)                                                  # Visualize data
names(mall_customers)                                                 # Check labels
str(mall_customers)                                                   # Check variables
mall_customers$Gender <- as.factor(mall_customers$Gender)             # Convert characters to factors
colnames(mall_customers) <- c('id','Gender','Age','Income','Score')   # Rename the variables
str(mall_customers)                                                   # Confirm changes


# Missing values
mssing_values<-data.frame(apply(mall_customers,2,function(x){sum(is.na(x))}))
names(mssing_values)[1]='Valores omissos'
mssing_values

# Summary statistics
mall_customers %>% 
  summary()

# Select custom colors
gender_colors <- c('#f04544', '#5e676e')

# Customers by gender
mall_customers %>% 
  group_by(Gender) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(perc = count/sum(count)*100) %>% 
  ggplot(aes(x    = Gender,
             y    = perc,
             fill = Gender)) +
  geom_bar(stat        = "identity",
           alpha       = 0.7,
           show.legend = FALSE,
           color       = 'black') +
  scale_fill_manual(values = gender_colors) +
  geom_text(aes(label = paste0(perc, "%")),
            vjust    = -0.5,
            color    = "black",
            position = position_dodge(0.9),
            size     = 3.5) +
  ylim(0, 60) +
  labs(title = "Distribuição de clientes por Género",
       x     = "",
       y     = "Percentagem")+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Table gender income
ddply(mall_customers,
      'Gender',
      summarise,
      min = min(Income),
      mean = round(mean(Income),1),
      median = median(Income),
      max = max(Income))

# Table gender score
ddply(mall_customers,
      'Gender',
      summarise,
      min = min(Score),
      mean = round(mean(Score),1),
      median = median(Score),
      max = max(Score))

# Table gender age
ddply(mall_customers,
      'Gender',
      summarise,
      min = min(Age),
      mean = round(mean(Age),1),
      median = median(Age),
      max = max(Age))

# ------------------------------------------------------------------------------

# Histogram (age)
ggplot(mall_customers, aes(Age)) + 
  geom_histogram(aes(y=..density..),
                 fill="white", 
                 color="black",
                 binwidth = 2) +
  scale_x_continuous(breaks = seq(15, 75, 10)) + 
  labs(x = "Idade", y = "Densidade") +
  ggtitle("Distribuição de idades dos consumidores") +
  geom_density(col='#f04544', 
               alpha=0.4)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

# Histogram (income)
ggplot(mall_customers, aes(Income)) + 
  geom_histogram(aes(y=..density..),
                 fill="white", 
                 color="black",
                 binwidth = 6) +
  scale_x_continuous(breaks = seq(0, 140, 20)) + 
  labs(x = "Salário", y = "Density") +
  ggtitle("Distribuição dos valores de salário") +
  geom_density(col= '#f04544', 
               alpha=0.4)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))
  
# Histogram (score)
ggplot(mall_customers, aes(Score)) + 
  geom_histogram(aes(y=..density..),
                 fill="white", 
                 color="black",
                 binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 101, 10)) + 
  labs(x = "Score", y = "Densidade") +
  ggtitle("Distribuição de SpendingScore") +
  geom_density(col='#f04544', 
               alpha=0.4)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))


# ------------------------------------------------------------------------------

# Correlation
corr_matrix <-data.frame(round(cor(mall_customers[,c(3,4,5)]),2))           # Correlation matrix
corr_matrix

male_customer <-mall_customers[mall_customers$Gender=="Male",]              # Filter male
corr_matrix_male <- data.frame(round(cor(male_customer[,c(3,4,5)]),2))      # Correlation matrix male
corr_matrix_male

female_customers <-mall_customers[mall_customers$Gender=="Female",]         # Filtering female
corr_matrix_female <-data.frame(round(cor(female_customers[,c(3,4,5)]),2))  # Correlation matrix female
corr_matrix_female

# Pairplot
pairs(mall_customers[,c(3,4,5)], col = c("grey30", "grey40"), main='Pairplot')
pairs(corr_matrix_male[,c(3,4,5)], col = c("grey30", "grey40"), main='Pairplot')
pairs(corr_matrix_female[,c(3,4,5)], col = c("grey30", "grey40"), main='Pairplot')

rm(corr_matrix)
rm(corr_matrix_male, male_customer)
rm(female_customers, corr_matrix_female)


# ------------------------------------------------------------------------------

# Scatterplot - correlation between Income Spending Score
ggplot(mall_customers, aes(x=Income, y=Score)) + geom_point() +
  labs(x = "Salário Anual", y = "SpendingScore") +
  scale_x_continuous(breaks = seq(0, 160, 20)) + 
  scale_y_continuous(breaks = seq(0, 120, 20))+
  theme_light()

# ------------------------------------------------------------------------------

# Boxplot Analysis
ggplot(mall_customers, aes(x=Age, y=Gender))+
  geom_boxplot(aes(fill=Gender), alpha=0.6)+
  xlab("Idade")+
  ylab("Género")+
  ggtitle("Boxplot Género-Idade")+
  scale_fill_manual(values=gender_colors)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mall_customers, aes(x=Income, y=Gender))+
  geom_boxplot(aes(fill=Gender), alpha=0.6)+
  xlab("Salário Anual")+
  ylab("Género")+
  ggtitle("Boxplot Género-Salário")+
  scale_fill_manual(values=gender_colors)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mall_customers, aes(x=Score, y=Gender))+
  geom_boxplot(aes(fill=Gender), alpha=0.6)+
  xlab("Score")+
  ylab("Género")+
  ggtitle("Boxplot Género-Score")+
  scale_fill_manual(values=gender_colors)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))


boxplot(mall_customers$Income~mall_customers$Gender)
mall_customers[which(mall_customers$Income %in% 
                       boxplot(mall_customers$Income~mall_customers$Gender)$out),]  # Check outliers

# ------------------------------------------------------------------------------

# Model
# Data Preparation
customer_prep <- mall_customers[2:5]

customer_prep$Gender  <- if_else(
  mall_customers$Gender == 'Female', 0, 1)          # (F=0, M=1)
customer_prep[2:4]    <- scale(customer_prep[2:4])  # Scaling variables (Age, Income, Score)
customer_prep %>% head()                            # Visualize data


seg_age_income  <- customer_prep[2:3]               # Filter by Age and Income
seg_age_score   <- customer_prep[, c(2,4)]          # Filter by Age and Score
seg_in_score    <- customer_prep[3:4]               # Filter by Income and Score

# ------------------------------------------------------------------------------

# Hierarquical clustering
# Dendogram
dendogram_plot <- function(data_, K_,line_) {
  dendogram <- data_ %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram()
  
  par(mar = c(0,0,1,0))
  dendogram  %>%
    set("labels", "") %>%
    set("branches_k_color",
        k = K_) %>%
    plot(axes = FALSE,
         main = paste('K =', K_, sep = " "))
  
  abline(h = line_, lty = 3)
  
}


par(mfrow=c(1,2))

dendogram_plot(seg_age_income,3,3.5)    # Age and Income - K=3
dendogram_plot(seg_age_income,4,3)      # Age and Income - K=4

dendogram_plot(seg_age_score,3,3.2)     # Age and Score - K=3
dendogram_plot(seg_age_score,5,2.5)     # Age and Score - K=5

dendogram_plot(seg_in_score,4,3.3)      # Income and Score - K=4
dendogram_plot(seg_in_score,5,2.72)     # Income and Score - K=5

dendogram_plot(customer_prep,4,4.42)    # Income, Age, Score and Gender - K=4
dendogram_plot(customer_prep,5,3.5)     # Income, Age, Score and Gender - K=5

par(mfrow=c(1,1))

# ------------------------------------------------------------------------------

# K-Means
# Clustering (K-means)
clustering_plot <- function(data_, center_) {
  
  # K Optimal.
  cluster_num <- fviz_nbclust(data_,
                              kmeans,
                              method = "wss",
                              k.max = 8) +
    geom_vline(xintercept = center_,
               linetype = 2,
               color = 'blue') +
    theme_light() +
    labs(title = '',
         subtitle = 'Optimal K.',
         x = 'Cluster',
         y = '')

  
  k_means <- kmeans(data_,
                    centers = center_,
                    nstart = 50)
  
  # Graphics
  cluster_plot <- 
    fviz_cluster(object =  k_means,
                 data = data_,
                 show.clust.cent = TRUE,
                 ellipse.type = "euclid",
                 star.plot = TRUE,
                 labelsize = 0) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = '',
         subtitle = 'Cluster Plot') +
    theme_light() +
    theme(legend.position = "none")
  

  
  df_number <- enframe(k_means$size,
                       name = "Cluster",
                       value = "Count")
  
  
  cluster_size <- ggplot(df_number,
                         aes(x = as.factor(Cluster),
                             y = Count,
                             fill = as.factor(Cluster))) +
    geom_bar(stat = "identity",
             show.legend = FALSE) +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = '',
         subtitle = 'Customers per cluster',
         x = 'Cluster',
         y = '') +
    theme_light()
  
  grid.arrange(cluster_plot, cluster_size, cluster_num, layout_matrix = rbind(c(1,1,2),c(1,1,3)))
  
  # Results
  segment <- mall_customers %>% 
    cbind(cluster =  k_means$cluster)

}


seg_age_income_cluster <- clustering_plot(seg_age_income, center_ = 3) # Age and Income
seg_age_score_cluster <- clustering_plot(seg_age_score, center_ = 3)   # Age and Score
seg_in_score_cluster <- clustering_plot(seg_in_score, center_ = 5)     # Income and Score
customer_prep_cluster <- clustering_plot(customer_prep,center_ = 4)    # Income, Age, Score and Gender

# ------------------------------------------------------------------------------

# Cluster description
cluster_description <- function(data_){
  ddply(data_[2:6],
        c('cluster', 'Gender'),
        summarise,
        mean_age = round(mean(Age),1),
        age_range = paste(min(Age), max(Age), sep="-"),
        mean_income = round(mean(Income),1),
        income_range = paste(min(Income), max(Income), sep="-"),
        score_range = paste(min(Score), max(Score), sep="-"))
}

cluster_description(seg_age_income_cluster) # Age and Income
cluster_description(seg_age_score_cluster)  # Age and Score
cluster_description(seg_in_score_cluster)   # Income and Score
cluster_description(customer_prep_cluster)  # Income, Age, Score and Gender



counting_genders(seg_age_income_cluster) # Age and Income
counting_genders(seg_age_score_cluster)  # Age and Score
counting_genders(seg_in_score_cluster)   # Income and Score
counting_genders(customer_prep_cluster)  # Income, Age, Score and Gender