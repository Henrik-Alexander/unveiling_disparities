################################################
#    Decreasing fertility in Scandinavia       #
#     Henrik-Alexander Schubert                #
#Max-Planck Institute for Demographic Research #
################################################

# last edited by: Henrik-Alexander Schubert
# last edited on: 28th of September 2021

# Preperations
library(tidyverse)
library(readxl)
library(reshape2)
library(quadprog)
library(ggthemes)
library(patchwork)
library(scales)

# load the data
pop.educ <- read.csv("Raw data/population.finland1998-2015.csv") 


# clean the region names
pop.educ[, "Area"] <- pop.educ$Area %>% str_remove("SK\\d*") %>% str_remove("MA\\d") %>% str_remove(" ") 

# filter major regions
pop.educ<- pop.educ %>% filter(Area != "WHOLECOUNTRY" & Area != "MAINLAND FINLAND" & Area != "ÅLAND")


# reshape the dataset
pop.educ <- pivot_longer(pop.educ, 
             cols = 4:17,
             names_to = c("sex", "educ_level"),
             names_pattern = "(.*)_(.*)",
             values_to = "population" )



# rescale education variable
post_secondary <- c("Master.s.or.equivalent.level", "Bachelor.s.or.equivalent.level", "Doctoral.or.equivalent.level", "Short.cycle.tertiary.education", "Post.secondary.non.tertiary.education")
pop.educ <- pop.educ %>% mutate(educ_level = case_when(educ_level %in% post_secondary ~ "Tertiary",
                                           educ_level == "Basic.education" ~ "Low education",
                                           educ_level == "Upper.secondary.education" ~ "Highschool"))
pop.educ$educ_level 

# adjust population numbers to the new educ categories
pop.educ <- pop.educ %>% group_by(Year, Area, Age, sex, educ_level) %>% summarise(population = sum(population)) %>% ungroup()

# recode age groups
pop.educ <- pop.educ %>% mutate(age = case_when(Age == "15 - 19" ~ 15,
                                    Age == "20 - 24" ~ 20,
                                    Age == "25 - 29" ~ 25,
                                    Age == "30 - 34" ~ 30,
                                    Age == "35 - 39" ~ 35,
                                    Age == "40 - 44" ~ 40,
                                    Age == "45 - 49" ~ 45,
                                    Age == "50 - 54" ~ 50),
                                    upper = 4)
# factorize
ord <- c("Low education", "Highschool", "Tertiary")
pop.educ$educ_level <- factor(pop.educ$educ_level, levels = ord)

### Plot an age pyramide for Helsinki and Åhlands Skärgård
a <- pop.educ %>% filter(Year == 2014 & Area == "Helsinki" & Age >= 20) %>%  mutate(population = case_when(sex == "Females" ~ population,
                                                                                         sex == "Males" ~ -population)) %>% 
  ggplot(aes(x = age, y = population, fill = educ_level))+
  geom_bar(stat = "identity", size = 2)+
  coord_flip()+
  scale_fill_viridis_d(option = "E", name = "Education:", labels = c("low", "mid", "high"))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0, 'cm'))+
  geom_hline(yintercept = 0)+
  ggtitle("Helsinki")+
  scale_y_continuous(
    breaks = seq(-60000, 60000, 20000),
    labels = seq(-60000, 60000, 20000) %>% abs %>% paste %>% 
      str_replace("0000", "0K")
  ) +
  annotate(geom = "text", x = 55, y = -65000, label = "MALES", hjust = 0, vjust = 1, family = "serif")+
  annotate(geom = "text", x = 55, y = 65000, label = "FEMALES", hjust = 1, vjust = 1, family = "serif")



b <- pop.educ %>% filter(Year == 2015 & Area == "Ålands skärgård" & Age >= 20) %>%  mutate(population = case_when(sex == "Females" ~ population,
                                                                                          sex == "Males" ~ -population)) %>% 
  ggplot(aes(x = age, y = population, fill = educ_level))+
  geom_bar(stat = "identity",  size = 2)+
  coord_flip()+
  scale_fill_viridis_d(option = "E", name = "Education:", labels = c("low", "mid", "high"))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(legend.position = "bottom",
  legend.spacing.x = unit(0, 'cm'))+
  geom_hline(yintercept = 0)+
  ggtitle("Ålands skärgård")+
  scale_y_continuous(limits = c(-85, 85),
    breaks = seq(-80, 80, 20),
    labels = seq(-80, 80, 20)  %>% abs) +
  annotate(geom = "text", x = 55, y = -70, label = "MALES", hjust = 0, vjust = 1, family = "serif")+
  annotate(geom = "text", x = 55, y = 70, label = "FEMALES", hjust = 1, vjust = 1, family = "serif")

c <- a | b
c
ggsave(c, filename = "Visualisations/pop_structure_fin.pdf", height = 12, width = 15, unit = "cm")


####
pop <- pop.educ %>% filter(Year == 2014) %>%
  pivot_wider(names_from = "sex", values_from = "population") %>%  
  group_by(Age, educ_level, Area) %>% mutate(availability = (if_else(Females == 0, 2, log(Males/Females))))
                                               
  
  
a <- pop %>% filter(Area == "Helsinki" & age > 20) %>%  ggplot(aes(x = age, y = availability, fill = educ_level))+
  geom_col()+
  facet_wrap( ~ educ_level, ncol = 1)+
  scale_fill_viridis_d(option = "E",  guide = "none")+
  theme_few(base_size = 20, base_family = "serif")+
  ylab("log(Men-Women ratio)")+
  geom_hline(yintercept = 0)+
  ggtitle("Helsinki")+
  coord_flip()+
  xlab("Age")

b <- pop %>% filter(Area == "Ålands skärgård" & age > 20) %>%  ggplot(aes(x = age, y = availability, fill = educ_level))+
  geom_col()+
  facet_wrap( ~ educ_level, ncol = 1)+
  scale_fill_viridis_d(option = "E",  guide = "none")+
  theme_few(base_size = 20, base_family = "serif")+
  ylab("log(Men-Women ratio)")+
  geom_hline(yintercept = 0)+
  ggtitle("Ålands skärgård")+
  coord_flip()+
  xlab("Age")
a | b

ggsave(plot = last_plot(), filename = "Visualisations/shortages_helsinki.pdf")

# split the age groups
sex <- pop.educ$sex %>%  unique()
year <- pop.educ$Year %>%  unique()
states <- pop.educ$Area %>%  unique()
educ_level <- pop.educ$educ_level %>%  unique()

# create a template
template <- matrix(NA, ncol = 3, nrow = 40)
colnames(template) <- educ_level
row.names(template) <- 15:54
ages <- 15:54

#create an empty data frame for results
population_education <- data.frame(sex = NA, year = NA, region = NA, age = NA, educ_level = NA, population = NA)




# load the splitting program
source("Code/QOSplit.R")


# split age groups
for(i in sex){
  
  tmp <- pop.educ[pop.educ$sex == i, ]
  

for(j in year){
  # 
  tmp1 <- tmp[tmp$Year == j, ]
  
  for(s in states){
    
    tmp2 <- tmp1[tmp1$Area == s, ]
    
    for(e in educ_level){
      
      # get the population vector
      x <- tmp2[tmp2$educ_level == e, ] %>% select(population, age, upper) 
      
      
      # run the alogrithm
      x <- QOSplit(x$population, x$age, x$upper)
      
      # assign to the template
      template[, paste(e)] <- x %>% filter(Age %in% ages)
      
    }
    
    # create a data frame
    df <- melt(template)
    
    # name the columns
    names(df) <- c("age", "educ_level", "population")
    
    # create new columns
    df %>% mutate(region = s, year = j, sex = i)
    
    # cbind
    population_education <- rbind(df, population_education)
    
      
    }
    
  }
}


######################################## END ################################################

QOSplit(x, age.groups, intervals)
