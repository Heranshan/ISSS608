library(tidyverse)
library(ggthemes)

# Age vs Education
ggplot(data=participants, aes(x=educationLevel, y=age)) +
  geom_boxplot() +
  stat_summary(geom = "point",
               fun="mean",
               colour="red",
               size=4) +
  labs(y="Age", x="Education Level", title="Education Level versus Age") +
  theme_minimal()



#	Education level count
participants$educationLevel <- factor(participants$educationLevel, levels =  c("Low", "HighSchoolOrCollege", "Bachelors", "Graduate"))

participants %>% count(educationLevel) %>% 
  ggplot(aes(x=educationLevel, y=n)) + 
  geom_col() +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Education Level", y="Count", title="Education Level Count Plot") +
  theme_minimal() +
  scale_fill_brewer(palette='Blues')
  


#	haveKids count
participants %>% count(haveKids) %>% 
  ggplot(aes(x=haveKids, y=n)) + 
  geom_col() +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Have Kids?", y="Count", title="Have Kids Count Plot") +
  theme_minimal()

#	Age distribution
ggplot(participants, aes(x= age)) +
  geom_histogram(colour = "black", bins=20) +
  guides(fill = "none") +
  theme_minimal()

#	Household size chart vs kids


#	Joviality distribution
ggplot(participants, aes(x= joviality)) +
  geom_histogram(colour = "black", bins=20) +
  guides(fill = "none") +
  theme_minimal()

#	Interest group count
participants %>% count(interestGroup) %>% 
  ggplot(aes(x=reorder(interestGroup, n, fun=sum), y=n)) + 
  geom_col() +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Interest Group", y="Count", title="Interest Group Count Plot") +
  theme_minimal()

#	Education level vs household size (100%?)
participants$householdSize <- as.character(participants$householdSize)

######
participants %>%
  group_by(educationLevel, householdSize) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n),3)) %>%
  ggplot(aes(fill=householdSize, x=educationLevel, y=freq)) + 
    geom_col() +
    geom_text(aes(label = freq), size = 3, position = position_stack(vjust = 0.5)) +
    labs(x="Education Level", y="Frequency",
         title = "Education Level versus Household Size", fill = "Household Size") +
    theme_minimal()

# Education level vs kids
participants %>%
  group_by(educationLevel, haveKids) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n),3)) %>%
  ggplot(aes(fill=haveKids, x=educationLevel, y=freq)) + 
  geom_col() +
  geom_text(aes(label = freq), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Education Level", y="Frequency",
       title = "Education Level versus Kids", fill = "Have Kids?") +
  theme_minimal()


#	Education level vs interest group
ggplot(participants, aes(x=interestGroup, y=educationLevel)) + 
  geom_bar(position="fill") +
  theme_minimal()

participants %>%
  group_by(interestGroup, educationLevel) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=interestGroup, y=educationLevel, fill=n)) +
    geom_tile() +
    geom_text(aes(label = n)) +
    scale_fill_continuous(low = "red", high = "aquamarine") +
    theme_minimal()

#	Education level vs joviality
ggplot(data=participants, aes(x=educationLevel, y=joviality)) +
  geom_boxplot() +
  stat_summary(geom = "point",
               fun="mean",
               colour="red",
               size=4) +
  labs(y="Joviality", x="Education Level", title="Education Level versus Joviality") +
  theme_minimal()


#	Kids vs interest group

participants %>%
  group_by(interestGroup, haveKids) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n),3)) %>%
  ggplot(aes(fill=haveKids, x= interestGroup, y=freq)) + 
  geom_col() +
  geom_text(aes(label = freq), size = 3, position = position_stack(vjust = 0.5)) +
  labs(x="Interest Group", y="Frequency",
       title = "Interest Group versus Kids", fill = "Have Kids?") +
  theme_minimal()

participants %>%
  group_by(interestGroup, haveKids) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n),3)) %>%
  dplyr::arrange(haveKids, freq)

#	Kids vs joviality
ggplot(data=participants, aes(x=haveKids, y=joviality)) +
  geom_boxplot() +
  stat_summary(geom = "point",
               fun="mean",
               colour="red",
               size=4) +
  labs(x="Have Kids", y="Joviality",
       title = "Kids vs Joviality") +
  theme_minimal()

#	Age vs interest group
ggplot(data=participants, aes(x=reorder(interestGroup, joviality, fun=median), y=joviality)) +
  geom_boxplot() +
  stat_summary(geom = "point",
               fun="mean",
               colour="red",
               size=4) +
  labs(x="interestGroup", y="Joviality",
       title = "Interest Group vs Joviality") +
  theme_minimal()

#	Age vs joviality
ggplot(data = participants, aes(x=age, y=joviality)) +
  geom_tile()
