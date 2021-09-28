library(readr)
library(tidyverse)
library(lubridate)
library(corrr)
library(devtools)
library(ggpcp)

#removing all variables in environment
rm(list=ls())


#reading the dataset
marathon <- read_csv("marathon.csv")


#data exploration
summary(as.factor(df$`Category Position`))
summary(df)
summary(marathon)

class(df$`30K`)


##visualizing the whole dataset
visdat::vis_dat(df)
visdat::vis_dat(marathon)


##data manipulation and (converting hms to s)
df <- marathon %>% 
  dplyr::select(-c(`...1`, `First Name`,Surname, Share, YouTube, `Your Race Video`, Club, `Race Number`)) %>% 
  mutate(across(c(`10K`,`30K`, Halfway), ~as.numeric(seconds(.)))) %>% #hms to numeric
  mutate(across(c(`Gun Time`,`Chip Time`), ~as.numeric(hms(.)))) %>% #character to numeric
  mutate(across(c(Gender, Category), ~as.factor(.))) %>% 
  mutate(across(c(`Gender Position`, `Category Position`, `Stage Position...11`,
                  `Stage Position...13`, `Stage Position...15`, `Overall Position`,
                  `Chip Position`), ~as.numeric(.))) #character to numeric


##separating age from gender in Category variable
df <- df %>% 
  mutate(age= as.character(Category), 
         age=replace(age, age=="MS","25"), #bcz MS and FS dont have any number beside them, so we replace them by any number in that age group 
         age=replace(age, age=="FS","25"),
         age= parse_number(age))



##count runners for each gender and category and visualize it 1
##number of runners based on their age segment
df_merge <- data.frame(age= unique(df$age), age_category= c("20-34", "40-44", "35-39",
                                                            "45-49", "55-59","50-54","65-69","60-64",
                                                            "70-74","< 19", "75-79", "80-84",
                                                            "85 <"))
df <- df %>% #merging two datasets
  left_join(., df_merge)

df %>% 
  ggplot( mapping= aes(x = age_category, fill= Gender)) +
  geom_bar(alpha= 0.8,color="black") + theme_bw() + labs(x="Age", y = "Count") +
  ggtitle("Number of Runners by Age Group and Gender")+
  scale_fill_manual(values = c("deeppink","cyan2"),
                    guide = guide_legend(aes.override=list(color= c("deeppink","cyan2")))) +
  theme_light()


#visualize 2 (cntd)
#number of runners based on their age segment
ggplot(df, aes(x = age, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("deeppink","cyan2"),
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~Gender, ncol = 1) +
  coord_flip()


##correlation  matrix and pair wise plot matrix
df_corr <- df %>%
  dplyr::select(`Stage Position...11`, `Stage Position...13`,`Stage Position...15`, `Overall Position`) %>%
  correlate() %>%
  rearrange()
df_corr

df <- df %>% rename(Stage11= `Stage Position...11` , Stage13= `Stage Position...13`,
                    Stage15= `Stage Position...15`) 
df %>% 
  select(Stage11, Stage13,
         Stage15, `Overall Position`, Gender) %>%
  GGally::ggpairs(aes(color = Gender , alpha=0.7),
                  columns = c("Stage11", "Stage13"
                              ,"Stage15", "Overall Position")) +
  scale_colour_manual(values = c("deeppink","cyan2")) +
  ggtitle("Fig 3. Correlation Matrix")
  

df %>%
  select(Stage11, Stage13,
         Stage15, `Overall Position`, Gender) %>%
  GGally::ggpairs(aes(color = Gender)) +
  scale_colour_manual(values = c("purple","cyan4")) +
  scale_fill_manual(values = c("purple","cyan4"))


##jitter plot position by gender
ggplot(data = df, aes(x = Gender, y = `Overall Position`)) +
  geom_jitter(aes(color = Gender),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE) +
  scale_color_manual(values = c("deeppink","cyan2"))


##histogram overall position vs gender
ggplot(data = df, aes(x = `Overall Position`)) +
  geom_histogram(aes(fill = Gender), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("deeppink","cyan2"))


##histogram stage11 position vs gender
ggplot(data = df, aes(x = Stage11)) +
  geom_histogram(aes(fill = Gender), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorchid","cyan4"))


##histogram stage13 position vs gender
ggplot(data = df, aes(x = Stage13)) +
  geom_histogram(aes(fill = Gender), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorchid","cyan4"))


##histogram stage15 position vs gender
ggplot(data = df, aes(x = Stage15)) +
  geom_histogram(aes(fill = Gender), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorchid","cyan4"))


##percentage of DQ+DNF in each gender and age 
marathon %>% 
  filter(`Gender Position`== "DNF") %>% 
  ggplot(mapping= aes(x=Gender, fill=`Gender Position`)) + 
  geom_bar(position="fill")
  #ggplot(aes(x= Gender, fill= `Gender Position`))+
  #geom_bar()


df %>% 
  mutate(GP= marathon$`Gender Position`)%>% 
  filter(GP=="DNF") %>% 
  ggplot(mapping= aes(x=age, fill=Gender)) + 
  geom_bar(position="fill")
  #ggplot(aes(x= age, fill= Gender))+
  #geom_bar()


##DNF+DQ vs race segment(nothing interesting found)

#count(marathon$`Stage Position...11`=="DNF")
#length(marathon$`Stage Position...11`[which(marathon$`Stage Position...11`=="DQ")])
marathon_long_stages <- marathon %>% 
  select(`Stage Position...11`, `Stage Position...13`, `Stage Position...15`,
         `Overall Position`,`...1`) %>%
  pivot_longer(names_to = "Stage", 
               values_to = "value", 
               -`...1`)


marathon_long_stages %>% 
  filter(value== "DQ") %>% 
  ggplot(aes(x=Stage)) + 
  geom_bar()


## mean finish time vs max age/gender
df %>% 
  group_by(age, Gender) %>% 
  summarise(`Chip Time`= mean(`Chip Time`, na.rm=T)) %>% 
  ggplot(aes(x=age, y=`Chip Time`, color= Gender)) + 
  geom_line()+ geom_point()+
  scale_x_continuous(breaks=unique(df$age)) + theme_minimal() 



##taken time to finish each race segment and showing the variablity of it
seg_long <- df %>% 
  mutate(seg1 = `10K`,
         seg2 = Halfway - `10K`,
         seg3 = `30K` - Halfway,
         seg4= `Gun Time` - `30K`) %>% 
  select(seg1, seg2, seg3, seg4, Gender) %>% 
  pivot_longer(names_to = "Race_Segments", 
               values_to = "time", -Gender
               )
  
ggplot(data= seg_long, mapping = aes(y=time,x=Race_Segments)) + 
  geom_boxplot(varwidth=TRUE) + ggtitle("Taken time to finish each segment ")

ggplot(data=seg_long, aes(y=time,x=Race_Segments, fill= Gender)) + 
  geom_violin()+ theme_light()


#parallel coordinates
df %>% 
  left_join(., df_merge) %>% #merging two datasets
  ggplot(aes(color=age_category)) +
  geom_pcp(aes(vars=vars(`10K`,Halfway, `30K`, `Gun Time`)), method= "raw", alpha=0.4)+
  ggtitle("Fig 5. Mean Chip-Time by Gender and Age Group")

# +
#   scale_color_manual(values = c("deeppink","cyan2"),
#                      guide = guide_legend(aes.override=list(color= c("deeppink","cyan2"))))


#the difference between stage positions in different stages
pos_long <- df %>% 
  mutate(stg1 = Stage11,
         stg2 = Stage13 - Stage11,
         stg3 = Stage15 - Stage13,
         overall_stage= `Overall Position` - Stage15) %>% 
  select(stg1, stg2, stg3, overall_stage, Gender) %>% 
  pivot_longer(names_to = "Stage_Positions", 
               values_to = "Count", -Gender
  )

 ggplot(data= pos_long, mapping = aes(y=Count,x=Stage_Positions, color= Gender)) + 
  geom_boxplot(varwidth=TRUE) 

ggplot(data=pos_long, aes(y=Count,x=Stage_Positions, color = Gender)) + 
  geom_violin(fill="pink")+ theme_light()


