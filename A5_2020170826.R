library(dplyr)
education<-read.csv("/Users/apple/Downloads/xAPI-Edu-Data.csv",stringsAsFactors = FALSE, header = TRUE )
View(education)
head(education)
str(education)
summary(education)

library(ggplot2)
library(XML)
library(reshape2)
library(plyr)
library(forecast)
library(ggpubr)
library(likert)
library(scales)
library(ggrepel)


#1. Identifying the population
numM<-education %>% filter(gender == "M") %>% nrow()
numF<-education %>% filter(gender == "F") %>% nrow()
pop<-data.frame(
  Gender=c("Male","Female"),
  value=c(numM,numF)
)
population<-ggplot(pop,aes(x="",y=value,fill=Gender))+
  geom_bar(stat="identity",width = 1,color="white")+
  geom_text(aes(label=paste(round(value/sum(value)*100,1),"%")),
            position = position_stack(vjust = 0.5))+
  labs(title = "Percentage of Gender",
       caption = "Source: education")+
  coord_polar("y",start = 0)+
  theme_void()
population

#2 Number of Stage ID by bar chart 
Stage<-sort(table(education$StageID), decreasing = TRUE)
StageID<-data.frame(Stage)
StageID$fraction = StageID$Freq/sum(StageID$Freq)
StageID$ymax = cumsum(StageID$fraction)
StageID$ymin = c(0, head(StageID$ymax, n=-1))
StageID$labelPosition<-(StageID$ymax + StageID$ymin)/2
StageID$label<-paste0(StageID$Var1, "\n value:", StageID$Freq)
ggplot(StageID, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1))+
  geom_rect()+
  labs(title="Number of Students in each Stage ID")+
  geom_label(x=3.5, aes(y=labelPosition, label=label, size=10))+
  scale_fill_brewer(palette=4)+
  coord_polar(theta = "y")+
  xlim(c(2,4))+
  theme_void()+
  theme(legend.position = "none")

#3. Identifying Nationality of Students by bar chart
# and estimate the number of students by their nationality and gender
theme_set(theme_classic()) #set theme

Nation <- ggplot(education, aes(NationalITy)) 
Nation + geom_bar(aes(fill=gender), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Number of Students Depending on Nationality", 
       subtitle="by Gender",
       x = "Nationality",
       y = "Number of Students")+
  coord_cartesian(ylim=c(0,200))
                  
#4 How many students Raised Hands in each interval 
a1<-ggplot(education, aes(x = raisedhands))+geom_histogram(fill="skyblue", color = "black")+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title="Number of Student in each interval of Raised Hands",
       x = "Frequency of Raised Hand",
       y = "Number of Students")

a2<-ggplot(education, aes(x = Discussion))+geom_histogram(fill="lightgreen", color = "black")+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title="Number of Student in each interval of Discussion",
       x = "Frequency of Discussion",
       y = "Number of Students")

a3<-ggplot(education, aes(x = AnnouncementsView))+geom_histogram(fill="skyblue", color = "black")+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title="Number of Student in each interval of Announcements View",
       x = "Frequency of AnnouncementsView",
       y = "Number of Students")

a4<-ggplot(education, aes(x = VisITedResources))+geom_histogram(fill="lightgreen", color = "black")+
  scale_x_continuous(breaks = seq(0,100,5))+
  labs(title="Number of Student in each interval of Visited Resources",
       x = "Frequency of Visited Resources",
       y = "Number of Students")

Comparisonfigure1<-ggarrange(a1,a2,a3,a4,
                             labels=c("A","B","C","D"),
                             ncol =2, nrow = 2)
Comparisonfigure1

#4.  Identifying the number of Subject that Students take and estimate students' level
#H is High level: interval includes values from 0 to 69
#M is Middle level: interval includes values from 70 to 89
#H is High level: interval includes values from 90-100
theme_set(theme_get())#set theme

Subject <- ggplot(education, aes(Topic)) 
Subject + geom_bar(aes(fill=Class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Subject that Students Take", 
       subtitle="arranged by Class",
       x = "Subject",
       y = "Number of Students")+
  coord_cartesian(ylim=c(0,100))

#Violin Plot on top3 subjects
theme_set(theme_bw())
Topsubject<-education %>% select(Topic,VisITedResources) %>% 
  filter(Topic == "IT" | Topic == "French" | Topic =="Arabic")
Topsubject<-ggplot(Topsubject, aes(Topic, VisITedResources, fill=Topic))
Topsubject + geom_violin()+
  labs(title = "The Visited Resources of Top 3 Subject",
       subtitle = "Arabic, French, IT",
       x = "Subject",
       y = "Frequency of Visited Resources")

#5 The relationship between raised hands and Gender with Density Plot
density1<-ggplot(education, aes(raisedhands, color=gender))+
  geom_density()+
  labs(title="The Relationship between Raised Hands and Gender",
       subtitle="with Density Plot",
       x = "Frequency of Raised Hands")
density1

#5 The relationship between raised hands and Topic with Density Plot

density2<-ggplot(education, aes(raisedhands, color=Topic))+
  geom_density()+
  labs(title="The Relationship between Raised Hands and Topic",
       subtitle="with Density Plot",
       x = "Frequency of Raised Hands")
density2


#6 Identify the activeness of students in class time depending on their level of class

#Relation with raised hands
theme_set(theme_bw())
b<-ggplot(education, aes(x=Class, y=raisedhands))
b1<-b +geom_boxplot(varwidth = T, fill = "plum")+
  stat_summary(fun = mean, geom = "point", shape=20, size=3, 
               color="red",fill="red")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title = "The Relaionship between the Number of Raised hands and Class",
       subtitle = "by Class ",
       caption = "Source: education",
       x = "Class",
       y = "Number of Raisedhands")+
  scale_y_continuous(breaks = seq(0,100,10))
b1

#Relation with Visited Resources
b<-ggplot(education, aes(x=Class, y=VisITedResources))
b2<-b +geom_boxplot(varwidth = T, fill = "skyblue")+
  stat_summary(fun = mean, geom = "point", shape=20, size=3, 
               color="red",fill="red")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title = "The Relaionship between the Number of Visited Resources and Class",
       caption = "Source: education",
       x = "Class",
       y = "Number of Visited Resources")+
  scale_y_continuous(breaks = seq(0,100,10))
b2

#Relation with Discussion
b<-ggplot(education, aes(x=Class, y=Discussion))
b3<-b +geom_boxplot(varwidth = T, fill = "lightgreen")+
  stat_summary(fun = mean, geom = "point", shape=20, size=3, 
               color="red",fill="red")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title = "The Relaionship between the Number of Discussion and Class",
       caption = "Source: education",
       x = "Class",
       y = "Number of Discussion")+
  scale_y_continuous(breaks = seq(0,100,10)) 
b3

#Relation with Announcements View Review
b<-ggplot(education, aes(x=Class, y=AnnouncementsView))
b4<-b +geom_boxplot(varwidth = T, fill = "orange")+
  stat_summary(fun = mean, geom = "point", shape=20, size=3, 
               color="red",fill="red")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title = "The Relaionship between the Number of Announcements View and Class",
       caption = "Source: education",
       x = "Class",
       y = "Number of AnnouncementsView")+
  scale_y_continuous(breaks = seq(0,100,10)) 
b4

#Combine
Comparisonfigure2<-ggarrange(b1,b2,b3,b4,
                  labels=c("A","B","C","D"),
                  ncol =2, nrow = 2)
Comparisonfigure2


#7 The relationship between raised hands and discussion by Comparing 
#each Stage ID and Student Absence Days

theme_set(theme_bw())
line1<-ggplot(education, aes(raisedhands,Discussion))+
  labs(title = "The Relationship between Raised Hands and Discussion",
       subtitle = "in Each StageID",
       x = "Frequency of Raised Hand",
       y = "Frequency of Discussion")
line1 + geom_smooth(aes(col=StageID), method="lm", se=F)

line2<-ggplot(education, aes(raisedhands,Discussion))+
  labs(title = "The Relationship between Raised Hands and Discussion",
       subtitle = "in Each Student Absence Days",
       x = "Frequency of Raised Hand",
       y = "Frequency of Discussion")
line2 + geom_smooth(aes(col=StudentAbsenceDays), method="lm", se=F)

#8 Parents' Satisfaction to School
Good<-education %>% filter(ParentschoolSatisfaction == "Good") %>% nrow()
Bad<-education %>% filter(ParentschoolSatisfaction == "Bad") %>% nrow()
Satisfaction<-data.frame(count = c(Good, Bad),
                         Satis = c("Good","Bad"))
theme_set(theme_bw())                         
ggplot(Satisfaction, aes(x=Satis,y=count))+
  geom_point(size = 5, color ="red", fill=alpha("orange", 0.3),
             alpha = 0.7, shape=21, stroke=2)+
  geom_segment(aes(x=Satis, xend=Satis,
                   y = 0,
                   yend = count))+
  labs(title="Parents' Satisfaction to School",
       subtitle = "with Lollipop plot",
       x = "Satisfaction",
       y ="Count")
  
#9 Parents' Satisfaction to School Depending on Class
HGood<-education %>% select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Good"&Class == "H") %>% nrow()
HBad<-education %>% select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Bad"&Class == "H") %>% nrow()
MGood<-education %>%select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Good"&Class == "M") %>% nrow()
MBad<-education %>%select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Bad"&Class == "M") %>% nrow()
LGood<-education %>%select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Good"&Class == "L") %>% nrow()
LBad<-education %>%select(ParentschoolSatisfaction,Class) %>% 
  filter(ParentschoolSatisfaction == "Bad"&Class == "L") %>% nrow()

SatisfactionbyClass<-data.frame(Class = c("H", "M", "L"),
                                Bads = c(HBad, MBad, LBad),
                                Goods = c(HGood, MGood, LGood))
#The change of Goods in Class
H.S.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "H"&Semester=="S") %>% nrow()
H.F.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "H"&Semester=="F") %>% nrow()
M.S.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "M"&Semester=="S") %>% nrow()
M.F.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "M"&Semester=="F") %>% nrow()
L.S.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "L"&Semester=="S") %>% nrow()
L.F.G<-education %>%
  filter(ParentschoolSatisfaction == "Good"&Class == "L"&Semester=="F") %>% nrow()
GoodsChange<-data.frame(class=c("H","H","M","M","L","L"),
                        Semester=c("S","F","S","F","S","F"),
                        Number=c(H.S.G,H.F.G,M.S.G,M.F.G,L.S.G,L.F.G))
GSatisfaction<-ggplot(GoodsChange,aes(x=Semester, y=Number, group=class))+
  geom_line(aes(color=class, alpha=1), size =2)+
  geom_point(aes(color=class, alpha=1), size = 4)+
  labs(title = "Satisfaction Change between Semester in Each Class",
       subtitle = "Satisfaction: Good",
       y="Number of Parents Satisfied with School")

#The change of Bads in Class
H.S.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "H"&Semester=="S") %>% nrow()
H.F.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "H"&Semester=="F") %>% nrow()
M.S.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "M"&Semester=="S") %>% nrow()
M.F.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "M"&Semester=="F") %>% nrow()
L.S.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "L"&Semester=="S") %>% nrow()
L.F.B<-education %>%
  filter(ParentschoolSatisfaction == "Bad"&Class == "L"&Semester=="F") %>% nrow()
BadsChange<-data.frame(class=c("H","H","M","M","L","L"),
                        Semester=c("S","F","S","F","S","F"),
                        Number=c(H.S.B,H.F.B,M.S.B,M.F.B,L.S.B,L.F.B))
BSatisfaction<-ggplot(BadsChange,aes(x=Semester, y=Number, group=class))+
  geom_line(aes(color=class, alpha=1), size =2)+
  geom_point(aes(color=class, alpha=1), size = 4)+
  labs(title = "Satisfaction Change between Semester in Each Class",
       subtitle = "Satisfaction: Bad",
       y="Number of Parents Unsatisfied with School")

Comparisonfigure3<-ggarrange(GSatisfaction,BSatisfaction,
                             labels=c("A","B"),
                             ncol =2, nrow = 1)
Comparisonfigure3

