library(readr)

characters = read_csv("data/characters.csv")
View(characters)

episodes = read_csv("data/episodes.csv")
View(episodes)

scenes = read_csv("data/scenes.csv")
View(scenes)

appearances = read_csv("data/appearances.csv")
View(appearances)

dim(appearances)
dim(scenes)
dim(episodes)
dim(characters)

str(appearances)
str(scenes)
str(episodes)
str(characters)

summary(appearances)
summary(scenes)
summary(episodes)
summary(characters)

intersect(names(appearances),names(scenes))
intersect(names(appearances),names(episodes))
intersect(names(appearances),names(characters))
intersect(names(scenes),names(episodes))
intersect(names(scenes),names(characters))
intersect(names(episodes),names(characters))

nb_death_all = sum(scenes$nbdeath)
nb_death_all

nb_death_s1 = 0 
for ( i in 1:10) { 
  
nb_death_s1 = nb_death_s1 + sum(scenes$nbdeath[scenes$episodeId==i])
  
}
nb_death_s1

sort(table(characters$killedBy),decreasing = TRUE)[1:5]

scenes[which.max(scenes$duration),]


library(dplyr)

head(arrange(scenes, desc(scenes$duration)),1)


long_scenes_charac = scenes %>% arrange(desc(duration)) %>% head(1) %>% left_join(appearances)
long_scenes_charac["name"]

by_loc = scenes %>% group_by(scenes$location)  %>%  summarise(nb = n())   %>%  arrange(desc(nb)) %>% head(1)
# by_loc  %>%  summarise(nb = n())   %>%  arrange(desc(nb))
scenes %>% filter(subLocation == "King's Landing") %>% group_by(subLocation) %>% summarise(nb = n())

scenes %>% group_by(subLocation) %>%  summarise(nb = sum(nbdeath)) %>%  arrange(desc(nb)) %>% head(1) 

scenes %>% left_join(appearances)%>% filter(name == "Jon Snow") %>% group_by(episodeId) %>% summarise(total_d = sum(duration)) %>%  arrange(desc(total_d))

appearances %>% left_join(scenes)  %>% group_by(name) %>% summarise(screenTime=sum(duration)) %>% filter(screenTime>30*60) %>% nrow()

appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% filter(name.x!=name.y) %>% group_by(name.x,name.y) %>% summarise(nbs=n()) %>% arrange(desc(nbs)) %>% head(1)

appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% left_join(scenes) %>%  filter(name.x!=name.y) %>% group_by(name.x,name.y) %>% summarize(total_d=sum(duration)) %>%  arrange(desc(total_d))

library(tidyr)

duration_location_character = scenes %>% left_join(appearances) %>% group_by(name,location) %>% summarize(duration=sum(duration))
duration_location_character

duration_large = duration_location_character %>% 
  pivot_wider(values_from = duration,names_from = location,values_fill = c("duration"=0))
View(duration_large)

X=as.matrix(duration_large[,-1])
Xs=X[rowSums(X)>60*60,]
Xs
Xns=Xs/rowSums(Xs)
Xns
rownames(Xns)=duration_large$name[rowSums(X)>60*60]
rownames(Xns)

hc=hclust(dist(Xns,method="manhattan"))
plot(hc,main = "Clustering des personnages principaux suivant leur lieux de présences",xlab = "")

library(ggplot2)
jstime = appearances %>% filter(name=="Jon Snow") %>% 
  left_join(scenes) %>% 
  group_by(episodeId) %>% 
  summarise(time=sum(duration)) %>% aggregate(desc())

ggplot(jstime)+ geom_line(aes(x=episodeId,y=time))+theme_bw()+xlab("?pisode")+ylab("temps")+ggtitle("Temps de pr?sence par ?pisode de John Snow")


ggplot(jstime)+ geom_area(aes(x=episodeId,y=time))+theme_bw()+xlab("?pisode")+ylab("temps")

deaths = scenes %>% select(nbdeath,duration,location,episodeId) %>% 
  mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
deaths

season_t = episodes  %>% mutate(t=cumsum(total_duration)) %>% 
  filter(episodeNum==1)
season_t

ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
  scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                     labels =   paste("Saison",1:8),)+
  scale_y_continuous("Nombre de morts cumul?s", expand=c(0,0))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  ggtitle("Evolution du nombre de mort au cours du temps")


episode_summary = episodes %>% left_join(scenes) %>% select(episodeTitle,episodeNum,seasonNum,sceneId,duration,nbdeath) %>% group_by(episodeTitle,episodeNum,seasonNum) %>% summarise(scene_count=n(),nb_death_ep=sum(nbdeath),longuest_scene= max(duration)) 
episode_summary

ggplot(episode_summary,aes(x=scene_count, y=longuest_scene))+ geom_point() 

labels = episode_summary %>% filter(longuest_scene>400|scene_count>200)
ggplot(episode_summary,aes(x=scene_count,y=longuest_scene,col=factor(seasonNum)))+
  geom_point(aes(size=nb_death_ep))+
  geom_text(data=labels,aes(label=episodeTitle),vjust=-0.6)+
  scale_x_continuous("Nombre de sc?ne",limits = c(0,280))+
  scale_y_continuous("Dur?e de la sc?ne la plus longue",limits = c(100,800))+
  scale_color_brewer("Saison",palette ="Spectral")+
  guides(colour = "legend", size = "legend")+
  theme_bw()


episode_summary
ggplot(scenes %>% left_join(episodes))+geom_boxplot(aes(x=episodeId,y=duration))

labels = scenes %>% filter(duration>400)
ggplot(scenes %>% left_join(episodes))+
  geom_boxplot(aes(x=factor(episodeId),y=duration,fill=factor(seasonNum)))+
  geom_text(data=labels ,aes(x=factor(episodeId),y=duration,label=subLocation),hjust = "right",vjust="top")+
  scale_x_discrete("N? ?pisode",as.character(seq(1,73, by=5)))+
  scale_fill_brewer(palette="Spectral",guide="none")+
  ylab("Dur?e des sc?nes (min)")+
  ggtitle("R?partition des dur?es des sc?nes par ?pisodes")+
  theme_bw()

factor(episodes$seasonNum)


apparition = appearances %>% left_join(scenes) %>% left_join(episodes) %>% select(name, sceneId, duration,episodeId,seasonNum) %>% group_by(name,seasonNum) %>% summarise(total_duration_ps=sum(duration))
apparition = apparition %>% filter(total_duration_ps > 3600)
ggplot(apparition, aes(fill=factor(seasonNum), x=name, y=total_duration_ps)) + 
  geom_bar(position="dodge", stat="identity")  

