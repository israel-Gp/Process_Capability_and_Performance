require(tidyverse)
require(ggrepel)

#Set reps and range
reps<-500000
data_range_a<-1:50
data_range_b<-51:100
data_range_c<-101:150
data_range_d<-151:200
data_range<-c(data_range_a,data_range_b,data_range_c,data_range_d)

#Set seed
set.seed(1)

#Create function to generate values and vectorize funtion
range.dist.data<-function(k){
  replicate(reps,rnorm(k) %>% #replicate to reps
              range() %>% #determinae range values
              diff() %>% #calculate diffrence of range values
              abs()) #make values absolute
}

range.dist.data<-Vectorize(range.dist.data) #Vectorize function

#Generate data as list
d2_data_list_a<-lapply(data_range_a,range.dist.data)
d2_data_list_b<-lapply(data_range_b,range.dist.data)
d2_data_list_c<-lapply(data_range_c,range.dist.data)
d2_data_list_d<-lapply(data_range_d,range.dist.data)

d2_data_list<-c(d2_data_list_a,d2_data_list_b,d2_data_list_c,d2_data_list_d)

#Add range values as list names
names(d2_data_list)<-data_range

#Convert List to tibble
d2_data<-d2_data_list %>% 
  bind_rows()

#Tidy d2 Dataframe
d2_data_tidy<-d2_data %>% 
  pivot_longer(cols = everything(),
               names_to = "sample.size",
               values_to = "d2"
  ) %>% 
  group_by(sample.size) %>% 
  arrange(sample.size) %>% 
  as.data.frame() %>% 
  mutate_at(vars(sample.size),as.numeric)

#Summarize Results
d2_data_tidy_reg<-d2_data_tidy %>% 
  group_by(sample.size) %>% 
  summarise(d2_mean=mean(d2,na.rm = TRUE)) %>% 
  arrange(sample.size) %>% 
  filter(sample.size>1) %>% 
  mutate(d1_minitab=3.4873+0.0250141*sample.size-0.00009823*sample.size^2,
         group=case_when(sample.size<=50~"A",
                         (sample.size>50 & sample.size<=100)~"B",
                         (sample.size>100 & sample.size<=150)~"C",
                         (sample.size>150 & sample.size<=200)~"D",
         )) %>% 
  pivot_longer(cols = -c("sample.size","group"),
               names_to = "type",
               values_to = "value") %>% 
  as.data.frame()

#Plot
d2_data_tidy_reg %>% 
  ggplot(aes(x=sample.size,y=value,color=type,shape=type,label=sample.size))+
  geom_smooth()+
  geom_point()+
  #geom_text_repel()+
  theme_bw()

#Regression
reg_1<-lm(data = d2_data_tidy_reg %>% 
            filter(sample.size>1 & type=="d2_mean"),formula = value~sample.size)
summary(reg_1)