require(tidyverse)
require(ggrepel)

#Set repetition range and established limit
rep_range<-seq(20000,2000000,by=20000)
d2_limit_4<-2.059
limit_value<-4

#Set seed
set.seed(1)

#Create function to generate values and vectorize funtion
range.dist.data.range<-function(r,k){
  replicate(r,rnorm(k) %>% #replicate to rep range
              range() %>% #determinae range values
              diff() %>% #calculate diffrence of range values
              abs()) %>%  #make values absolute
    mean() #calculate d2_bar
}

range.dist.data.range<-Vectorize(range.dist.data.range) #Vectorize function

#Run replication for limit value
d2_bar_limit_4_list<-lapply(rep_range,range.dist.data.range,k=limit_value)

names(d2_bar_limit_4_list)<-rep_range

#Convert list to tibble
d2_bar_limit_4<-d2_bar_limit_4_list %>% 
  bind_rows()

#Tidy d2_bar_limit_4 to a dataframe
d2_bar_limit_4_tidy<-d2_bar_limit_4 %>% 
  pivot_longer(cols = everything(),
               names_to = "range.size",
               values_to = "d2_bar"
  ) %>% 
  as.data.frame() %>% 
  mutate_at(vars(range.size,d2_bar),as.numeric)

#Plot summarised values
d2_bar_limit_4_plot<-d2_bar_limit_4_tidy %>% 
  ggplot(aes(x=range.size,y=d2_bar))+
  geom_point()+
  geom_line(linetype="dashed",color="grey")+
  geom_smooth()+
  geom_hline(yintercept=d2_limit_4,color="red")+
  theme_bw()+
  labs(title=bquote(bar(d[2])~"Replication Limits"),
       subtitle = bquote("Replications required to determine an approximate"~ bar(d[2])~"for a subgroup size 4"))+
  xlab("Replications")+
  ylab(bquote(bar(d[2])))
d2_bar_limit_4_plot
