

# Packages

library(tidyverse)
library(patchwork)
library(ggsci)


# read data
survey_data <- read_csv("https://raw.githubusercontent.com/emdelponte/paper-coffee-rust-Ethiopia/master/data/survey_clean.csv") %>% 
  select(-c(farm, region, zone, district, lon, lat))

# prepare the data
survey_data_long = survey_data  %>%
  pivot_longer(cols = c(inc, sev2),
               names_to = "metric",
               values_to = "rate") %>%
  mutate(
    farm_management = factor(
      farm_management,
      levels = c("Unmanaged", "Minimal", "Moderate", "Intensive")
    ),
    metric = factor(
      metric,
      levels = c("inc", "sev2"),
      labels = c("Incidence", "Severity")
    )
  )


# plot 1 ------------------------------------------------------------------



ggplot(survey_data_long, aes(x= farm_management, y = rate))+
  
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(aes(color = farm_management),
              size = 3, width = 0.2, alpha = .5)+
  
  facet_wrap(~metric, ncol = 1)+
  scale_color_npg()+
  
  labs(title = "Disease distribution across farm mannagement", 
       x = NULL,
       y = "Percent (%)",
       color = "Farm management")+
  
  theme(
    panel.background = element_blank() ,
    panel.grid.major = element_line(colour = "grey88") ,
    panel.border     = element_rect(colour = "grey80", fill = NA) ,
    
    axis.text.y  = element_text(colour = "black"),
    axis.text.x  = element_blank(),
    title = element_text(face = "bold", size = 13),
    
    legend.position = c(0.81, .37),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(colour = "black", size=12),

    strip.text.x =  element_text(hjust =0.01, face = "bold", size = 14),
    strip.background =  element_blank())


ggsave(
  here::here("graph",  "test.tiff"),
  width = 140,
  height = 200,
  units = "mm",
  dpi = 300
)


# geom elements -----------------------------------------------------------



# 1 slide

survey_data <- read.csv("survey_clean.csv") %>% 
  select(-c(farm, region, zone, district, lon, lat))

head(survey_data)

ggplot()


ggplot(survey_data, aes(x= farm_management, y = inc))


# 2 slide

survey_data = survey_data  %>% 
  mutate(farm_management = factor(farm_management, 
              levels = c("Unmanaged", "Minimal", "Moderate", "Intensive"))) 
  
head(survey_data)

ggplot(survey_data,  aes(x= farm_management, y = inc))


# 3 slide

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot()



ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_point()


# 4 slide

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter()

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2)


# 5 slide

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2)+
  geom_boxplot()
       

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(width = 0.2)
  

# 6 slide


ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_violin()

ggplot(survey_data,  aes(x= inc))+
  geom_histogram()


ggplot(survey_data,  aes(x= inc))+
  geom_density()



# 7 slide

ggplot(survey_data,  aes(x= farm_management, y = inc))+
geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1.2)


ggplot(survey_data, aes(x = inc, y = sev2))+
  geom_point()


ggplot(survey_data, aes(x = inc, y = sev2))+
  geom_point()+ geom_smooth() + geom_rug()



# Aesthetics --------------------------------------------------------------

# Slide 1

ggplot(survey_data,  aes(x= farm_management, y = inc, 
                      shape = farm_management, 
                      color = farm_management))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(width = 0.2)



ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(aes(color = farm_management),width = 0.2)



# Slide 2

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(aes(color = farm_management),width = 0.2,
              size = 3)



ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(aes(colour = farm_management),width = 0.2,
              size = 3, alpha = 0.5)



# Slide 3

head(survey_data)

ggplot(survey_data,  aes(x= farm_management))+
  geom_bar(aes(fill = cultivar)) 



ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, size = 4, shape = 21,
              aes(fill = altitude ))



# Slide 4


ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, size = 4,
              aes(alpha = sev2 ))


ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, shape = 21, fill = "grey",
              aes(size = sev2 ))



# Slide 5

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, size = 3, alpha = 0.8,
    aes(shape = farm_management))


# slide 6

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, size = 3, alpha = 0.8, shape = 21,
              aes(color = farm_management))

ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_jitter(width = 0.2, size = 3, alpha = 0.8, shape = 21,
              aes(fill = farm_management))



# slide 7


ggplot(survey_data, aes(x=inc , y = sev2))+
  geom_smooth(aes(linetype = cultivar), se = FALSE)


ggplot(survey_data, aes(x=inc , y = sev2, color = cultivar))+
  geom_point(alpha = 0.2)+
  geom_smooth(aes(linetype = cultivar), se = FALSE)+
  geom_rug()
  


# Scales -----------------------------------------------------------------

# slide 1

p = ggplot(survey_data,  aes(x= farm_management, y = inc))+
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(aes(colour = farm_management), 
              width = 0.2,size = 3)

p + scale_y_continuous(name = "Incidence (%)",
                       breaks = c(0,25,50, 75, 100),
                       limits = c(0,100))

# slide 2

p + scale_color_manual(
        name = "Farm management",
        values = c("blue", "grey80", "#2ca25f", "#756bb120"))


# slide 3

p + scale_color_brewer(
          name = "Farm management",
          palette = "PRGn")

# slide 4

p + ggsci::scale_color_npg()

p + ggsci::scale_color_aaas()

inauguration::inauguration("bernie_mittens")


# slide 5

ggplot(survey_data, aes(x=inc , y = sev2))+
  geom_smooth(se = FALSE, aes(linetype = cultivar, color = cultivar))+
  # scale_linetype_manual(values = c(1,2,4)) + 
  scale_linetype_manual(values = c("solid","longdash", "991191"))




# Facet -------------------------------------------------------------------

# Slide 1

head(survey_data)

survey_data_long = survey_data  %>% 
   pivot_longer(cols = c(inc, sev2), 
               names_to = "metric", 
               values_to = "rate") %>% 
  
  mutate(metric = factor(metric, 
                         levels = c("inc", "sev2"), 
                         labels = c("Incidence", "Severity")))

head(survey_data_long)



p2 = ggplot(survey_data_long, aes(x= farm_management, y = rate))+
  
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(aes(color = farm_management),
              size = 3, width = 0.2, alpha = .5)+
  
  scale_y_continuous(breaks = seq(0,100,25),
                     limits = c(0,100))+
  scale_color_npg()


# slide 2

p2 + facet_wrap(~metric)

p2 + facet_wrap(~metric, ncol = 1)


# slide 3

p2 + facet_grid(shade~metric)

p2 + facet_grid(~metric, ncol = 1)


# Labels ------------------------------------------------------------------

# Slide 1

  p2 + facet_wrap(~metric, ncol = 1)+
  
  labs(title = "Plot title", 
       subtitle = "Here is a subtitle",
       x = NULL, # suppress x-axis title
       y = "y-axis title",
       color = "Legend title", 
       caption = "Hey!!! This can be a footnote")


# Themes ------------------------------------------------------------------

# slide 1

p2 + facet_wrap(~metric, ncol = 1)+

    theme_bw()

p2 + facet_wrap(~metric, ncol = 1)+
  
  theme_classic()

p2 + facet_wrap(~metric, ncol = 1)+

  theme_minimal()


#slide 2

p2 + facet_wrap(~metric, ncol = 1)+

  ggthemes::theme_wsj()


p2 + facet_wrap(~metric, ncol = 1)+
 
  ggthemes::theme_economist()


p2 + facet_wrap(~metric, ncol = 1)+

  ggthemes::theme_excel()

#slide 3


  p2 + facet_wrap(~metric, ncol = 1)+
  labs(x = NULL, y = "Percent (%)", color = "Farm Management")+  
  
  theme(
    panel.background = element_blank() ,
    panel.grid.major = element_line(colour = "grey88") ,
    panel.border     = element_rect(colour = "grey80", fill = NA),
    
    axis.text.y  = element_text(colour = "black"),
    axis.text.x  = element_blank(),
    title = element_text(colour = "black", size = 12, face = "bold"),

    legend.position = c(0.85, .39),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(colour = "black", size = 12),
    
    strip.text.x =  element_text(hjust =0.01, face = "bold", size = 14),
    strip.background =  element_blank())



# save --------------------------------------------------------------------

  #slide 1
  
  ggsave(filename = here::here("graph", "APS_plot.jpg"),
         width =85, height = 140, units = "mm",
         dpi = 300)
  
  
# slide 2
  
 p3 =  
  p2 + facet_wrap(~metric, ncol = 1)+
      labs(x = NULL, y = "Percent (%)", color = "Farm Management")+  
    
    theme(
      panel.background = element_blank() ,
      panel.grid.major = element_line(colour = "grey88") ,
      panel.border     = element_rect(colour = "grey80", fill = NA),
      
      axis.text.y  = element_text(colour = "black"),
      axis.text.x  = element_blank(),
      title = element_text(colour = "black", face = "bold"),
      
      legend.position = c(0.72, .33),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(colour = "black"),
      
      strip.text.x =  element_text(hjust =0.01, face = "bold"),
      strip.background =  element_blank())
  
 ggsave(
   plot = p3,
   filename = here::here("graph","APS_plot_2.jpg"),
   width = 85,
   height = 140,
   units = "mm",
   dpi = 300
 )
 


# Slide 3
  
  
  
  p4 = 
    ggplot(survey_data_long, aes(x= farm_management, y = rate))+
    
    geom_boxplot(outlier.alpha = 0)+
    geom_jitter(aes(color = farm_management),
                size = 1.5, width = 0.3, alpha = .5)+
    
    scale_y_continuous(breaks = seq(0,100,25),
                       limits = c(0,100))+
    scale_color_npg()+
  
    facet_wrap(~metric, ncol = 1)+
    labs(x = NULL, y = "Percent (%)")+  
    
    theme(
      panel.background = element_blank() ,
      panel.grid.major = element_line(colour = "grey88") ,
      panel.border     = element_rect(colour = "grey80", fill = NA),

      axis.text  = element_text(colour = "black"),
      title = element_text(colour = "black", face = "bold"),
      
      legend.position = "none",
    
      strip.text.x =  element_text(hjust =0.01, face = "bold"),
      strip.background =  element_blank())
  
  ggsave(
    plot = p4,
    filename =here::here("graph", "APS_plot_3.jpg"),
    width = 85,
    height = 140,
    units = "mm",
    dpi = 300
  )
  
  
  
  


