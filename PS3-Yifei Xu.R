#Your task is to replicate, and then improve on, Figure 2. Be sure to choose an appropriate size for the visuals in your final output. 

#1. Replicate Figure 2 from the article. Match *every* element of the plot. Note that for this figure, they create two separate graphs and paste them together. You will need to do the same with `patchwork`.  

#2. Improve Figure 2. I'm asking for several changes:   
#    - Set the vertical axes to range from 0-1.   
#   - Remove the gridlines from the plot.   
#    - Add a lightly shaded area denoting the use of the Understanding Clause (1954 to 1965 according to Justice Department investigations).  
#    - Use faceting to create two subplots instead of two separate plots. Hint: you will need to pivot to make this work.  
#    - Improve the visibility of the lines/shapes as you see fit. 

library(dplyr)
library(ggplot2)
install.packages("patchwork")
library(patchwork)

cleaned_data<-la_turnout_basic %>%
  filter(!is.na(understandingclause2)) %>%
  group_by(year,understandingclause2) %>%
  summarize(
    mean_black_byyear=mean(blackregrate,na.rm = TRUE),
    mean_white_byyear=mean(whiteregrate,na.rm = TRUE),
    .groups = "drop")


plot1<-ggplot(cleaned_data,aes(x=year,y=mean_black_byyear,group=understandingclause2)) +
  geom_line(data=cleaned_data %>% filter(understandingclause2 == 1), color = "yellow")+
  geom_line(data = cleaned_data %>% filter(understandingclause2 == 0), color = "black") +
  geom_point(aes(shape = factor(understandingclause2)), size = 2) + 
  scale_shape_manual(values = c(16, 17),labels = c("Control", "Treated")) +
  scale_x_continuous(
    limits = c(1950,1970),
    breaks=c(1950, 1955, 1960, 1965, 1970),
    labels = c("1950", "1955", "1960", "1965", "1970"),
    expand=c(0.05,0))+
  scale_y_continuous(
    limits = c(0.065,0.65),
    breaks = c(0.2,0.4,0.6),
    labels = c("0.2","0.4","0.6"),
    expand=c(0,0.05)
    ) +
  labs(
    title = NULL,
    caption = "(a) Black Registration",
    x="Year",
    y="Black Registration Rate"
  ) +
  theme_minimal()+
  theme(
    plot.caption = element_text(size = 12,hjust =0.5),
    legend.title = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )


plot2<-ggplot(cleaned_data,aes(x=year,y=mean_white_byyear,group=understandingclause2)) +
  geom_line(data=cleaned_data %>% filter(understandingclause2 == 1), color = "yellow")+
  geom_line(data = cleaned_data %>% filter(understandingclause2 == 0), color = "black") +
  geom_point(aes(shape = factor(understandingclause2)), size = 2) + 
  scale_shape_manual(values = c(16, 17),labels = c("Control", "Treated")) +
  scale_x_continuous(
    limits = c(1950,1970),
    breaks=c(1950, 1955, 1960, 1965, 1970),
    labels = c("1950", "1955", "1960", "1965", "1970"),
    expand=c(0.05,0))+
  scale_y_continuous(
    limits = c(0.6,1.0),
    breaks = c(0.6,0.7,0.8,0.9,1.0),
    labels = c("0.6","0.7","0.8","0.9","1.0"),
    expand=c(0.06,0.01) 
    ) +
  labs(
    title = NULL,
    caption = "(b) White Registration",
    x="Year",
    y="White Registration Rate"
  ) +
  theme_minimal()+
  theme(
    plot.caption = element_text(size = 12,hjust = 0.5),
    legend.title = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) 

plot_together<-plot1+plot2



