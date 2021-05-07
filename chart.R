

library(Cairo)
library(ggplot2)
library(scales)
library(datapasta)
library(DescTools)

input <-
  data.frame(
    stringsAsFactors = FALSE,
    Year = c("2008/09", "2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19"),
    Surgical.Spells = c(3967217,3967826,3963278,
                        4011631,3894315,3966194,3951965,3886593,3884866,3722246,
                        3765686),
    Deaths = c(24179,23206,22667,21868,
               21648,20737,20657,20458,20796,20949,19698),
    Crude.Death.rate = c(0.0060947,0.0058485,0.0057193,
                         0.0054511,0.0055589,0.0052284,0.005227,0.0052637,
                         0.0053531,0.0056281,0.0052309)
  )


library(binom)

input$CIlower <- binom.confint(x=input$Death, n=input$Surgical.Spells, methods = "wilson")$lower
input$CIupper <- binom.confint(x=input$Death, n=input$Surgical.Spells, methods = "wilson")$upper



# Custom caption
my_caption <- expression(paste(bold("Data Source:  \n"), " NHS Secondary Uses Service.  Surgical spells were identified by episodes with Treatment Function codes for surgical specialties, \n where a primary procedure was present. Regular attenders and spells with unknown Patient Classifications were exluded. Binomial \n confidence intervals (grey) were calculated using the Wilson score method."))

library(grid)
library(gridExtra)

# Custom title to get around alignment issue
title.grob <- textGrob(
  label = "England periopertative in-hospital mortality rate per year",
  x = unit(1, "lines"),
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 16, fontface="bold"))


p<-ggplot(input, aes(x=Year, y=Crude.Death.rate))+
  #geom_line(aes(y=CIupper, group=1), linetype="dashed", col="grey")+
  #geom_line(aes(y=CIlower, group=1), linetype="dashed", col="grey")+
  #geom_errorbar(aes(ymin=CIlower, ymax=CIupper, group=1))+
  geom_ribbon(aes(ymin=CIlower, ymax=CIupper, group=1), col ="grey", fill="grey", alpha=0.7)+
  geom_point()+
  geom_line(aes(group=1))+

  geom_text(aes(label=comma(Deaths)), nudge_x = -0.33
            , nudge_y = c(rep(0,4), rep(0.00002,1), 0 ,rep(0.00002,3), 0,0)
            ,size=3)+
  scale_y_continuous(name ="Crude Mortality Rate (%)"
                     , labels=label_percent(accuracy = 0.01)
                     ,expand = expansion(mult=0.1)
                    , breaks = seq(0, 0.007, 0.0001)
                     #limits= c(0, 0.008)
  )+
  labs(title = NULL
        #title = "England periopertative in-hospital mortality rate",
       # subtitle=,
          ,caption = my_caption)+
  theme_minimal()+
  theme(#plot.subtitle = element_text(face="italic"),
        #plot.title = element_text(face="bold", margin=margin(l=-10, b=5)),
        plot.caption.position = "plot", plot.caption = element_text(hjust=0, vjust = 0),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 20, l = 0)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))



p1 <- arrangeGrob(p, top = title.grob)

# png(filename = "plot_scaled.png", width = 869, height = 550)
# grid.draw(p1)
# dev.off()

ggsave(filename = "plot_scaled_ggplot.png", p1)




#  Same but rescaled

p2<-ggplot(input, aes(x=Year, y=Crude.Death.rate))+
  #geom_line(aes(y=CIupper, group=1), linetype="dashed", col="grey")+
  #geom_line(aes(y=CIlower, group=1), linetype="dashed", col="grey")+
  #geom_errorbar(aes(ymin=CIlower, ymax=CIupper, group=1))+
  geom_ribbon(aes(ymin=CIlower, ymax=CIupper, group=1), col ="grey", fill="grey", alpha=0.7)+
  geom_point()+
  geom_line(aes(group=1))+

  geom_text(aes(label=comma(Deaths)), nudge_x = -0.33
            , nudge_y = -0.0003
            ,size=3)+
  scale_y_continuous(name ="Crude Mortality Rate (%)"
                     , labels=label_percent(accuracy = 0.01)
                     ,expand = expansion(mult=0.1)
                     , breaks = seq(0, 0.007, 0.001)
                     , limits= c(0, 0.007)
  )+
  labs(title = NULL
       #title = "England periopertative in-hospital mortality rate",
       # subtitle=,
       ,caption = my_caption)+
  theme_minimal()+
  theme(#plot.subtitle = element_text(face="italic"),
    #plot.title = element_text(face="bold", margin=margin(l=-10, b=5)),
    plot.caption.position = "plot", plot.caption = element_text(hjust=0, vjust = 0),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 20, l = 0)),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))



p3 <- arrangeGrob(p2, top = title.grob)

# png(filename = "plot_scaled.png", width = 869, height = 550)
grid.draw(p3)
# dev.off()

ggsave(filename = "plot_ggplot.png", p3)
