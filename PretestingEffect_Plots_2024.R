### PACKAGES
library(readr)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(interactions)
library(jtools)
library(sjPlot)
library(cowplot)
library(ggplot2)
library(patchwork)
library(writexl)

###---- DATA MANAGEMENT

#EXPERIMENT 1
SQ22df <- read_csv("SQ22df.csv")


#EXPERIMENT 2
FQ22df <- read_csv("FQ22df.csv")
df_fq = FQ22df

#COMBINED EXPERIMENTS
d_sq = SQ22df
d_sq$exp = factor(1)
d_fq = FQ22df
d_fq$exp = factor(2)
d_fq_p = subset(d_fq,group=="A")
d_fq_p = d_fq_p[,-6]
d_fq_p$id = d_fq_p$id + 35
d_fq_p$id = factor(d_fq_p$id)
d_all = rbind(d_sq,d_fq_p)

###---- PLOTS

#FIGURE 1A
SQ22df$Condition <- as.factor(ifelse(df_sq$cond==0,'Study','Pretest'))

df_sq <- aggregate(corr ~ id + cond + Condition, data = SQ22df,FUN = 'mean',na.rm=T)

sq_colors <- c("Study"='#7E7E7E',
               "Pretest"="#08519C")

sim_col <- c("Cue-to-Target"="gray10",
             "Cue-to-Guess"="gray40",
             "Target-to-Guess"="gray70")

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      n = length(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
tempdf_sq2 <- data_summary(df_sq, varname="corr", 
                           groupnames=c("Condition"))

tempdf_sq2$se <- (tempdf_sq2$sd)/(sqrt(tempdf_sq2$n))

df_sq$se <- ifelse(df_sq$cond==0, tempdf_sq2$se[which(tempdf_sq2$Condition=="Study")],
                   tempdf_sq2$se[which(tempdf_sq2$Condition=="Pretest")])

df_sq$mean <- ifelse(df_sq$cond==0, tempdf_sq2$corr[which(tempdf_sq2$Condition=="Study")],
                     tempdf_sq2$corr[which(tempdf_sq2$Condition=="Pretest")])

CondCorr <- ggplot(data=df_sq,aes(x = Condition,y = corr,fill=Condition)) +
  geom_bar(stat = "summary", fun = "mean",position = "dodge",colour='gray0')+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,position=position_dodge(.9))+
  scale_fill_manual(values = sq_colors)+
  ylab("Proportion of Targets Recalled")+
  xlab("")+
  scale_y_continuous(expand = expansion(mult = c(0, -.4)))+
  geom_signif(stat = "identity",
              data = data.frame(x = 1,
                                xend = 2,
                                y = .45,
                                annotation = c("**")),
              aes(x = x,
                  xend = xend,
                  y = y,
                  yend = y,
                  annotation = annotation,
                  textsize = 12),
              tip_length = .01,
              vjust=0.4,
              manual=T,
              inherit.aes=F)

ttest_graph <- CondCorr + theme(axis.ticks.x=element_blank(),
                                axis.title.y = element_text(face='bold'),
                                axis.text.x = element_text(face='bold'),
                                text = element_text(size = 25),
                                panel.border = element_rect(color = "gray0",
                                                            fill = NA,
                                                            size = 1),
                                panel.background = element_rect('white'),
                                legend.position = 'none')

ttest_graph
ggsave('Figure1A.png',width = 4.75,height = 6.2, dpi = 300)

#FIGURE 1B
corrmodel_sq_int = glmer(corr ~ (c2t + c2g + t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1 | id),
                         data=d_sq,family=binomial(link='logit'))
summary(corrmodel_sq_int)

df_sq_s <- subset(df_sq,cond==0)
intval_mean_sq <- mean(df_sq$t2g,na.rm=T)
intval_sd_sq <- sd(df_sq$t2g,na.rm=T)
intval_possd_sq <- intval_mean_sq + intval_sd_sq
intval_negsd_sq <- intval_mean_sq - intval_sd_sq
figure1_B <- interact_plot(corrmodel_sq_int, pred = c2t, modx = t2g,
                            legend.main = "Pretest Condition",
                            x.label = "Cue-to-Target similarity",
                            y.label = "Target Accuracy",
                            colors= c('#08306B','#2171B5'),
                            line.thickness = 2.5,
                            modx.values = c(intval_negsd_sq,
                                            intval_possd_sq),
                            modx.labels = c('Low Guess-Target Similarity', 'High Guess-Target Similarity'))+
  geom_smooth(data=df_sq_s,aes(x=c2t,y=corr),
              method='loess',se=F,color= '#7E7E7E',
              size=2.5, span = 1,
              inherit.aes = F)+
  theme(text=element_text(size=25),
        panel.border = element_rect(color = "gray0",
                                    fill = NA,
                                    size = 1),
        panel.grid.major=element_blank(),
        panel.background = element_blank())

figure1_B

ggsave('Figure1B.png',width = 10,height = 6.2,dpi = 300)

#FIGURE 1C
figure1_C = interact_plot(corrmodel_sq_int, pred = c2g, modx = t2g,
                          legend.main = "Pretest Condition",
                          x.label = "Cue-to-Guess similarity",
                          y.label = "Target Accuracy",
                          colors= c('#08306B','#2171B5'),
                          line.thickness = 2.5,
                          modx.values = c(intval_negsd_sq,
                                          intval_possd_sq),
                          modx.labels = c('Low Guess-Target Similarity', 'High Guess-Target Similarity'))+
  theme(text=element_text(size=25),
        panel.border = element_rect(color = "gray0",
                                    fill = NA,
                                    size = 1),
        panel.grid.major=element_blank(),
        panel.background = element_blank())

figure1_C

ggsave('Figure1C.png',width = 10,height = 6.2,dpi = 300)


#FIGURE 2A
interference_int = glmer(corr ~ corr_g + c2t + c2g+ t2g + (corr_g*c2t) + (corr_g * c2g) + (corr_g*t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1|id),
                         data = d_sq,family=binomial(link='logit'))
summary(interference_int)

val_mean_sq <- mean(d_sq$t2g,na.rm=T)
val_sd_sq <- sd(d_sq$t2g,na.rm=T)
val_possd_sq <- val_mean_sq + val_sd_sq
val_negsd_sq <- val_mean_sq - val_sd_sq

interact_plot(interference_int,pred=corr_g,modx=t2g,
              legend.main = "Pretest Condition",
              x.label = "Guess Recall Accuracy",
              y.label = "Target Accuracy",
              colors= c('#08306B','#2171B5'),
              line.thickness = 2.5,
              modx.values = c(val_negsd_sq,
                              val_possd_sq),
              modx.labels = c('Low Guess-Target Similarity', 'High Guess-Target Similarity'))+
  theme(text=element_text(size=25),
        panel.border = element_rect(color = "gray0",
                                    fill = NA,
                                    size = 1),
        panel.grid.major=element_blank(),
        panel.background = element_blank())

ggsave('Figure2A.png',width = 10,height = 6.2,dpi = 300)

#FIGURE 2B
val_mean_sq_c2g <- mean(d_sq$c2g,na.rm=T)
val_sd_sq_c2g <- sd(d_sq$c2g,na.rm=T)
val_possd_sq_c2g <- val_mean_sq_c2g + val_sd_sq_c2g
val_negsd_sq_c2g <- val_mean_sq_c2g - val_sd_sq_c2g

interact_plot(interference_int,pred=corr_g,modx=c2g,
              legend.main = "Pretest Condition",
              x.label = "Guess Recall Accuracy",
              y.label = "Target Accuracy",
              colors= c('#4292C6','#DEEBF7'),
              line.thickness = 2.5,
              modx.values = c(val_negsd_sq,
                              val_possd_sq),
              modx.labels = c('Low Cue-Guess Similarity', 'High Cue-Guess Similarity'))+
  theme(text=element_text(size=25),
        panel.border = element_rect(color = "gray0",
                                    fill = NA,
                                    size = 1),
        panel.grid.major=element_blank(),
        panel.background = element_blank())

ggsave('Figure2B.png',width = 10,height = 6.2,dpi = 300)

#FIGURE 3A
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      n = length(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

fq_sub_df <- aggregate(corr ~ id + cond + group,
                       data = FQ22df,FUN = 'mean',na.rm=T)

fq_colors <- c("Study"='#7E7E7E',
               "Pretest"="#08519C",
               "Generation Short"="#E31A1C",
               "Generation Long"='#FFEDA0')


fq_sub_df$Condition <- as.factor(ifelse(fq_sub_df$cond==0,'Study',
                                        ifelse(fq_sub_df$cond==1,'Pretest',
                                               ifelse(fq_sub_df$cond==2,'Generation Short',
                                                      'Generation Long'))))

fq_sub_df_g <- aggregate(corr_g ~ id + cond + group,
                         data = FQ22df,FUN = 'mean',na.rm=T)

tempdf <- data_summary(fq_sub_df, varname="corr", 
                       groupnames=c("Condition","group"))

tempdf$se <- (tempdf$sd)/(sqrt(tempdf$n))

df <- fq_sub_df

df$se <- ifelse(df$cond==0 & df$group=="A", tempdf$se[which(tempdf$Condition=="Study"&tempdf$group=="A")],
                ifelse(df$cond==0 & df$group=="B",tempdf$se[which(tempdf$Condition=="Study"&tempdf$group=="B")],
                       ifelse(df$cond==0 & df$group=="C", tempdf$se[which(tempdf$Condition=="Study"&tempdf$group=="C")],
                              ifelse(df$cond==1, tempdf$se[which(tempdf$Condition=="Pretest")],
                                     ifelse(df$cond==2, tempdf$se[which(tempdf$Condition=="Generation Short")],
                                            tempdf$se[which(tempdf$Condition=="Generation Long")])))))

df$mean <- ifelse(df$cond==0 & df$group=="A", tempdf$corr[which(tempdf$Condition=="Study"&tempdf$group=="A")],
                  ifelse(df$cond==0 & df$group=="B",tempdf$corr[which(tempdf$Condition=="Study"&tempdf$group=="B")],
                         ifelse(df$cond==0 & df$group=="C", tempdf$corr[which(tempdf$Condition=="Study"&tempdf$group=="C")],
                                ifelse(df$cond==1, tempdf$corr[which(tempdf$Condition=="Pretest")],
                                       ifelse(df$cond==2, tempdf$corr[which(tempdf$Condition=="Generation Short")],
                                              tempdf$corr[which(tempdf$Condition=="Generation Long")])))))

CondCorr_fq <- ggplot(data=df,aes(x = group,y = corr,fill=Condition)) +
  geom_bar(stat = "summary", fun = "mean",position = "dodge",color="gray0")+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,position=position_dodge(.9))+
  geom_signif(annotations = "***",
              y_position = .45,
              xmin = .75,xmax = 1.25,
              tip_length = 0, vjust=0.1,
              textsize = 9)+
  geom_signif(annotations='***',
              y_position = .5,
              xmin= .75, xmax = 1.75,
              tip_length= 0, vjust=0.1,
              textsize = 9)+
  geom_signif(annotations='p = .06',
              y_position = .55,
              xmin = .75, xmax = 2.75,
              tip_length = 0,vjust = 0.1,
              textsize = 7)+
  geom_signif(annotations = '*',
              y_position = .4,
              xmin = 2.75, xmax = 3.25,
              tip_length = 0, vjust = .1,
              textsize = 9)+
  scale_fill_manual(values = fq_colors)+
  ylab("Proportion of Targets Recalled")+
  scale_x_discrete(labels=group_lab,name="Group")+
  scale_y_continuous(expand = expansion(mult = c(0, -.35)))

anova_graph <- CondCorr_fq + theme(axis.ticks.x=element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.title.y = element_text(face='bold'),
                                   axis.title.x = element_text(face='bold'),
                                   legend.title = element_text(face='bold'),
                                   text = element_text(size = 25),
                                   panel.border = element_rect(color = "gray0",
                                                               fill = NA,
                                                               size = 1),
                                   panel.background = element_rect('white'))

anova_graph

ggsave('Figure3A.png', width = 8.8, height = 6.2,dpi=300)

#FIGURE 3B

df_g <- subset(fq_sub_df_g,cond!='0')

df_g$Condition <- as.factor(ifelse(df_g$cond==1,'Pretest',
                                   ifelse(df_g$cond==2,'Generation Short',
                                          'Generation Long')))

df_g <- na.omit(df_g)

tempdf_g <- data_summary(df_g, varname="corr_g", 
                         groupnames=c("Condition","group"))

tempdf_g$se <- (tempdf_g$sd)/(sqrt(tempdf_g$n))

df_g$se <- ifelse(df_g$group=="A", tempdf_g$se[which(tempdf_g$group=="A")],
                  ifelse(df_g$group=="B",tempdf_g$se[which(tempdf_g$group=="B")],
                         tempdf_g$se[which(tempdf_g$group=="C")]))

df_g$mean <- ifelse(df_g$group=="A", tempdf_g$corr_g[which(tempdf_g$group=="A")],
                    ifelse(df_g$group=="B",tempdf_g$corr_g[which(tempdf_g$group=="B")],
                           tempdf_g$corr_g[which(tempdf_g$group=="C")]))


CondCorrg <- ggplot(data=df_g,aes(x = group,y = corr_g,fill=Condition)) +
  geom_bar(stat = "summary", fun = "mean",position = "dodge",color="gray0")+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=.1,position=position_dodge(.9))+
  scale_fill_manual(values = fq_colors)+
  ylab("Average Guess Recall Accuracy")+
  scale_x_discrete(labels=group_lab,name="Group")+
  scale_y_continuous(expand = expansion(mult = c(0, -.4)))

corrg_graph <- CondCorrg + theme(axis.ticks.x=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.title.y = element_text(face='bold'),
                                 axis.title.x = element_text(face='bold'),
                                 legend.title = element_text(face='bold'),
                                 text = element_text(size = 20),
                                 panel.border = element_rect(color = "gray0",
                                                             fill = NA,
                                                             size = 1),
                                 panel.background = element_rect('white'))

corrg_graph

ggsave('Figure3B.png', width = 7.3, height = 6.2,dpi=300)

#FIGURE 4
corrmodel_fq_p_int2 <- glmer(corr ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (c2t*c2g*t2g) + (1|id),data=d_fq_p,
                             family=binomial(link='logit'))
summary(corrmodel_fq_p_int2)

threeway <- interact_plot(corrmodel_fq_p_int2,pred=c2t,modx=t2g,mod2 = c2g,
                          legend.main='Target-to-Guess similarity',
                          x.label='Cue-to-Target similarity',
                          y.label = 'Target Accuracy',
                          colors = c('#08306B','#2171B5'),
                          line.thickness = 2.5,
                          modx.values = c(intval_negsd_fq_t,
                                          intval_possd_fq_t),
                          modx.labels = c('- 1 SD', '+ 1 SD'))+
  theme(text=element_text(size=20),
        panel.border = element_rect(color = "gray0",
                                    fill = NA,
                                    size = 1),
        panel.grid.major=element_blank())

threeway

ggsave('Figure4.png',width=12.5,height=6.25,dpi = 300)