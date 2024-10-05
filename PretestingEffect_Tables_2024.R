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


###---- TABLES


## TABLE 1

pretestmodel_sq <-glmer(corr ~ cond + c2t + (1 | id),data=d_sq,family=binomial(link='logit'))
summary(pretestmodel_sq)

pretestmodel_sq_int <- glmer(corr ~ cond + c2t + (cond * c2t) + (1 | id),data=d_sq,family=binomial(link='logit'))
summary(pretestmodel_sq_int)

pretesttable_sq <- tab_model(pretestmodel_sq,pretestmodel_sq_int,
                             pred.labels = c('(Intercept)',
                                             'Condition',
                                             'Cue-to-Target Similarity',
                                             'Condition * Cue-to-Target'),
                             dv.labels = c('Model 1',
                                           'Model 2'),
                             transform = NULL,
                             show.est = T,
                             show.stat = T,
                             show.r2 = F,
                             show.re.var = F,
                             show.ngroups = F,
                             show.obs = F,
                             show.icc = F,
                             file = 'TABLE1.html')

pretesttable_sq

## TABLE 2

corrmodel_sq <- glmer(corr ~ (c2t + c2g + t2g) + (1 | id),data=d_sq,family=binomial(link='logit'))
summary(corrmodel_sq)

corrmodel_sq_int = glmer(corr ~ (c2t + c2g + t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1 | id),
                         data=d_sq,family=binomial(link='logit'))
summary(corrmodel_sq_int)

corrmodel_sq_int1 <- glmer(corr ~ (c2t + c2g + t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (c2t*c2g*t2g) + (1 | id),
                           data=d_sq,family=binomial(link='logit'))
summary(corrmodel_sq_int1)


simtable_sq <- tab_model(corrmodel_sq,
                         corrmodel_sq_int,
                         corrmodel_sq_int1,
                         pred.labels = c('(Intercept)',
                                         'Cue-to-Target Similarity',
                                         'Cue-to-Guess Similarity',
                                         'Guess-to-Target Similarity',
                                         'Cue-to-Target * Cue-to-Guess',
                                         'Cue-to-Target * Guess-to-Target',
                                         'Cue-to-Guess * Guess-to-Target',
                                         'Cue-to-Target * Cue-to-Guess * Guess-to-Target'),
                         dv.labels = c('Model 1','Model 2', 'Model 3'),
                         transform = NULL,
                         show.r2 = F,
                         show.re.var = F,
                         show.ngroups = F,
                         show.obs = F,
                         show.icc = F,
                         file = 'TABLE2.html'
)

simtable_sq

## TABLE 3

corrgmodel_sq <-glmer(corr_g ~ c2t+c2g+t2g + (1|id),data=d_sq,family=binomial(link='logit'))
summary(corrgmodel_sq)

corrgmodel_sq_int <- glmer(corr_g ~ c2t+c2g+t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (1 | id),data=d_sq,family=binomial(link='logit'))
summary(corrgmodel_sq_int)

corrgmodel_sq_int2 <- glmer(corr_g ~ c2t+c2g+t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (c2t*c2g*t2g) + (1 | id),data=d_sq,family=binomial(link='logit'))
summary(corrgmodel_sq_int2)

simgtable_sq <- tab_model(corrgmodel_sq,
                          corrgmodel_sq_int,
                          corrgmodel_sq_int2,
                          pred.labels = c('(Intercept)',
                                          'Cue-to-Target Similarity',
                                          'Cue-to-Guess Similarity',
                                          'Guess-to-Target Similarity',
                                          'Cue-to-Target * Cue-to-Guess',
                                          'Cue-to-Target * Guess-to-Target',
                                          'Cue-to-Guess * Guess-to-Target',
                                          'Cue-to-Target * Cue-to-Guess * Guess-to-Target'),
                          dv.labels = c('Model 1','Model 2','Model 3'),
                          transform = NULL,
                          show.r2 = F,
                          show.re.var = F,
                          show.ngroups = F,
                          show.obs = F,
                          show.icc = F,
                          file = 'TABLE3.html'
)

simgtable_sq

## TABLE 4

interference_main = glmer(corr ~ corr_g + c2t + c2g+ t2g + (1|id), data = d_sq,family=binomial(link='logit'))
summary(interference_main)

interference_int = glmer(corr ~ corr_g + c2t + c2g+ t2g + (corr_g*c2t) + (corr_g * c2g) + (corr_g*t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1|id),
                         data = d_sq,family=binomial(link='logit'))
summary(interference_int)

interference_int2 = glmer(corr ~ corr_g+c2t+c2g+t2g+(corr_g*c2t)+(corr_g * c2g)+(corr_g*t2g)+(c2t*c2g)+(c2t*t2g)+(c2g*t2g)+(corr_g*c2t*c2g)+(corr_g*c2t*t2g)+(corr_g*c2g*t2g)+(c2t*c2g*t2g)+(1|id),
                         data = d_sq,family=binomial(link='logit'))
summary(interference_int2)

interference_int3 = glmer(corr ~ corr_g+c2t+c2g+t2g+(corr_g*c2t)+(corr_g * c2g)+(corr_g*t2g)+(c2t*c2g)+(c2t*t2g)+(c2g*t2g)+(corr_g*c2t*c2g)+(corr_g*c2t*t2g)+(corr_g*c2g*t2g)+(c2t*c2g*t2g)+(corr_g*c2t*c2g*t2g)+(1|id),
                          data = d_sq,family=binomial(link='logit'))
summary(interference_int3)

inttable_sq <- tab_model(interference_main,
                         interference_int,
                         interference_int2,
                         interference_int3,
                         pred.labels = c('(Intercept)',
                                         'Guess Recall Accuracy',
                                         'Cue-to-Target Similarity',
                                         'Cue-to-Guess Similarity',
                                         'Guess-to-Target Similarity',
                                         'Guess Recall * Cue-Target',
                                         'Guess Recall * Cue-Guess',
                                         'Guess Recall * Guess-Target',
                                         'Cue-to-Target * Cue-to-Guess',
                                         'Cue-to-Target * Guess-to-Target',
                                         'Cue-to-Guess * Guess-to-Target',
                                         'Guess Recall * Cue-to-Target * Cue-to-Guess',
                                         'Guess Recall * Cue-to-Target * Guess-to-Target',
                                         'Guess Recall * Cue-to-Guess * Guess-to-Target',
                                         'Cue-to-Target * Cue-to-Guess * Guess-to-Target',
                                         'Guess Recall * Cue-to-Target * Cue-to-Guess * Guess-to-Target'),
                         dv.labels = c('Model 1','Model 2','Model 3','Model 4'),
                         transform = NULL,
                         show.r2 = F,
                         show.re.var = F,
                         show.ngroups = F,
                         show.obs = F,
                         show.icc = F,
                         file = 'TABLE4.html'
)

inttable_sq

## TABLE 5

condmodel_fq_p <- glmer(corr ~ cond + c2t + (1 | id),data=d_fq_p,
                        family=binomial(link='logit'))
summary(condmodel_fq_p)

condmodel_fq_p_int <- glmer(corr ~ cond + c2t + (cond * c2t) + (1 | id),data=d_fq_p,
                            family=binomial(link='logit'))
summary(condmodel_fq_p_int)

pretesttable_fq <- tab_model(condmodel_fq_p,condmodel_fq_p_int,
                             pred.labels = c('(Intercept)',
                                             'Condition',
                                             'Cue-to-Target Similarity',
                                             'Condition * Cue-to-Target'),
                             dv.labels = c('Model 1',
                                           'Model 2'),
                             transform = NULL,
                             show.est = T,
                             show.stat = T,
                             show.r2 = F,
                             show.re.var = F,
                             show.ngroups = F,
                             show.obs = F,
                             show.icc = F,
                             file = 'TABLE5.html')

pretesttable_fq

## TABLE 6

corrmodel_fq_p <- glmer(corr ~ c2t + c2g + t2g + (1|id),data=d_fq_p,
                        family=binomial(link='logit'))
summary(corrmodel_fq_p)

corrmodel_fq_p_int <- glmer(corr ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (1|id),
                            data=d_fq_p,
                            family=binomial(link='logit'))
summary(corrmodel_fq_p_int)

corrmodel_fq_p_int2 <- glmer(corr ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (c2t*c2g*t2g) + (1|id),data=d_fq_p,
                             family=binomial(link='logit'))
summary(corrmodel_fq_p_int2)

ptsimtable_fq <- tab_model(corrmodel_fq_p,
                           corrmodel_fq_p_int,
                           corrmodel_fq_p_int2,
                           pred.labels = c('(Intercept)',
                                           'Cue-to-Target Similarity',
                                           'Cue-to-Guess Similarity',
                                           'Guess-to-Target Similarity',
                                           'Cue-to-Target * Cue-to-Guess',
                                           'Cue-to-Target * Guess-to-Target',
                                           'Cue-to-Guess * Guess-to-Target',
                                           'Cue-Target * Cue-Guess * Guess-to-Target'),
                           dv.labels = c('Model 1','Model 2',
                                         'Model 3'),
                           transform = NULL,
                           show.r2 = F,
                           show.re.var = F,
                           show.ngroups = F,
                           show.obs = F,
                           show.icc = F,
                           file = 'TABLE6.html'
)

ptsimtable_fq

## TABLE 7

corrgmodel_fq_p <- glmer(corr_g ~ c2t + c2g + t2g + (1|id),data=d_fq_p,
                         family=binomial(link='logit'))
summary(corrgmodel_fq_p)

corrgmodel_fq_p_int <- glmer(corr_g ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (1|id),
                             data=d_fq_p,
                             family=binomial(link='logit'))
summary(corrgmodel_fq_p_int)

corrgmodel_fq_p_int2 <- glmer(corr_g ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (c2t*c2g*t2g) + (1|id),
                             data=d_fq_p,
                             family=binomial(link='logit'))
summary(corrgmodel_fq_p_int2)

ptsimgtable_fq <- tab_model(corrgmodel_fq_p,
                            corrgmodel_fq_p_int,
                            corrgmodel_fq_p_int2,
                            pred.labels = c('(Intercept)',
                                            'Cue-to-Target Similarity',
                                            'Cue-to-Guess Similarity',
                                            'Guess-to-Target Similarity',
                                            'Cue-to-Target * Cue-to-Guess',
                                            'Cue-to-Target * Guess-to-Target',
                                            'Cue-to-Guess * Guess-to-Target',
                                            'Cue-to-Target * Cue-to-Guess * Guess-to-Target'),
                            dv.labels = c('Model 1','Model 2','Model 3'),
                            transform = NULL,
                            show.r2 = F,
                            show.re.var = F,
                            show.ngroups = F,
                            show.obs = F,
                            show.icc = F,
                            file = 'TABLE7.html'
)

ptsimgtable_fq

## TABLE 8

interference_main_fq = glmer(corr ~ corr_g + c2t + c2g+ t2g + (1|id), data = d_fq_p,family=binomial(link='logit'))
summary(interference_main_fq)

interference_int_fq = glmer(corr ~ corr_g + c2t + c2g+ t2g + (corr_g*c2t) + (corr_g * c2g) + (corr_g*t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1|id),
                            data = d_fq_p,family=binomial(link='logit'))
summary(interference_int_fq)

#interact_plot(interference_int_fq,pred=t2g,modx=corr_g)

interference_int2_fq = glmer(corr ~ corr_g + c2t + c2g+ t2g + (corr_g*c2t) + (corr_g * c2g) + (corr_g*t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (corr_g*c2t*c2g) + (corr_g*c2t*t2g) + (corr_g*c2g*t2g) + (c2t*c2g*t2g) + (1|id),
                             data = d_fq_p,family=binomial(link='logit'))
summary(interference_int2_fq)

interference_int3_fq = glmer(corr ~ corr_g+c2t+c2g+t2g+(corr_g*c2t)+(corr_g * c2g)+(corr_g*t2g)+(c2t*c2g)+(c2t*t2g)+(c2g*t2g)+(corr_g*c2t*c2g)+(corr_g*c2t*t2g)+(corr_g*c2g*t2g)+(c2t*c2g*t2g)+(corr_g*c2t*c2g*t2g)+(1|id),
                          data = d_fq_p,family=binomial(link='logit'))
summary(interference_int3_fq)

inttable_fq = tab_model(interference_main_fq,
                        interference_int_fq,
                        interference_int2_fq,
                        interference_int3_fq,
                        pred.labels = c('(Intercept)',
                                        'Guess Recall Accuracy',
                                        'Cue-to-Target Similarity',
                                        'Cue-to-Guess Similarity',
                                        'Guess-to-Target Similarity',
                                        'Guess Recall * Cue-Target',
                                        'Guess Recall * Cue-Guess',
                                        'Guess Recall * Guess-Target',
                                        'Cue-to-Target * Cue-to-Guess',
                                        'Cue-to-Target * Guess-to-Target',
                                        'Cue-to-Guess * Guess-to-Target',
                                        'Guess Recall * Cue-to-Target * Cue-to-Guess',
                                        'Guess Recall * Cue-to-Target * Guess-to-Target',
                                        'Guess Recall * Cue-to-Guess * Guess-to-Target',
                                        'Cue-to-Target * Cue-to-Guess * Guess-to-Target',
                                        'Guess Recall * Cue-to-Target * Cue-to-Guess * Guess-to-Target'),
                        dv.labels = c('Model 1','Model 2','Model 3','Model 4'),
                        transform = NULL,
                        show.r2 = F,
                        show.re.var = F,
                        show.ngroups = F,
                        show.obs = F,
                        show.icc = F,
                        file = 'TABLE8.html')

inttable_fq


## TABLE 9

corrmodel_fq_gl <- glmer(corr ~ c2t + c2g + t2g + (1|id),data=d_fq_gl,
                         family=binomial(link='logit'))
summary(corrmodel_fq_gl)

corrmodel_fq_gl_int <- glmer(corr ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (1|id),
                             data=d_fq_gl,
                             family=binomial(link='logit'))
summary(corrmodel_fq_gl_int)

corrmodel_fq_gl_int2 <- glmer(corr ~ c2t + c2g + t2g + (c2t * c2g) + (c2t * t2g) + (c2g * t2g) + (c2t*c2g*t2g) + (1|id),
                             data=d_fq_gl,
                             family=binomial(link='logit'))
summary(corrmodel_fq_gl_int2)

gl_table = tab_model(corrmodel_fq_gl,
                     corrmodel_fq_gl_int,
                     corrmodel_fq_gl_int2,
                     pred.labels = c('(Intercept)',
                                     'Cue-to-Target Similarity',
                                     'Cue-to-Generation Similarity',
                                     'Generation-to-Target Similarity',
                                     'Cue-to-Target * Cue-to-Generation',
                                     'Cue-to-Target * Generation-to-Target',
                                     'Cue-to-Generation * Generation-to-Target',
                                     'Cue-to-Target * Cue-to-Generation * Generation-to-Target'),
                     dv.labels = c('Model 1','Model 2', 'Model 3'),
                     transform = NULL,
                     show.r2 = F,
                     show.re.var = F,
                     show.ngroups = F,
                     show.obs = F,
                     show.icc = F,
                     file = 'TABLE9.html')

gl_table

## TABLE 10

interference_main_gl = glmer(corr ~ corr_g + c2t + c2g+ t2g + (1|id), data = d_fq_gl,family=binomial(link='logit'))
summary(interference_main_gl)

interference_int_gl = glmer(corr ~ corr_g + c2t + c2g+ t2g + (corr_g*c2t) + (corr_g * c2g) + (corr_g*t2g) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (1|id),
                            data = d_fq_gl,family=binomial(link='logit'))
summary(interference_int_gl)

interference_int2_gl = glmer(corr ~ corr_g+c2t+c2g+t2g+(corr_g*c2t)+(corr_g * c2g)+(corr_g*t2g)+(c2t*c2g)+(c2t*t2g)+(c2g*t2g)+(corr_g*c2t*c2g)+(corr_g*c2t*t2g)+(corr_g*c2g*t2g)+(c2t*c2g*t2g)+(1|id),
                             data = d_fq_gl,family=binomial(link='logit'))
summary(interference_int2_gl)

interference_int3_gl = glmer(corr ~ corr_g+c2t+c2g+t2g+(corr_g*c2t)+(corr_g * c2g)+(corr_g*t2g)+(c2t*c2g)+(c2t*t2g)+(c2g*t2g)+(corr_g*c2t*c2g)+(corr_g*c2t*t2g)+(corr_g*c2g*t2g)+(c2t*c2g*t2g)+(corr_g*c2t*c2g*t2g)+(1|id),
                            data = d_fq_gl,family=binomial(link='logit'))
summary(interference_int3_gl)

inttable_gl = tab_model(interference_main_gl,
                        interference_int_gl,
                        interference_int2_gl,
                        interference_int3_gl,
                        pred.labels = c('(Intercept)',
                                        'Generation Recall Accuracy',
                                        'Cue-Target Similarity',
                                        'Cue-Generation Similarity',
                                        'Generation-Target Similarity',
                                        'Generation Recall * Cue-Target',
                                        'Generation Recall * Cue-Generation',
                                        'Generation Recall * Generation-Target',
                                        'Cue-Target * Cue-Generation',
                                        'Cue-Target * Generation-Target',
                                        'Cue-Generation * Generation-Target',
                                        'Generation Recall * Cue-Target * Cue-Generation',
                                        'Generation Recall * Cue-Target * Generation-Target',
                                        'Generation Recall * Cue-Generation * Generation-Target',
                                        'Cue-Target * Cue-Generation * Generation-Target',
                                        'Generation Recall * Cue-Target * Cue-Generation * Generation-Target'),
                        dv.labels = c('Model 1','Model 2','Model 3', "Model 4"),
                        transform = NULL,
                        show.r2 = F,
                        show.re.var = F,
                        show.ngroups = F,
                        show.obs = F,
                        show.icc = F,
                        file = 'TABLE10.html'
)

inttable_gl

## TABLE 11

pretestmodel_all <-glmer(corr ~ cond + c2t + exp + (1 | id),data=d_all,family=binomial(link='logit'))
summary(pretestmodel_all)

pretestmodel_all_int <- glmer(corr ~ cond + c2t + exp + (cond * c2t) + (cond*exp) + (c2t*exp) + (1 | id),data=d_all,family=binomial(link='logit'))
summary(pretestmodel_all_int)

pretestmodel_all_int2 <- glmer(corr ~ cond + c2t + exp + (cond * c2t) + (cond*exp) + (c2t*exp) + (cond*c2t*exp) +(1 | id),data=d_all,family=binomial(link='logit'))
summary(pretestmodel_all_int2)

pretesttable_all <- tab_model(pretestmodel_all,pretestmodel_all_int,pretestmodel_all_int2,
                              pred.labels = c('(Intercept)',
                                              'Condition',
                                              'Cue-to-Target Similarity',
                                              'Experiment 2',
                                              'Condition * Cue-to-Target',
                                              'Condition * Experiment',
                                              'Cue-to-Target * Experiment',
                                              'Condition * Cue-to-Target * Experiment'),
                              dv.labels = c('Model 1',
                                            'Model 2',
                                            'Model 3'),
                              transform = NULL,
                              show.est = T,
                              show.stat = T,
                              show.r2 = F,
                              show.re.var = F,
                              show.ngroups = F,
                              show.obs = F,
                              show.icc = F,
                              file = 'TABLE11.html')

pretesttable_all

## TABLE 12


corrmodel_all <- glmer(corr ~ (c2t + c2g + t2g + exp) + (1 | id),data=d_all,family=binomial(link='logit'))
summary(corrmodel_all)
#c2t, c2g and t2g significant

corrmodel_all_int = glmer(corr ~ (c2t + c2g + t2g + exp) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (1 | id),
                          data=d_all,family=binomial(link='logit'))
summary(corrmodel_all_int)

corrmodel_all_int2 = glmer(corr ~ (c2t + c2g + t2g + exp) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (c2t*c2g*t2g) + (c2t*c2g*exp) + (c2t*t2g*exp) + (c2g*t2g*exp) + (1 | id),
                          data=d_all,family=binomial(link='logit'))
summary(corrmodel_all_int2)

corrmodel_all_int3 = glmer(corr ~ (c2t + c2g + t2g + exp) + (c2t*c2g) + (c2t*t2g) + (c2g*t2g) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (c2t*c2g*t2g) + (c2t*c2g*exp) + (c2t*t2g*exp) + (c2g*t2g*exp) + (c2t*c2g*t2g*exp) + (1 | id),
                           data=d_all,family=binomial(link='logit'))
summary(corrmodel_all_int3)

ptsimtable_all <- tab_model(corrmodel_all,
                            corrmodel_all_int,
                            corrmodel_all_int2,
                            corrmodel_all_int3,
                            pred.labels = c('(Intercept)',
                                            'Cue-to-Target Similarity',
                                            'Cue-to-Guess Similarity',
                                            'Guess-to-Target Similarity',
                                            'Experiment 2',
                                            'Cue-to-Target * Cue-to-Guess',
                                            'Cue-to-Target * Guess-to-Target',
                                            'Cue-to-Guess * Guess-to-Target',
                                            'Cue-to-Target * Experiment 2',
                                            'Cue-to-Guess * Experiment 2',
                                            'Guess-to-Target * Experiment 2',
                                            'Cue-Target * Cue-Guess * Guess-Target',
                                            'Cue-Target * Cue-Guess * Experiment 2',
                                            'Cue-Target * Guess-Target * Experiment 2',
                                            'Cue-Guess * Guess-Target * Experiment 2',
                                            'Cue-Target * Cue-Guess * Guess-Target * Experiment 2'),
                            dv.labels = c('Model 1','Model 2',
                                          'Model 3','Model 4'),
                            transform = NULL,
                            show.r2 = F,
                            show.re.var = F,
                            show.ngroups = F,
                            show.obs = F,
                            show.icc = F,
                            file = 'TABLE12.html'
)

ptsimtable_all

## TABLE 13

interference_main_all = glmer(corr ~ corr_g + c2t + c2g + t2g + exp + (1|id), data = d_all,family=binomial(link='logit'))
summary(interference_main_all)

interference_int_all1 = glmer(corr ~ corr_g + c2t + c2g + t2g + exp + (corr_g*c2t) + (corr_g * c2g)  + (c2t*c2g) + (corr_g*t2g) + (c2t*t2g) + (c2g*t2g) + (corr_g*exp) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (1|id),
                              data = d_all,family=binomial(link='logit'))
summary(interference_int_all)

interference_int_all2 = glmer(corr ~ corr_g + c2t + c2g + t2g + exp + (corr_g*c2t) + (corr_g * c2g)  + (c2t*c2g) + (corr_g*t2g) + (c2t*t2g) + (c2g*t2g) + (corr_g*exp) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (corr_g*c2t*c2g) + (corr_g*c2t*t2g) + (corr_g*c2g*t2g) + (c2t*c2g*t2g) + (corr_g*c2t*exp) + (corr_g*c2g*exp) + (c2t*t2g*exp) + (c2g*t2g*exp) + (1|id),
                              data = d_all,family=binomial(link='logit'))
summary(interference_int_all2)

interference_int_all3 = glmer(corr ~ corr_g + c2t + c2g + t2g + exp + (corr_g*c2t) + (corr_g * c2g)  + (c2t*c2g) + (corr_g*t2g) + (c2t*t2g) + (c2g*t2g) + (corr_g*exp) + (c2t*exp) + (c2g*exp) + (t2g*exp) + (corr_g*c2t*c2g) + (corr_g*c2t*t2g) + (corr_g*c2g*t2g) + (c2t*c2g*t2g) + (corr_g*c2t*exp) + (corr_g*c2g*exp) + (c2t*t2g*exp) + (c2g*t2g*exp) + (corr_g*c2t*c2g*t2g) + (corr_g*c2t*c2g*exp) + (corr_g*c2t*t2g*exp) + (corr_g*c2g*t2g*exp) + (c2t*c2g*t2g*exp) + (1|id),
                             data = d_all,family=binomial(link='logit'))
summary(interference_int_all3)

interference_main_all4 = glmer(corr ~ corr_g * c2t * c2g * t2g * exp + (1|id), data = d_all,family=binomial(link='logit'))
summary(interference_main_all4)

inttable_all <- tab_model(interference_main_all,
                          interference_int_all,
                          interference_int_all1,
                          interference_int_all2,
                          interference_int_all3,
                          interference_main_all4,
                          pred.labels = c('(Intercept)',
                                          'Guess Recall Accuracy',
                                          'Cue-to-Target Similarity',
                                          'Cue-to-Guess Similarity',
                                          'Guess-to-Target Similarity',
                                          'Experiment 2',
                                          'Guess Recall * Cue-Target',
                                          'Guess Recall * Cue-Guess',
                                          'Cue-to-Target * Cue-to-Guess',
                                          'Guess Recall * Guess-Target',
                                          'Cue-to-Target * Guess-to-Target',
                                          'Cue-to-Guess * Guess-to-Target',
                                          'Guess Recall * Experiment 2',
                                          'Cue-to-Target * Experiment 2',
                                          'Cue-to-Guess * Experiment 2',
                                          'Guess-to-Target * Experiment 2',
                                          'Guess Recall * Cue-Target * Cue-Guess',
                                          'Guess Recall * Cue-Target * Guess-Target',
                                          'Guess Recall * Cue-Guess * Guess-Target',
                                          'Cue-Target * Cue-Guess * Guess-Target',
                                          'Guess Recall * Cue-Target * Experiment 2',
                                          'Guess Recall * Cue-Guess * Experiment 2',
                                          'Cue-Target * Cue-Guess * Experiment 2',
                                          'Guess Recall * Guess-Target * Experiment 2',
                                          'Cue-Target * Guess-Target * Experiment 2',
                                          'Cue-Guess * Guess-Target * Experiment 2',
                                          'Guess Recall * Cue-Target * Cue-Guess * Guess-Target',
                                          'Guess Recall * Cue-Target * Cue-Guess * Experiment 2',
                                          'Guess Recall * Cue-Target * Guess-Target * Experiment 2',
                                          'Guess Recall * Cue-Guess * Guess-Target * Experiment 2',
                                          'Cue-Target * Cue-Guess * Guess-Target * Experiment 2',
                                          'Guess Recall*Cue-Target*Cue-Guess*Guess-Target*Experiment 2'),
                          dv.labels = c('Model 1','Model 2','Model 3', 'Model 4', "Model 5", "Model 6"),
                          transform = NULL,
                          show.r2 = F,
                          show.re.var = F,
                          show.ngroups = F,
                          show.obs = F,
                          show.icc = F,
                          file = 'TABLE13.html'
)

inttable_all
