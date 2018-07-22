#https://katherinemwood.github.io/post/violins/

setwd("/Users/nicolestone/Desktop/20180719_yuzhang.violin.plots/")

require('gplots')
library(ggplot2)
library(dplyr)

###I found the template here: https://ggplot2.tidyverse.org/reference/geom_violin.html

ddd = read.csv("WBC_2.csv")

library(ggplot)
#p = ggplot(ddd, aes(group, value, fill=group)) + geom_violin() # alex basic
p = ggplot(ddd, aes(group, value)) + geom_violin(trim = FALSE, fill = "grey75", draw_quantiles = c(0.05, 0.5, 0.95))

print(p)

##
#### http://eamoncaddigan.net/dataviz/r/psych/2015/09/26/violin-plots/

plotViolins <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) + 
  geom_violin(aes(y = mpg, fill = factor(cyl))) + 
  geom_point(aes(y = mpg_mean), color = "black", size = 2, data = mtcarsSummary) + 
  geom_errorbar(aes(y = mpg_mean, ymin = mpg_mean-mpg_se, ymax = mpg_mean+mpg_se), 
                color = "black", width = 0.2, data = mtcarsSummary) + 
  ylim(0, 35) + 
  theme(legend.position = "none") + 
  ggtitle("Violin plot")

##
setwd("/Users/nicolestone/Desktop/20180719_yuzhang.violin.plots/")
df <- read.csv("WBC_2.csv")
head(df)
summary(df)
nummatrix <- as.matrix(df)
head(nummatrix)
dat <- data.frame(df)
set.seed(836)
#dat <- data.frame('condition'=c(rep('t1', 30), rep('t2', 30)), 
                  'value'=c(rnorm(30, 10, 3), rnorm(30, 20, 7)))

basic_violin <- ggplot(data=df, aes(x=df$group, y=df$value)) +
  geom_violin(aes(fill=df$group, color=df$group)) + theme_classic()
  #theme_minimal() 
  #theme_classic()
  #p + theme_void()

print(basic_violin)

####
errbar_lims <- group_by(dat, df$group) %>% 
  summarize(mean=mean(df$value), se=sd(df$value)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))

mean_se_violin <- ggplot() +
  geom_violin(data=dat, aes(x=df$group, y=df$value, fill=df$group, color=df$group)) +
  geom_point(data=errbar_lims, aes(x=df$group, y=mean), size=3) +
  geom_errorbar(aes(x=errbar_lims$(df$group), ymax=errbar_lims$upper, ymin=errbar_lims$lower, stat='identity', width=.25) + theme_minimal()

print(mean_se_violin)


