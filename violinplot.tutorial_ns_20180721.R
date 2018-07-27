## 20180721 code to make violin plots in R
## written by Nicole Stone for Yu Zhang

#### References with examples I reviewed:
#### http://eamoncaddigan.net/dataviz/r/psych/2015/09/26/violin-plots/
#### https://ggplot2.tidyverse.org/reference/geom_violin.html
#### https://katherinemwood.github.io/post/violins/
#### https://ggplot2.tidyverse.org/reference/geom_violin.html # alex's rec
#### https://www.quora.com/In-R-what-is-the-difference-between-dt-pt-and-qt-in-reference-to-the-student-t-distribution ## info on quantile function qt()

require(gplots)
library(ggplot2)
library(dplyr)

setwd("/Users/nicolestone/Desktop/20180719_yuzhang.violin.plots/") ## CHANGE THIS TO YOUR OWN PATH!!!

ddd = read.csv("WBC_2.csv")
se <- function(x){sd(x)/sqrt(length(x))} # make a standard error function

yudataSummary <- ddd %>% group_by(group) %>% summarize(yudata_mean = mean(value),
            yudata_median = median(value),
            yudata_se = se(value),
            yudata_sd = sd(value),
            yudata_length = length(value),
            yudata_2se = 2*yudata_se,
            Qlow = quantile(value, probs=0.025),
            Qhigh = quantile(value, probs=0.975)
            )

print(yudataSummary) # wow this is a useful tool!!

#new
p = ggplot(ddd, aes(group, value)) + geom_violin(trim = FALSE, fill = "grey75") + 
  geom_point(aes(y = yudata_median), color = "black", size = 2, data = yudataSummary) + 
  geom_errorbar(aes(y = yudata_mean, ymin = Qlow, ymax = Qhigh), color = "black", width = 0.1, data = yudataSummary) + ylim(0, 15) + ggtitle("Violin plots for Yu Zhang!") 

print(p)


