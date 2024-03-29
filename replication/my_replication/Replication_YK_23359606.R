# Replication files for "Harnessing Backlash" BJPS
# Created 6/29/2022
library(stargazer)
library(ggplot2)
library(sjPlot)

setwd("/Users/yana/Documents/01_TRINITY/01_Statistical Analysis II/06_Replication Presentation/Paper5/Replication/")

# Analysis of American public opinion

load("data/data_US.RDS")

# Figure 1: Change in favorability towards Netanyahu following his speech, by partisanship.
model1 <- glm(favorable_netanyahu~ post_visit*party + male + church_attend + college + income + age + white + ideology + jewish, family=binomial(link='probit'), data=data_US)

plot_model(model1,
           type = "pred", terms = c("party", "post_visit"), axis.labels = c("Democrat", "Independent", "Republican"), line.size = 2, dot.size = 3, linetype = c(1,2)) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c('Republican', 'Independent', 'Democrat'), limits = c(.5, 3.5)) +
  xlab("Respondent's Party") + ylab("Favorable opinion of Netanyahu") + labs(title=NULL) + theme_bw() + scale_color_manual(values=c("#9E9E9E", "#212121")) + 
  theme(legend.title = element_blank(), text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) 

ggsave(file = "figures_rep/Figure1.png", width = 10, height = 6,  dpi=600)


# Figure 2: Change in favorability towards Netanyahu following his speech, by Obama approval.
model2 <- glm(favorable_netanyahu~ post_visit*approve_obama + male + church_attend + college + income + age + white + ideology + jewish, family=binomial(link='probit'), data=data_US)

plot_model(model2,
           type = "pred", terms = c("approve_obama", "post_visit"), line.size = 2, dot.size = 3) +
  scale_x_continuous(breaks=c(1,2), labels=c('Does not approve of Obama', 'Approves of Obama'), limits = c(.5, 2.5)) + xlab("  ") + ylab("Favorable opinion of Netanyahu") + 
  labs(title=NULL)  + theme_bw() + scale_color_manual(values=c("#9E9E9E", "#212121")) + 
  theme(legend.title = element_blank(), text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) 

ggsave(file = "figures_rep/Figure2.png", width = 10, height = 6,  dpi=600)

# Analysis of Israeli public Opinion

load("data/data_INES.RDS")

# Figure 3: Change in support for a Likud-led coalition among right-wing party supporters following Netanyahu's speech
model3 <- lm(Likud ~ vote_2013*after_visit + age + sex + jewish + upper_class, data = data_INES)

plot_model(model3,
           type = "pred", terms = c("vote_2013", "after_visit"), axis.labels = c("Non-coalition", "Likud", "Non-Likud coalition"), line.size = 2, dot.size = 3, ci.lvl= .9) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Non-coalition party", "Likud", "Non-Likud coalition party"), limits = c(.5, 3.5)) +
  xlab("2013 vote") + ylab("Support for Likud-Led coalition") + labs(title=NULL) + theme_bw() + scale_color_manual(values=c("#9E9E9E", "#212121")) + 
  theme(legend.title = element_blank(), text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) 

ggsave(file = "figures_rep/Figure3.png", width = 10, height = 6,  dpi=600)


# Replication code

# Running multiplicative linear model with interaction term 
model_with_interaction <- lm(Likud ~ vote_2013*after_visit + age + sex + jewish + upper_class, data = data_INES)
#summary(model_with_interaction)


# Running additive linear model without interaction term 
model_without_interaction <- lm(Likud ~ vote_2013 + after_visit + age + sex + jewish + upper_class, data = data_INES)
#summary(model_without_interaction)

plot_model(model_without_interaction,
           type = "pred", terms = c("vote_2013", "after_visit"), axis.labels = c("Non-coalition", "Likud", "Non-Likud coalition"), line.size = 2, dot.size = 3, ci.lvl= .9) +
  scale_x_continuous(breaks=c(1, 2, 3), labels=c("Non-coalition party", "Likud", "Non-Likud coalition party"), limits = c(.5, 3.5)) +
  xlab("2013 vote") + ylab("Support for Likud-Led coalition") + labs(title=NULL) + theme_bw() + scale_color_manual(values=c("#9E9E9E", "#212121")) + 
  theme(legend.title = element_blank(), text = element_text(size=20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = , b = 0, l = 0))) 

ggsave(file = "figures_rep/Figure3a.png", width = 10, height = 6,  dpi=600)

# Printing the estimated coefficients of linear model with and without interaction term 
stargazer(model_with_interaction, model_without_interaction)
