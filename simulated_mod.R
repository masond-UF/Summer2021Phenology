
ggplot(d,aes(y=SEEDS,x=TREATMENT,color=PHENOLOGY))+
	geom_boxplot()+
	facet_wrap(~DATE)+
	theme_bw()

d_mean <- group_by(d, DATE, TREATMENT,PHENOLOGY) %>% 
					summarise(mean = mean(SEEDS))

ggplot(d_mean,aes(y=mean,x=DATE,group=PHENOLOGY,color=PHENOLOGY))+
	geom_point()+
	geom_line()+
	facet_wrap(~TREATMENT)+
	theme_bw()

library(lme4)
library(car)

d$obs <- 1:length(d$SEEDS)

mod <- glmer(SEEDS ~ FA * TREATMENT * DATE * PHENOLOGY + 
				(1|LOCATION/PLOT) + (1|obs), family="poisson",data = d)

summary(mod)  ## summary for glmmTMB negative binomial
Anova(mod)  ## Anova table for overdispersed poisson
