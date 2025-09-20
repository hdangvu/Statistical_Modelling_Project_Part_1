bixi5_part1 <- read.csv("bixi5_part1.csv")

#Add Weekend var: 0 or 1
bixi5_part1$weekend <- ifelse(bixi5_part1$jj %in% c(6, 7), 1, 0)

#Add Rain Var:0 or 1 
bixi5_part1$Rain <- ifelse(bixi5_part1$precip > 0, 1, 0)

#Add season var 1(Winter:12,1,2) 2(spring:3,4,5) 3(summer:6,7,8) 4(Fall:9,10,11)
season_num <- ifelse(bixi5_part1$mm %in% c(12, 1, 2), 1,
                     ifelse(bixi5_part1$mm %in% c(3, 4, 5),  2,
                            ifelse(bixi5_part1$mm %in% c(6, 7, 8),  3, 4)))

bixi5_part1$season <- season_num

#Go through the adding var
head(bixi5_part1[, c("mm","jj","precip","Rain","weekend","season")])

#Cut the vars we don't need
model_df <- subset(
  bixi5_part1,
  select = -c(station, arrondissement, lat, long, mm, jj)
)

#Encoder season to category variable (Cuz it is not binary)
model_df$season <- factor(model_df$season, levels = c(1,2,3,4))

#Base model:binary "rain or not" and precip together 
m_base <- lm(dur ~ temp + Rain + precip + weekend + season, data = model_df)
summary(m_base)

#Global F Test
null <- lm(dur ~ 1, data = model_df)    # intercept-only 模型
anova(null, m_base)                      # null -> m_base (F and p)                      

#Variable“global effects”（Type II F）
#Loading required package: carData
library(car)
Anova(m_base) 

res.dat <- cbind(model_df,
                 fitted = fitted,
                 resid  = rstud) 

library(ggplot2)
ggplot(res.dat, aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.5) +
  geom_density()

ggplot(res.dat, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE)

ggplot(res.dat, aes(sample = resid)) +
  stat_qq() + stat_qq_line()


#interact model:same time, include rainfall amount and rain or not 0/1
#m_inter <- lm(dur ~ temp + precip * Rain + weekend + season, data = model_df)
#I don't find a good way to do the interact model so I put aside
