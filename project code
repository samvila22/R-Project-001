# R-Project-001
In this article, we'll be analyzing the results of 24578 NBA basketball games over the years 2004-2020. We'll be looking at statistics such as Home and Away Points, Home and Away Assists, Home and Away Rebounds, and Home and Away fg% and 3pfg% as well as comparing some of these sample statistics to their league average.

The main goal of this article is to assess whether the home team truly has an advantage over the away team in the NBA, as well as investigate whether scoring, rebounding, assisting, or efficiency is correlated or associated between both teams. 
```{r, echo=FALSE, include=FALSE}
games <- read.csv("~/Downloads/games.csv")
library(flipTime)
library(lubridate)
library(tidyverse)
library(modelr)
library(infer)
library(devtools)
```

```{r, echo=FALSE}
games$GAME_DATE_EST <- str_remove_all(games$GAME_DATE_EST, "-")
games$GAME_DATE_EST <- ymd(games$GAME_DATE_EST)
games <- games %>%
  select(GAME_DATE_EST|PTS_home|PTS_away|FG_PCT_home|FG_PCT_away|FG3_PCT_home|FG3_PCT_away|AST_home|AST_away|REB_home|REB_away|HOME_TEAM_WINS) %>%
  mutate(
    winning_team = case_when(
      HOME_TEAM_WINS == 1 ~ "HOME",
      HOME_TEAM_WINS == 0 ~ "AWAY"
    ),
    diff = PTS_home - PTS_away
  ) %>%
  drop_na()
```

## Since our total time interval is from 2004-2020 and this is such a long time frame, we are going to make sure that changes in league performance over time does not potentially affect our analysis of home and away team statistics.


First, we'll be breaking down our data into 3 5-year time intervals and assessing whether there are any significant periods worth looking at independently. We'll start by looking at home points:
```{r, echo=FALSE}
data1 <- data.frame(x = rnorm(8458, 100, 12))
interval1 <- interval(mdy("01012015"), mdy("01012020"))
interval_20152020 <- games %>%
  filter(GAME_DATE_EST %within% interval1 == TRUE)
interval2 <- interval(mdy("01012010"), mdy("01012015"))
interval_20102015 <- games %>%
  filter(GAME_DATE_EST %within% interval2 == TRUE)
interval3 <- interval(mdy("01012004"), mdy("01012010"))
interval_20042010 <- games %>%
  filter(GAME_DATE_EST %within% interval3 == TRUE)
p <- ggplot(data = interval_20152020) +
  geom_bar(mapping = aes(x = PTS_home)) 
  p + labs(title="Plot of Home Points over 2015-2020",
        x ="Home Points", y = "Count")
l <- ggplot(data = interval_20102015) +
  geom_bar(mapping = aes(x = PTS_home)) 
  l + labs(title="Plot of Home Points over 2010-2015",
        x ="Home Points", y = "Count")
m <- ggplot(data = interval_20042010) +
  geom_bar(mapping = aes(x = PTS_home)) 
  m + labs(title="Plot of Home Points over 2004-2010",
        x ="Home Points", y = "Count") 
```

Center of 2004-2010 interval:
```{r, echo=FALSE}
mean(interval_20042010$PTS_home)
```
Center of 2010-2015 interval:
```{r, echo=FALSE}
mean(interval_20102015$PTS_home)
```
Center of 2015-2020 interval:
```{r, echo=FALSE}
mean(interval_20152020$PTS_home)
```
Graph of normal distribution with mean of 100 and sd of 12:
```{r, echo=FALSE}
df1 <- data.frame(x = rnorm(1000, 100, 12))
ggplot(df1, aes(x)) + 
  geom_histogram(bins = 40, aes(y = ..density..), color="black", fill="white") + xlim(35, 170)
```


All 3 graphs show home points looking normally distributed. We can see from comparing the normal distribution with similar mean and sd to our 3 interval graphs of PTS that they do in fact resmeble a normal distribution. This finding is notable because we now know there is no bias in our bell shaped distribution. We can also make regular infrences on significance. The latest 5 years appear to yield the highest home points at 107 per game compared to the previous 10 years that yields around 100 home points per game on average. However, none of these values are over a standard deviation away from one another and aren't significant. 

Let's also analyze away points:

```{r, echo=FALSE}
e <- ggplot(data = interval_20152020) +
  geom_bar(mapping = aes(x = PTS_away)) 
  e + labs(title="Plot of Away Points over 2015-2020",
        x ="Away Points", y = "Count")
f <- ggplot(data = interval_20102015) +
  geom_bar(mapping = aes(x = PTS_away)) 
  f + labs(title="Plot of Away Points over 2010-2015",
        x ="Away Points", y = "Count")
g <- ggplot(data = interval_20042010) +
  geom_bar(mapping = aes(x = PTS_away)) 
  g + labs(title="Plot of Away Points over 2004-2010",
        x ="Away Points", y = "Count")
```

Center of 2004-2010 interval:
```{r, echo=FALSE}
mean(interval_20042010$PTS_away)
```
Center of 2010-2015 interval:
```{r, echo=FALSE}
mean(interval_20102015$PTS_away)
```
Center of 2015-2020 interval:
```{r, echo=FALSE}
mean(interval_20152020$PTS_away)
```
Graph of normal distribution with mean of 100 and sd of 12:
```{r, echo=FALSE}
df1 <- data.frame(x = rnorm(1000, 100, 12))
ggplot(df1, aes(x)) + 
  geom_histogram(bins = 40, aes(y = ..density..), color="black", fill="white") + xlim(35, 170)
```

Similar to home points, away points are distributed normally with none of the 3 time intervals having significant mean values compared to the total 16 years time. It is looking like we will be able to analyze this whole dataset within all 16 years for association but lets check one more variable to confirm that our 3 intervals are similar enough. 

Checking percent of home team winning:

2004-2010:
```{r, echo=FALSE}
winning_num_1 <-interval_20042010 %>%
  filter(winning_team == "HOME") %>%
  count()
home_team_winning_percent_20042010 <- winning_num_1/(count(interval_20042010))
winning_num_2 <-interval_20102015 %>%
  filter(winning_team == "HOME") %>%
  count()
home_team_winning_percent_20102015 <- winning_num_2/(count(interval_20102015))
winning_num_3 <-interval_20152020 %>%
  filter(winning_team == "HOME") %>%
  count()
home_team_winning_percent_20152020 <- winning_num_3/(count(interval_20152020))
```

```{r, echo=FALSE}
home_team_winning_percent_20042010
```
2010-2015:
```{r, echo=FALSE}
home_team_winning_percent_20102015
```
2015-2020:
```{r, echo=FALSE}
home_team_winning_percent_20152020
```
All these values are within .02 of one another and hold our previous assumption that these 3 intervals may be treated as the same since none of them are significant to one another. 


After assessing multiple statistics, we can accurately conclude that none of these 5-year intervals are worth looking at independently. We'll be carrying out our further tests on all the data from 2004-2020.



## Is there an association and/or correlation between production of the home team and the production of the away team?


Let's start by checking association and correlation of variables for home and away teams:

Is there an association or correlation between REB of home and away teams?
```{r, echo=FALSE, warning=FALSE}
games %>%
  ggplot(aes(x = REB_home, y = REB_away)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title="Association of Home and Away rebounds over the 2004-2020 NBA seasons",
        x ="REB home", y = "REB away")  
my_model <- games %>%
  lm(REB_home ~ REB_away, data = .)

my_model
summary(my_model)
broom::tidy(my_model)
```
We see a very small R-squared so there is a weak correlation of home and away rebounds. Getting rebounds for either the home or away team is not correlated with the opposing team getting rebounds as well. However, there is an association between these 2 variables based on our small p-value. Let's check assists. 

Is there an association and/or correlation between Assists of home and away teams?
```{r, echo=FALSE, warning=FALSE}
games %>%
  ggplot(aes(x = AST_home, y = AST_away)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title="Association of Home and Away assists over the 2004-2020 NBA seasons",
        x ="AST home", y = "AST away")
my_model <- games %>%
  lm(AST_home ~ AST_away, data = .)

my_model
summary(my_model)
broom::tidy(my_model)
```
No strong correlation of home and away assists with an R-squared of .01981. Getting assists for either the home or away team is also not correlated with the opposing team getting assists. Again though, there is an association between the home and away team's assists based on our F stat of 497 and our p-value basically being zero.

Let's look at points:
```{r, echo=FALSE, warning=FALSE}
games %>%
  ggplot(aes(x = PTS_home, y = PTS_away)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title="Association of Home and Away Points over the 2004-2020 NBA seasons",
        x ="Points home", y = "Points away")
my_model <- games %>%
  lm(PTS_home ~ PTS_away, data = .)


my_model
summary(my_model)
broom::tidy(my_model)
```
R-squared is not high enough to assume a strong correlation. However, with an extremely small p-value and large F-stat we can assume association between home and away team points. 

Lets check fg%:
```{r, echo=FALSE, warning=FALSE}
games %>%
  ggplot(aes(x = FG_PCT_home, y = FG_PCT_away)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title="Association of Home and Away FG% over the 2004-2020 NBA seasons",
        x ="FG% home", y = "FG% away")
my_model <- games %>%
  lm(FG_PCT_home ~ FG_PCT_away, data = .)


my_model
summary(my_model)
broom::tidy(my_model)
```
Very weak correlation between home and away FG%. Higher fg% for either the home or away team is also not correlated with the opposing team getting a higher fg%. F-stat of 43.12 means that we do have an association though.

Let's look at 3pfg%:
```{r, echo=FALSE, warning=FALSE}
games %>%
  ggplot(aes(x = FG3_PCT_home, y = FG3_PCT_away)) +
  geom_point() +
  stat_smooth(method = "lm") + labs(title="Association of Home and Away 3FG% over the 2004-2020 NBA seasons",
        x ="3FG% home", y = "3FG% away")
my_model <- games %>%
  lm(FG3_PCT_home ~ FG3_PCT_away, data = .)


my_model
summary(my_model)
broom::tidy(my_model)
```
R-squared for correlation of 3p% between home and away team is incredibly insignificant. There is no correlation between the home team and away team shooting from the 3 point line. However, we can assume these 2 statistics are independent because we got a very large p-value and therefore there is no association.


None of these 5 main statistics (fg% 3pfg% PTS AST REB) are correlated between home and away team. The home team and away team's basketball production are never correlated with one another. On the other hand, 3pfg% is the only statistic of these 5 that has no association between home and away team, meaning that PTS, AST, REB, and fg% of the home team all have statistical dependence to the PTS, AST, REB, and fg% of the away team. The only truly independent statistic is 3pfg%.



# Home Team Advantage?


Function to calculate fg% and 3fg% above or below league average that a team shot on a particular night. 
```{r, echo=FALSE}
(mean(interval_20152020$FG_PCT_home) + mean(interval_20152020$FG_PCT_away) + mean(interval_20102015$FG_PCT_home) + mean(interval_20102015$FG_PCT_away) + mean(interval_20042010$FG_PCT_home) + mean(interval_20042010$FG_PCT_away)) / 6
(mean(interval_20152020$FG3_PCT_home) + mean(interval_20152020$FG3_PCT_away) + mean(interval_20102015$FG3_PCT_home) + mean(interval_20102015$FG3_PCT_away) + mean(interval_20042010$FG3_PCT_home) + mean(interval_20042010$FG3_PCT_away)) / 6
```
League average over 2004-2020 for fg% is .45425 and for 3fg% is .3523
```{r, echo=FALSE}

League_AVG_residual <- function(fg_percent, reg_or_3p){
  if (reg_or_3p == "reg"){
    resid <- fg_percent - .45425
  } else if(reg_or_3p == "3p"){
    resid <- fg_percent - .3523 
  } else{
    stop("please enter reg or 3p")
  }
  
  return(resid)
}

games <- games %>%
  mutate(
    home_fg_percent_resid_to_league_AVG = map_dbl(FG_PCT_home, ~League_AVG_residual(.x, "reg")),
    away_fg_percent_resid_to_league_AVG = map_dbl(FG_PCT_away, ~League_AVG_residual(.x, "reg")),
    home_3fg_percent_resid_to_league_AVG = map_dbl(FG3_PCT_home, ~League_AVG_residual(.x, "3p")),
    away_3fg_percent_resid_to_league_AVG = map_dbl(FG3_PCT_away, ~League_AVG_residual(.x, "3p"))
  )

interval1 <- interval(mdy("01012015"), mdy("01012020"))
interval_20152020 <- games %>%
  filter(GAME_DATE_EST %within% interval1 == TRUE)
interval2 <- interval(mdy("01012010"), mdy("01012015"))
interval_20102015 <- games %>%
  filter(GAME_DATE_EST %within% interval2 == TRUE)
interval3 <- interval(mdy("01012004"), mdy("01012010"))
interval_20042010 <- games %>%
  filter(GAME_DATE_EST %within% interval3 == TRUE)
```

Let's look at the home and away fg% and 3pfg% residuals for our 2004-2020 sample:

Home fg%:
```{r, echo=FALSE}
mean(games$home_fg_percent_resid_to_league_AVG)
```
Away fg%:
```{r, echo=FALSE}
mean(games$away_fg_percent_resid_to_league_AVG)
```
Home 3pfg%:
```{r, echo=FALSE}
mean(games$home_3fg_percent_resid_to_league_AVG)
```
Away 3pfg%:
```{r, echo=FALSE}
mean(games$away_3fg_percent_resid_to_league_AVG)
```


Not surprisingly, the home team shot above league average for fg% and 3fg% whereas the away team shot worse than league average for fg% and 3fg%. It's worth noting that the home team benfitted more than the away team suffered in these statistics. 

Let's try bootstrapping these values for a 95% confidence interval and seeing if they yield a different result than a t test 95% confidence interval.

Home fg% residual to league average:
```{r, echo=FALSE}
mean_test_home_fg <- games %>%
  t_test(response = home_fg_percent_resid_to_league_AVG)
mean_test_home_fg %>% select(lower_ci, upper_ci)
```
Away fg% residual to league average:
```{r, echo=FALSE}
mean_test_away_fg <- games %>%
  t_test(response = away_fg_percent_resid_to_league_AVG)
mean_test_away_fg %>% select(lower_ci, upper_ci)
```
Home 3pfg% residual to league average:
```{r, echo=FALSE}
mean_test_home_3fg <- games %>%
  t_test(response = home_3fg_percent_resid_to_league_AVG)
mean_test_home_3fg %>% select(lower_ci, upper_ci)
```
Away 3pfg% residual to league average:
```{r, echo=FALSE}
mean_test_away_3fg <- games %>%
  t_test(response = away_3fg_percent_resid_to_league_AVG)
mean_test_away_3fg %>% select(lower_ci, upper_ci)
```

None of these confidence intervals overlap zero. Worth noting.

Let's check bootstrapping now:

Home fg% residual to league average:
```{r, echo=FALSE}
my_boots <- games %>%
  bootstrap(100)
my_boots <- my_boots %>%
  mutate(
    boot_home_fg = map_dbl(strap, ~mean(data.frame(.x)$home_fg_percent_resid_to_league_AVG)),
    boot_away_fg = map_dbl(strap, ~mean(data.frame(.x)$away_fg_percent_resid_to_league_AVG)),
    boot_home_3fg = map_dbl(strap, ~mean(data.frame(.x)$home_3fg_percent_resid_to_league_AVG)),
    boot_away_3fg = map_dbl(strap, ~mean(data.frame(.x)$away_3fg_percent_resid_to_league_AVG))
  )
my_boots %>%
  pull(boot_home_fg) %>%
  quantile(c(.025, .975))
```
Away fg% residual to league average:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_away_fg) %>%
  quantile(c(.025, .975))
```
Home 3pfg% residual to league average:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_home_3fg) %>%
  quantile(c(.025, .975))
```
Away 3pfg% residual to league average:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_away_3fg) %>%
  quantile(c(.025, .975))
```


The bootstrapping and t-test confidence intervals are both very similar and confirm our previous assumptions. Because our 95% confidence intervals do not contain 0, it is likely that, in a typical away game, the away team will have a poor fg% and 3pfg% compared to league average AND there it is likely that, in a typical home game, the home team will have a good fg% and 3pfg% compared to league average. These fg% and 3pfg% statistics give us strong evidence that the home team will have an advantage over the away team but let's check ASTs REBs and Points as well. 

Points, ASTs, and REBs t test 95% confidence intervals:

Home Points:
```{r, echo=FALSE}
mean_test_home_PTS <- games %>%
  t_test(response = PTS_home)
mean_test_home_PTS %>% select(lower_ci, upper_ci)
```
Away Points:
```{r, echo=FALSE}
mean_test_away_PTS <- games %>%
  t_test(response = PTS_away)
mean_test_away_PTS %>% select(lower_ci, upper_ci)
```
Home Assists:
```{r, echo=FALSE}
mean_test_home_AST <- games %>%
  t_test(response = AST_home)
mean_test_home_AST %>% select(lower_ci, upper_ci)
```
Away Assists:
```{r, echo=FALSE}
mean_test_away_AST <- games %>%
  t_test(response = AST_away)
mean_test_away_AST %>% select(lower_ci, upper_ci)
```
Home Rebounds:
```{r, echo=FALSE}
mean_test_home_REBs <- games %>%
  t_test(response = REB_home)
mean_test_home_REBs %>% select(lower_ci, upper_ci)
```
Away Rebounds:
```{r, echo=FALSE}
mean_test_away_REBs <- games %>%
  t_test(response = REB_away)
mean_test_away_REBs %>% select(lower_ci, upper_ci)
```

The 95% t-test confidence intervals for all 3 of these statistics all favor the home team and all do not overlap between the home and away team. 

Let's look at a bootstrapped 95% confidence interval:

Home Points:
```{r, echo=FALSE}
my_boots <- games %>%
  bootstrap(100)
my_boots <- my_boots %>%
  mutate(
    boot_home_PTS = map_dbl(strap, ~mean(data.frame(.x)$PTS_home)),
    boot_away_PTS = map_dbl(strap, ~mean(data.frame(.x)$PTS_away)),
    boot_home_AST = map_dbl(strap, ~mean(data.frame(.x)$AST_home)),
    boot_away_AST = map_dbl(strap, ~mean(data.frame(.x)$AST_away)),
    boot_home_REB = map_dbl(strap, ~mean(data.frame(.x)$REB_home)),
    boot_away_REB = map_dbl(strap, ~mean(data.frame(.x)$REB_away))
  )
my_boots %>%
  pull(boot_home_PTS) %>%
  quantile(c(.025, .975))
```
Away Points:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_away_PTS) %>%
  quantile(c(.025, .975))
```
Home Assists:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_home_AST) %>%
  quantile(c(.025, .975))
```
Away Assists:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_away_AST) %>%
  quantile(c(.025, .975))
```
Home Rebounds:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_home_REB) %>%
  quantile(c(.025, .975))
```
Away Assists:
```{r, echo=FALSE}
my_boots %>%
  pull(boot_away_REB) %>%
  quantile(c(.025, .975))
```
Once again favoring home team and no overlap.

We can accurately conclude that the home team, in a typical game, is much more likely to record more PTS, REBs, and ASTs than the away team based on the 95% confidence intervals we got from t-test and bootstrapping.



# Conclusion


1. None of time intervals are significant from one another. We analyzed home points, away points, and home team winning percentage over all 3 5-year time intervals and found no signifcance in any of the 3. It is worth mentioning that we saw the most points scored by a nearly 7 point margin from the 2015-2020 time interval, but this interval also saw the highest away points and the lowest home team win percentage. We also saw from comparing a normal distribution to our graphs of home and away points that all our intervals are approximately normally distributed. This allows us to make significance assumptions about our data and move forward knowing there's no bias in our data. 
2. After assessing the 5 main variables that go into each basketball game played by the home and away teams, we saw that none of the home and away team statistics are correlated. Points, ASTs, REBs, fg%, and 3pfg% are all not correlated when compared with the home and away teams. We can generalize to all NBA games played in 2004-2020 that none of the major statistics have this correlation. If the home team and away score off of one another, we know that doesn't neccesarily mean they're because of one another. If an NBA announcer were to make an assumption that home and away teams run off one another in terms of AST REB or Point production, our data can prove that statement and that announcer as bogus. 
Although none of the variables have correlation between home and away team, we did see an association between all 4 other variables besides 3pfg%. This means that PTS, AST, REB, and fg% are all dependent when compared across the home and away teams. The only truly indepedent variable in our home v away team context is 3pfg%. 
3. Now that we know home and away team's productions are not correlated with one another, our next question is to find out whether the home team truly has an advantage over the away team. Because our 95% confidence intervals for home and away for all 5 notable basketball statistics(PTS AST REB fg% 3pfg%) all favor the home team and do not overlap, we have sufficient evidence that, in a typical NBA game, the home team is more likely to produce better stats than the away team. This conclusion is further backed by our confidence interval of fg% and 3pfg% of the home and away team compared to league average, which shows that the true average shooting percentages of NBA home teams is above average and the true average shooting percentages of NBA away teams is below average. 


In summary, through the data we have seen above, we conclude that: 1. the 2004-2020 NBA game has statistically stayed the same and there's no need to independently assess any time period within this range. 2. PTS, AST, REB, fg%, and 3pfg% are not correlated between the home and away team. The home team getting points does not mean the away team gets points and so on for all of these statistics. All of these statistics are dependent of the other team's production however, with the exception of 3pfg% which has no association between home and away team. 3. The home team has a statistical advantage over the away team and it's reasonable to assume that the home team will produce better stats.
