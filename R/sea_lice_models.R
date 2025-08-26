require(readr)
require(tidyverse)
require(ggplot2)
require(emmeans)
require(patchwork)

# read data -- different assumed infestation start dates 
data <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data.csv")
data_2001 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data_2001-start.csv")
data_2000 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data_2000-start.csv")

new_data_2000 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/updated_lice_data_2000.csv")

data_1999 <- data_2000 %>%
  mutate(group = case_when(
    year == 1999 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

data_1998 <- data_1999 %>%
  mutate(group = case_when(
    year == 1998 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

data_1997 <- data_1998 %>%
  mutate(group = case_when(
    year == 1997 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

data_1996 <- data_1997 %>%
  mutate(group = case_when(
    year == 1996 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

data_1995 <- data_1996 %>%
  mutate(group = case_when(
    year == 1995 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))


# Ricker stock-recruit equations 
log(n(t)/n(t-2)) = r-bn(t-2)
log(R/S) = a-bS

# random affects on both r and b (a and b) -- didn't end up using 
log(n(t)/n(t-2)) = (r+mur)-(b+mub)*n(t-2)

# 2002 start year (base case from paper) ----------------------------
# Mutate data 
df <- data %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot lnrs vs. S 
df %>%
  filter(group != "fallow") %>%
ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1) +
  facet_grid(vars(group)) + 
  geom_smooth(method = "lm") +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# all on one 
df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  facet_wrap(vars(population)) + 
  theme_classic()

lice <- df %>% filter(group == "lice")
pre_lice <- df %>% filter(group == "pre_lice")
unexposed <- df %>% filter(group == "unexposed")

lice.model <- lm(lnrs ~ S, data = lice)
summary(lice.model)

pre_lice.model <- lm(lnrs ~ S, data = pre_lice)
summary(pre_lice.model)

unexposed.model <- lm(lnrs ~ S, data = unexposed)
summary(unexposed.model)

# compare three models 
# anova(lice.model, pre_lice.model, unexposed.model)

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# 2001 start year ----------------------------

# Mutate data 
df_2001 <- data_2001 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot lnrs vs. S 
df_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1) +
  facet_grid(vars(group)) + 
  geom_smooth(method = "lm") +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# all on one 
df_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

lice <- df %>% filter(group == "lice")
pre_lice <- df %>% filter(group == "pre_lice")
unexposed <- df %>% filter(group == "unexposed")

lice.model <- lm(lnrs ~ S, data = lice)
summary(lice.model)

pre_lice.model <- lm(lnrs ~ S, data = pre_lice)
summary(pre_lice.model)

unexposed.model <- lm(lnrs ~ S, data = unexposed)
summary(unexposed.model)

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df_2001)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# 2000 start year ----------------------------

# Mutate data 
df_2000 <- data_2000 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot lnrs vs. S 
df_2000 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1) +
  facet_grid(vars(group)) + 
  geom_smooth(method = "lm") +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# all on one 
df_2000 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

lice <- df %>% filter(group == "lice")
pre_lice <- df %>% filter(group == "pre_lice")
unexposed <- df %>% filter(group == "unexposed")

lice.model <- lm(lnrs ~ S, data = lice)
summary(lice.model)

pre_lice.model <- lm(lnrs ~ S, data = pre_lice)
summary(pre_lice.model)

unexposed.model <- lm(lnrs ~ S, data = unexposed)
summary(unexposed.model)

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df_2000)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# 1999 start year ----------------------------

# Mutate data 
df_2002 <- data %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))
# all on one 
p.2002 <- df_2002 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

lice <- df %>% filter(group == "lice")
pre_lice <- df %>% filter(group == "pre_lice")
unexposed <- df %>% filter(group == "unexposed")

lice.model <- lm(lnrs ~ S, data = lice)
summary(lice.model)

pre_lice.model <- lm(lnrs ~ S, data = pre_lice)
summary(pre_lice.model)

unexposed.model <- lm(lnrs ~ S, data = unexposed)
summary(unexposed.model)

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df_2000)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# plot all years ----------
p.2002 <- p.2002 + labs(title = "2002")
p.2001 <- p.2001 + labs(title = "2001")
p.2000 <- p.2000 + labs(title = "2000")
p.1999 <- p.1999 + labs(title = "1999")
p.1998 <- p.1998 + labs(title = "1998")
p.1997 <- p.1997 + labs(title = "1997")
p.1996 <- p.1996 + labs(title = "1996")
p.1995 <- p.1995 + labs(title = "1995")


(p.2002 | p.2001) / (p.2000 | p.1999 ) / (p.1998 | p.1997) / (p.1996 | p.1995)

# 2000 start year - UPDATED DATA ----------------------------

# Mutate data 
df_2000 <- data_2000 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
new_data_2000 %>%
  ggplot(aes(x=spawners, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()


# what's the deal - they aggregated? 
# simply do the same analysis as them -- lagged S to get R (crude but just to compare)

lice <- df %>% filter(group == "lice")
pre_lice <- df %>% filter(group == "pre_lice")
unexposed <- df %>% filter(group == "unexposed")

lice.model <- lm(lnrs ~ S, data = lice)
summary(lice.model)

pre_lice.model <- lm(lnrs ~ S, data = pre_lice)
summary(pre_lice.model)

unexposed.model <- lm(lnrs ~ S, data = unexposed)
summary(unexposed.model)

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df_2000)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# Fallow years  ----------------------------
fallow_data <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data_fallow.csv")

# Mutate data 
df <- fallow_data %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot
df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()


# Fit a combined model
model <- lm(lnrs ~ S + group, data = df)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# 2001 start 
fallow_2001 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data_2001-start_fallow.csv")

# Mutate data 
df <- fallow_2001 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot
df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# 2000 start 
fallow_2000 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/science_pub_data_2000-start_fallow.csv")

# Mutate data 
df <- fallow_2000 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot
df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()


# Fit a combined model
model <- lm(lnrs ~ S + group, data = df)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# Updated data from preprint ------------------------------

# Run for even / odd years separately ---------------------

# Mutate data 
df <- data %>%
  group_by(population) %>%
  filter(odd_even == 2) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot even 2002
even <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "even") +
  theme_classic()

# plot odd 2002
odd <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "odd") +
  theme_classic()

even | odd

# 2001
df <- data_2001 %>%
  group_by(population) %>%
  filter(odd_even == 1) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
even <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "even") +
  theme_classic()

odd <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "odd") +
  theme_classic()

even | odd

# 2000
df <- data_2000 %>%
  group_by(population) %>%
  filter(odd_even == 2) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T), # normalize 
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

even <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "even") +
  theme_classic()

odd <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "odd") +
  theme_classic()

even | odd

ggplot(data_2000, aes(x=year,y=updated_abundance,color=as.factor(population))) +
  geom_line() +
  facet_wrap(vars(odd_even))

data_2000 %>%
  group_by(odd_even) %>%
  summarise(mean = mean(updated_abundance, na.rm=T))

# UPDATED data -----------
# reference <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/reference_updated.csv")
# 
# reference <- reference %>%
#   pivot_longer(cols = 2:64, names_to = "population", values_to = "spawners") 
# 
# write_csv(reference,"/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/reference_updated_new.csv" )

# 2002
broughton <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/broughton_updated.csv")

df <- broughton %>%
  group_by(population) %>%
  mutate(R = spawners / mean(spawners, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
df %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# 2001
broughton_2001 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/broughton_updated_2001.csv")

df <- broughton_2001 %>%
  group_by(population) %>%
  mutate(R = spawners / mean(spawners, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
df %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# 2000
broughton_2000 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/broughton_updated_2000.csv")

df <- broughton_2000 %>%
  group_by(population) %>%
  mutate(R = spawners / mean(spawners, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
df %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# 1999
broughton_1999 <- read_csv("/Users/haleyoleynik/Documents/UBC/Sea Lice Analysis/broughton_updated_1999.csv")

df <- broughton_1999 %>%
  group_by(population) %>%
  mutate(R = spawners / mean(spawners, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# all on one 
df %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# redo and make sure the lag is okay 



