require(readr)
require(tidyverse)
require(ggplot2)
require(emmeans)
require(patchwork)

# read data -- different assumed infestation start dates 
data <- read_csv("data/science_pub_data.csv")
data_2001 <- read_csv("data/science_pub_data_2001-start.csv")
data_2000 <- read_csv("data/science_pub_data_2000-start.csv")
data_1999 <- read_csv("data/science_pub_data_1999-start.csv")

new_data_2000 <- read_csv("data/updated_lice_data_2000.csv")

# older data -- do not use --------
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
#log(n(t)/n(t-2)) = r-bn(t-2)
#log(R/S) = a-bS

# random affects on both r and b (a and b) -- didn't end up using 
#log(n(t)/n(t-2)) = (r+mur)-(b+mub)*n(t-2)

# 2002 start year (base case from paper) ----------------------------
# Mutate data 
df <- data %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lead(R,1), # because it's every two years 
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

df.1 <- df %>% filter(group!="fallow")

# Fit a combined model
model <- lm(lnrs ~ S + group, data = df.1)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

# CORRECT 2001 start year ----------------------------

# # THIS IS CORRECT USE THIS # # 
# Mutate data 
df <- data %>%
  group_by(population) %>%
  arrange(population, year) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S)) %>%
  ungroup()

 # # THIS IS CORRECT USE THIS # # 
# Mutate data 
df_2001 <- data_2001 %>%
  group_by(population) %>%
  arrange(population, year) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S)) %>%
  ungroup()

# # THIS IS CORRECT USE THIS # # 
# Mutate data 
df_2000 <- data_2000 %>%
  group_by(population) %>%
  arrange(population, year) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S)) %>%
  ungroup()

check2001 <- df_2001 %>% filter(group == "lice")

df_2001 %>% filter(year == 2001) %>%
  filter(group == "lice")

check <- df_2001 %>% filter(population %in% c(202,204,206,208,210,212,214))

check2 <- data_2001 %>% filter(population %in% c(202,204,206,208,210,212,214))
df_2001 %>% filter(population %in% c(202))


# Don't group by population !!!!!!!!!!!!!!!!!!
df_2001 <- data_2001 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

df_2001 <- df_2001 %>%
  filter(group != "fallow") 

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
         S = lead(R,1), # because it's every two years 
         lnrs = log(R/S))

df_2000 <- df_2000 %>%
  filter(group != "fallow")

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
df_1999 <- data_1999 %>%
  group_by(population) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lead(R,1), # because it's every two years 
         lnrs = log(R/S)) %>%
  filter(group != "fallow")


# Fit a combined model
model <- lm(lnrs ~ S + group, data = df_2000)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)


# reprocess data ----------------
data <- read_csv("data/science_pub_data.csv")

data_2001 <- data %>%
  mutate(group = case_when(
    year == 2001 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

# this doesn't do it 
data_2000 <- data %>%
  mutate(group = case_when(
    year == 2001 ~ "lice",
    year == 2000 ~ "lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

# FIG 1. plot all years ---------

#2002
# Faceted plot (3 groups separately)
p1 <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  ylim(-8,8) +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
p2 <- df %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Stack them
p1 / p2   # patchwork syntax: top / bottom

# 2001
p2001.1 <- df_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  ylim(-8,8) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
p2001.2 <- df_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Stack them
p2001.1 / p2001.2   # patchwork syntax: top / bottom

# 2000
p2000.1 <- df_1999 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  ylim(-8,8) +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
p2000.2 <- df_1999 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = S, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Stack them
p2000.1 / p2000.2   


(p1 / p2) | (p2001.1 / p2001.2) | (p2000.1 / p2000.2) 

ggsave(
  filename = "Figures/Fig_3.png",  
  width = 15,               
  height = 10,                 
  dpi = 600                  
)

# updated data -----------------------------

new_data_2000 <- read_csv("data/updated_lice_data_2000.csv")

new_data_2001 <- new_data_2000 %>%
  mutate(group = case_when(
    Year == 2000 ~ "pre_lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

new_data_2002 <- new_data_2000 %>%
  mutate(group = case_when(
    Year == 2000 ~ "pre_lice",
    Year == 2001 ~ "pre_lice",
    TRUE ~ group  # Keep existing values otherwise
  ))

# new
new_data_2002 %>%
  ggplot(aes(x=spawners, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]") +
  theme_classic()

# new starting in 2000
new2000.1 <- new_data_2000 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  ylim(-8,8) +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
new2000.2 <- new_data_2000 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Stack them
new2000.1 / new2000.2   

# new starting in 2001
new2001.1 <- new_data_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  ylim(-8,8) +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
new2001.2 <- new_data_2001 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Stack them
new2001.1 / new2001.2 

# new - starting in 2002
new2002.1 <- new_data_2002 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs)) +
  geom_point(alpha = 0.2, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group, linetype = group), se = FALSE) +
  facet_wrap(vars(group), nrow = 3,
             labeller = labeller(group = c(
               "lice" = "Lice",
               "pre_lice" = "Pre Lice",
               "unexposed" = "Unexposed"))) + 
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  ylim(-8,8) +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) +
  theme(legend.position = "none")

# Combined smooths plot
new2002.2 <- new_data_2002 %>%
  filter(group != "fallow") %>%
  ggplot(aes(x = spawners, y = lnrs, color = group)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = group)) +
  labs(x = "n(t-2)", y = "log[n(t)/n(t-2)]") +
  theme_classic() +
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 4)) +
  theme(legend.position = "none")

# Stack them
new2002.1 / new2002.2  


(new2002.1 / new2002.2) | (new2001.1 / new2001.2) | (new2000.1 / new2000.2)


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
model <- lm(lnrs ~ spawners + group, data = new_data_2002)

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

# even / odd years separately ---------------------

## 2002 start date --------------
# Mutate data 
df.even <- data %>%
  group_by(population) %>%
  filter(odd_even == 2) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

df.odd <- data %>%
  group_by(population) %>%
  filter(odd_even == 1) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot even 2002
even <- df.even %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Even years", color = "") +
  theme_classic()+
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

# plot odd 2002
odd <- df.odd %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Odd years", color = "") +
  theme_classic() +
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

even | odd

# MODEL 
df.even <- df.even %>% filter(group != "fallow")

# run the model 
# Fit a combined model
model <- lm(lnrs ~ S + group, data = df.even)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

## 2001 start date --------------
# Mutate data 
df.even <- data_2001 %>%
  group_by(population) %>%
  filter(odd_even == 2) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

df.odd <- data_2001 %>%
  group_by(population) %>%
  filter(odd_even == 1) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot even 2002
even <- df.even %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Even years", color = "") +
  theme_classic()+
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

# plot odd 2002
odd <- df.odd %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Odd years", color = "") +
  theme_classic() +
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

even | odd

# MODEL 
df.even <- df.even %>% filter(group != "fallow")

# run the model 
# Fit a combined model
model <- lm(lnrs ~ S + group, data = df.odd)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)

## 2000 start date --------------
# Mutate data 
df.even <- data_2000 %>%
  group_by(population) %>%
  filter(odd_even == 2) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

df.odd <- data_2000 %>%
  group_by(population) %>%
  filter(odd_even == 1) %>%
  mutate(R = updated_abundance / mean(updated_abundance, na.rm=T),
         S = lag(R,1), # because it's every two years 
         lnrs = log(R/S))

# plot even 2002
even <- df.even %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Even years", color = "") +
  theme_classic()+
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

# plot odd 2002
odd <- df.odd %>%
  filter(group != "fallow") %>%
  ggplot(aes(x=S, y=lnrs)) +
  geom_point(alpha = 0.1, aes(color = group)) +
  geom_smooth(method = "lm", aes(color = group)) +
  labs(x="n(t-2)",y="log[n(t)/n(t-2)]", title = "Odd years", color = "") +
  theme_classic() +
  ylim(-10,5)+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3)) 

even | odd

# MODEL 
df.even <- df.even %>% filter(group != "fallow")

# run the model 
# Fit a combined model
model <- lm(lnrs ~ S + group, data = df.odd)

# Perform pairwise comparisons of intercepts
emm <- emmeans(model, ~ group)
pairs(emm)


# facet wrap populations 
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



