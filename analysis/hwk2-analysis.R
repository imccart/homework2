
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 2 Answers
## Author:        Ian McCarthy
## Date Created:  2/20/2020
## Date Edited:   2/8/2023
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               ggbeeswarm, MatchIt)


# Read data and set workspace for knitr -------------------------------
hcris.data.v1996 <- readRDS('data/HCRIS_Data_v1996.rds')
hcris.data.v2010 <- readRDS('data/HCRIS_Data_v2010.rds')
hcris.data <- readRDS('data/HCRIS_Data.rds')

# Create objects for markdown ---------------------------------------------

version.1996 <- hcris.data.v1996 %>% group_by(year) %>% summarize(count_1996=n())
version.2010 <- hcris.data.v2010 %>% group_by(year) %>% summarize(count_2010=n())
version.dat <- version.1996 %>%
  full_join(version.2010, by='year')

final.hcris.v1996 = hcris.data.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,hcris.data.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

## count of hospitals/provider_number by year
dup.count <- final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports") %>%
  mutate(dup_report = (total_reports>1)) %>%
  group_by(fyear) %>%
  summarize(duplicates=sum(dup_report))

fig.dup <- dup.count %>%
  ggplot(aes(x=as.factor(fyear), y=duplicates, group=1)) + geom_line() +
  labs(
    x="Year",
    y="Number of Hospitals",
    title=""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))  

fig.unique <- hcris.data %>% group_by(year) %>%
  summarize(hosp_count=n()) %>%
  ggplot(aes(x=as.factor(year), y=hosp_count, group=1)) + 
  geom_line() +
  labs(
    x="Year",
    y="Number of Hospitals",
    title=""
  ) + theme_bw() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

## violin plot of total charges
charge.data <- hcris.data %>%
  group_by(year) %>% 
  mutate(tot_charges_low=quantile(tot_charges, probs=0.01, na.rm=TRUE),
            tot_charges_high=quantile(tot_charges, probs=0.99, na.rm=TRUE)) %>%
  filter(tot_charges<tot_charges_high, tot_charges>tot_charges_low,
         !is.na(tot_charges), year>1997) %>%
  mutate(log_charge=log(tot_charges))

fig.totcharge <- charge.data %>%
  ggplot(aes(x=as.factor(year), y=log_charge)) +
  geom_quasirandom(alpha=0.05, width=0.2, color = "#eba487") +
  stat_summary(fun='median', geom='point', size=2, color = "#abdcf1") +
  stat_summary(fun='median', geom='line', aes(group=1), size=1.1, color = "#abdcf1") +
  labs(
    x="Year",
    y="Log $",
    title=""
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))



## Violin plot of prices
price.data <- hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom) %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, !is.na(beds))

fig.prices <- price.data %>%
  ggplot(aes(x=as.factor(year), y=price)) +
  geom_quasirandom(alpha=0.05, width=0.2, color = "#eba487") +
  stat_summary(fun='median', geom='point', size=2, color = "#abdcf1") +
  stat_summary(fun='median', geom='line', aes(group=1), size=1.1, color = "#abdcf1") +
  labs(
    x="Year",
    y="Dollars",
    title=""
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))

## Penalty graph
pen.data <- price.data %>%
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
          hrrp_penalty = as.numeric((hrrp_payment>0)),
          penalty = as.numeric((hvbp_payment-hrrp_payment<0)))

fig.pen <- pen.data %>% group_by(year) %>%
  ggplot(aes(x=as.factor(year), y=hrrp_penalty, group=1)) +
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Percent Penalized",
    title=""
  ) + 
  scale_y_continuous(labels=percent, limits=c(0,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust=1))



## Final data for analysis
pen.data.2012 <- pen.data %>% filter(year==2012) %>% ungroup() %>%
  mutate(beds_q1 = quantile(beds, probs=0.25, na.rm=TRUE),
         beds_q2 = quantile(beds, probs=0.50, na.rm=TRUE),
         beds_q3 = quantile(beds, probs=0.75, na.rm=TRUE),
         beds_q4 = max(beds, na.rm=TRUE)) %>%
  mutate(bed_size1 = ifelse(beds<beds_q1,1,0),
         bed_size2 = ifelse(beds>= beds_q1 & beds<beds_q2,1,0),
         bed_size3 = ifelse(beds>= beds_q2 & beds<beds_q3,1,0),
         bed_size4 = ifelse(beds>  beds_q3 & beds<beds_q4,1,0),
         bed_quart = 1*bed_size1 + 2*bed_size2 + 3*bed_size3 + 4*bed_size4) %>%
  filter(bed_quart>0)


## average price by penalty status
avg.pen1 <- pen.data.2012 %>%
  group_by(penalty) %>%
  summarize(mean_price=mean(price))

avg.pen <- pen.data.2012 %>%
  group_by(penalty, bed_quart) %>% 
  summarize(mean_price=mean(price))

avg.pen.tab <- pivot_wider(avg.pen, names_from=penalty, values_from=mean_price, names_prefix="price_")


## matching
match.inv <- Matching::Match(Y=pen.data.2012$price,
                Tr=pen.data.2012$penalty,
                X= (pen.data.2012 %>% select(bed_size1, bed_size2, bed_size3)),
                M=1,
                Weight=1,
                estimand="ATE")

match.mah <- Matching::Match(Y=pen.data.2012$price,
                          Tr=pen.data.2012$penalty,
                          X= (pen.data.2012 %>% select(bed_size1, bed_size2, bed_size3)),
                          M=1,
                          Weight=2,
                          estimand="ATE")

  
## Propensity scores and IPW
logit.model <- glm(penalty ~ bed_size1 + bed_size2 + bed_size3, 
                   family=binomial, 
                   data=pen.data.2012)
ps <- fitted(logit.model)

pen.data.2012 <- pen.data.2012 %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))

mean.t1 <- pen.data.2012 %>% filter(penalty==1) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- pen.data.2012 %>% filter(penalty==0) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
ipw.diff <- mean.t1$mean_p - mean.t0$mean_p

ipw.reg <- lm(price ~ penalty, data=pen.data.2012, weights=ipw)


## Regression
reg.data <- pen.data.2012 %>% ungroup() %>%
  mutate(size1_diff = penalty*(bed_size1 - mean(bed_size1)),
         size2_diff = penalty*(bed_size2 - mean(bed_size2)),
         size3_diff = penalty*(bed_size3 - mean(bed_size3)))

reg <- lm(price ~ penalty + bed_size1 + bed_size2 + bed_size3 +
            size1_diff + size2_diff + size3_diff,
          data=reg.data)


rm(list=c("version.1996", "version.2010", "pen.data", "pen.data.2012",
          "hcris.data.v1996", "hcris.data.v2010", "charge.data", "price.data",
          "final.hcris.v1996", "final.hcris", "hcris.data", "reg.data"))
save.image("assignments/Hwk2_workspace.Rdata")

