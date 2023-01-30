#1
#a
library(USgas)
data <- USgas::us_total
#b
library(tsibble)
library(dplyr)
library(feasts)
library(ggplot2)
df <- data %>% filter(state %in% c('Maine','Vermont','New Hampshire','Massachusetts','Connecticut','Rhode Island')) %>% mutate(y = y/1000) %>%
  as_tsibble(key = state,index = year)
#c
autoplot(df,y) + labs(y = "Natural Gas Consumption ('000)",title = 'Natural Gas Consumption by State',x = 'Year')

#2
#a
library(tsibbledata)
df <- tail(aus_production,5*4) %>% select(Gas)
autoplot(df,Gas)
print('Yes, there seems to be both trend and seasonal component in the data')
print('Seasonal Component: Minimal production in Q1 and peak in Q3')
print('Trend Component: Overall the production seems to be increasing year on year')
#b
components_aus <- df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components()
df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components() %>% autoplot() + labs(title = 'Classical Multiplicative Decomposition of Australia Gas Production')
#c
print('Yes, the results are consistent with part a. Gas production trend is increasing and there is seasonality with peaks in Q3 and minimum production in Q1.')
#d
components_aus %>%
  ggplot(aes(x = Quarter)) + 
  geom_line(aes(y = Gas,colour = 'Data')) +
  geom_line(aes(y = season_adjust,colour = 'Seasonally Adjusted')) +
  scale_colour_manual(
    values = c('Red','Blue'),
    breaks = c('Data','Seasonally Adjusted')
  )
#e
df[9,1] <- df[9,1] + 300
components_aus <- df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components()
df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components() %>% autoplot() + labs(title = 'Classical Multiplicative Decomposition of Australia Gas Production after adding an outlier')
components_aus %>%
  ggplot(aes(x = Quarter)) + 
  geom_line(aes(y = Gas,colour = 'Data')) +
  geom_line(aes(y = season_adjust,colour = 'Seasonally Adjusted')) +
  scale_colour_manual(
    values = c('Red','Blue'),
    breaks = c('Data','Seasonally Adjusted')
  )
print('The trend component changes and is no longer consistently increasing. Seasonal component remains unchanged.')
#f
df <- tail(aus_production,5*4) %>% select(Gas)
df[20,1] <- df[20,1] + 300
components_aus <- df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components()
df %>% model(classical_decomposition(Gas,type = 'multiplicative')) %>% components() %>% autoplot() + labs(title = 'Classical Multiplicative Decomposition of Australia Gas Production after adding an outlier')
components_aus %>%
  ggplot(aes(x = Quarter)) + 
  geom_line(aes(y = Gas,colour = 'Data')) +
  geom_line(aes(y = season_adjust,colour = 'Seasonally Adjusted')) +
  scale_colour_manual(
    values = c('Red','Blue'),
    breaks = c('Data','Seasonally Adjusted')
  )
print('Adding the outlier at the end does not change the increasing trend, however, the increase is extreme.')
