library(tidyverse)
library(scales)
library(data.table)
library(sp)
library(rgdal)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Loading the dataset
prison_trends <- read.csv("../docs/incarceration_trends.csv")

# Comparing average black, hispanic and white jail population in the past 20 years across all rural locations

  comparison_rural <- prison_trends %>%
    filter(year > 1995) %>%
    filter(urbanicity == "rural") %>%
    group_by(county_name) %>%
    summarize(av_black = mean(black_jail_pop), av_white = mean(white_jail_pop), av_latinx = mean(latinx_jail_pop)) %>%
    drop_na()


# Comparing average black, hispanic and white jail population in the past 20 years in urban areas
  comparison_urban <- prison_trends %>%
    filter(year > 1995) %>%
    filter(urbanicity == "small/mid") %>%
    group_by(county_name) %>%
    summarize(av_black = mean(black_jail_pop), av_white = mean(white_jail_pop), av_latinx = mean(latinx_jail_pop)) %>%
    drop_na()


# Comparing by how much the sum of averages differ in urban areas between blacks and whites
  average_difference_urban_black_white <- function(){
    average_black <- sum(comparison_urban$av_black)
    average_latin <- sum(comparison_urban$av_latinx)
    average_white <- sum(comparison_urban$av_white) 
    
    average_dif_black_white <- average_white/average_black 
    return(average_dif_black_white)
  }
average_difference_urban_black_white() 
# Comparing by how much the sum of averages differ in urban areas between hispanics and whites
 average_difference_urban_hispanic_white <- function(){
   average_black <- sum(comparison_urban$av_black)
   average_latin <- sum(comparison_urban$av_latinx)
   average_white <- sum(comparison_urban$av_white) 
   
   average_dif_hispanic_white <- average_white/average_latin
   return(average_dif_hispanic_white)
 }
average_difference_urban_hispanic_white() 
 # Comparing by how much the sum of averages differ in rural areas between hispanics and whites
 average_difference_rural_hispanic_white <- function(){
   average_black <- sum(comparison_rural$av_black)
   average_latin <- sum(comparison_rural$av_latinx)
   average_white <- sum(comparison_rural$av_white) 
   
   average_dif_hispanic_white_rural <- average_white / average_latin 
   return(average_dif_hispanic_white_rural)
   
 }
 # Comparing by how much the sum of averages differ in rural areas between blacks and whites
 average_difference_rural_black_white <- function() {
   average_black <- sum(comparison_rural$av_black)
   average_latin <- sum(comparison_rural$av_latinx)
   average_white <- sum(comparison_rural$av_white) 
   
   average_dif_black_white_rural <- average_white/average_black 
   return(average_dif_black_white_rural)
 }
 average_difference_rural_hispanic_white() 
 
# Comparing total populations between blacks  and whites in each small-medium urban area (not just in jail)
  average_black_white_difference_general <- function(){
    general_comparison_urban <- prison_trends %>%
      filter(year > 1995) %>%
      filter(urbanicity == "small/mid") %>%
      group_by(county_name) %>%
      summarize(av_black = mean(black_pop_15to64), av_white = mean(white_pop_15to64), av_latinx = mean(latinx_pop_15to64)) %>%
      drop_na()
    
    average_pop_black <- sum(general_comparison_urban$av_black) 
    average_pop_white <- sum(general_comparison_urban$av_white)
    average_pop_latinx <- sum(general_comparison_urban$av_latinx)
    
    dif <- average_pop_white / average_pop_black 
    return(dif)
  }
 average_black_white_difference_general() 
# Comparing total populations between hispanics and whites in each small-medium urban area (not just in jail)
 average_hispanic_white_difference_general <- function(){
    general_comparison_urban <- prison_trends %>%
      filter(year > 1995) %>%
      filter(urbanicity == "small/mid") %>%
      group_by(county_name) %>%
      summarize(av_black = mean(black_pop_15to64), av_white = mean(white_pop_15to64), av_latinx = mean(latinx_pop_15to64)) %>%
      drop_na()
    
    average_pop_black <- sum(general_comparison_urban$av_black) 
    average_pop_white <- sum(general_comparison_urban$av_white)
    average_pop_latinx <- sum(general_comparison_urban$av_latinx)
    
    dif <- average_pop_white / average_pop_latinx 
    return(dif)
  }
 average_hispanic_white_difference_general()
 

             

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. jail Population
# This section presents two functions to chart the growth of the U.S. jail population
# from 1970 to 2018
#----------------------------------------------------------------------------#
# This function returns a dataframe that shows the growth of the U.S. jail
# population from 1970 to 2018, 
get_year_jail_pop <- function() {
      total_change <- prison_trends %>% 
        group_by(year) %>%
        summarize(change = sum(total_jail_pop, na.rm = TRUE))
return(total_change)   
}
get_year_jail_pop()

# This function plots the given dataframe into a bar chart
plot_jail_pop_for_us <- function()  {
  total_change <- get_year_jail_pop()
  change_plot <- total_change %>%
      ggplot(aes(x = year, y = change)) + 
      geom_bar(stat = 'identity') +
      theme_bw() + 
      scale_y_continuous(label = comma_format(big.mark = ",")) +
      labs(x = "Year", y = "Total jail Population", title = "Increase of jail Population in U.S. (1970-2018)")
  return(change_plot)   
} 

  plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# This function returns a dataframe that shows the growth of the U.S. jail 
# population from 1970 to 2018 by given State(s) as paramater
  get_jail_pop_by_states <- function(states){
    state_change <- prison_trends %>% 
      filter(state %in% states) %>%
      group_by(year, state) %>%
      summarize(change = sum(total_jail_pop, na.rm = TRUE))
    return(state_change)
  }
  
# This function plots a line graph of the growth of the U.S. jail 
# population from 1970 to 2018 by given State(s) as paramater
  plot_jail_pop_by_states <- function(states){
    plot_state_change <- get_jail_pop_by_states(states) %>%
      ggplot(aes(year, change, colour = state))+ 
      geom_line(size = 1)+
      theme_minimal()+ 
      labs(title = "jail Population By State in U.S. (1970 - 2018)", x = "Year", y = "jail Population")
    return(plot_state_change)
    
  }
# Testing plot 
  plot_jail_pop_by_states(c("WA", "AL", "CA", "KY"))
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Function comparing jail population to total population across whites, latinx and blacks since 1995
  compute_comparison <- function(){
  population_comparison <- prison_trends %>% 
    filter(year > 1995) %>% 
    filter(urbanicity == "small/mid") %>% 
    group_by(year) %>% 
    summarize(Total_Black = sum(black_pop_15to64, na.rm = TRUE), Total_White = sum(white_pop_15to64, na.rm = TRUE), 
              Total_Latinx = sum(latinx_pop_15to64, na.rm = TRUE), jailed_Black = sum(black_jail_pop, na.rm = TRUE), 
              jailed_White = sum(white_jail_pop, na.rm = TRUE),
              jailed_Latinx = sum(latinx_jail_pop, na.rm = TRUE)) %>% 
    drop_na() 
    comparison_gathered <- population_comparison %>% 
    gather(key = "Group", value = "Population", Total_Black, Total_White, Total_Latinx, jailed_Black, jailed_White,
           jailed_Latinx)
    return(comparison_gathered)
}
 
# Function creating a bar graph of the jail to general population per race 
  plot_comparison <- function(){
    plot_comparison <- compute_comparison() %>% 
      ggplot(aes(x = Group, y = Population)) + 
      geom_bar(stat = 'identity') + 
      theme_bw() + 
      scale_y_continuous(label = comma_format(big.mark = ",")) +
      labs(x = "Group", y = "Population", title = "Amount Incarcerated vs. Total Population Per Ethnic Group")
  return(plot_comparison)
  }
  
  plot_comparison()
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically
states <- map_data("state")
  
  
# This function finds states that have incarcerated over 1 percent of the total black population
# since 1995, and creates a dataframe. This function returns that dataframe
create_black_coords <- function(){
    inequality <- prison_trends %>% 
      filter(year > 1995) %>% 
      group_by(state) %>%
      summarize(state, black_jailed = sum(black_jail_pop, na.rm = TRUE),
                black_total = sum(black_pop_15to64, na.rm = TRUE)) %>%
      mutate(Percentage_jailed = black_jailed / black_total) %>% 
      filter(Percentage_jailed > 0.01) %>%
      drop_na() %>% 
      distinct()
    
    inequality$region <- state.name[match(inequality$state, state.abb)]
    inequality$region <- tolower(inequality$region)
    
    inequality_ready_for_map <- inequality%>% select(region)
    inequality_ready_for_map$region <- tolower(inequality_ready_for_map$region)
    inequality_ready_for_map <- drop_na(inequality_ready_for_map) 
    return(inequality_ready_for_map)
}
create_black_coords()
# This function finds states that have incarcerated over 1 percent of the total white population
# since 1995 and returns it as a dataframe. 
create_white_coords <- function(){
    white <- prison_trends %>% 
      filter(year > 1995) %>% 
      group_by(state) %>%
      summarize(state, white_jailed = sum(white_jail_pop, na.rm = TRUE),
              white_total = sum(white_pop_15to64, na.rm = TRUE)) %>%
      mutate(Percentage_jailed = white_jailed / white_total) %>% 
      filter(Percentage_jailed > 0.01) %>%
      drop_na() %>% 
      distinct()
    return(white)
}
create_white_coords()

# This function maps the states that have incarcerated over 1 percent of the black population and 
# over 1 percent of the white population(there aren't any) and returns it.
plot_map <- function(){
  
  states <- left_join(states, create_black_coords(), by = "region")
  map <- ggplot(states, aes(x = long, y = lat, group=group)) + 
    geom_polygon(aes(fill = state), color = "black") +
    theme_minimal()+ 
    labs(title = "States that Jailed over 1% of the Black Population since 1995")
  map
  
  return(map)
}
plot_map() 

# See Canvas
#----------------------------------------------------------------------------#


  

