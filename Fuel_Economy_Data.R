library(skimr)
library(tidyverse)
library(h2o)
library(glue)
library(e1071)
library(highcharter)
h2o.init()


mpg <- ggplot2::mpg

mpg %>% glimpse()

mpg %>% 
  inspect_na() 

num_vars <- mpg %>%
  select(-cty) %>% 
  select_if(is.numeric) %>% 
  names()
num_vars

for_vars <- c()
for (b in 2:length(num_vars)) {
  OutVals <- boxplot(mpg[[num_vars[b]]])$out
  if(length(OutVals)>0){
    for_vars[b] <- num_vars[b]
  }
}

num_vars <- mpg %>% 
  select_if(is.numeric) %>% 
  names()
num_vars

par(mfrow=c(2, 2))  

for (p in 1:length(num_vars)) {
  var.name = num_vars[p]
  plot(density(mpg[[num_vars[p]]]),
       main=glue('{enexpr(var.name)}'), 
       ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(mpg[[num_vars[p]]]), 2)))  
  polygon(density(mpg[[num_vars[p]]]), col="red")
}

hchart(cor(mpg %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)


predictors <- c("year","cyl","displ")
response <- "cty"

mpg <- as.h2o(mpg)
mpg.glm <-  h2o.glm(family= "gaussian", x= predictors, y=response, training_frame=mpg, lambda = 0, compute_p_values = TRUE)

h2o.coef(mpg.glm)
h2o.coef_norm(mpg.glm)

show(mpg.glm@model$coefficients_table)

mpg.glm@model$coefficients_table$std_error

mpg.glm@model$coefficients_table$p_value
#All x variables are strong predictors as p-value is less than 0.05.

mpg.glm@model$coefficients_table$z_value

h2o.std_coef_plot(mpg.glm)
#From the graphic we can see that cyl and displ have a greater effect on cty than year variable. 

h2o.performance(mpg.glm)
#From the results we can see that R2 is around 67% which means that 67% of the variance in cty is predictable 
#from the variables as year, cyl, displ.
