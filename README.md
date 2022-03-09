# Data Science Capstone Project: Analyzing Public Health Indicators Associated with Teenage Birth Rate in Chicago
Francisca Moya Jimenez


This capstone started as my final project for STAT 318: Regression Analysis and Statistical Models in Fall of 2021, where my partner Andrea Mock and I, fit a multiple linear regression model to identify public health indicators associated with teen birth rate in Chicago. 
In my capstone project for my Data Science major, I built upon our work by looking into possible interaction models into more detail, and visualizing the interactions in the best multiple linear regression model. We can compare the first-order model and the model
with interactions to better understand the relationships between the different predictors. 


## Abstract

The CDC identifies the social determinants of health as “conditions in the environments where people are born, live, learn, work, play, worship, and age that affect a wide range of health, functioning, and quality-of-life outcomes and risks”. Social determinants like high unemployment, low education, and low income have been associated with higher teen birth rates (Public Health Reports: “Socioeconomic Disadvantage as a Social Determinant of Teen Childbearing in the United States.”) 
Similarly to the general trend, teen birth rates in Chicago have dropped in recent years, yet remain higher than the national average (20.3 per 1,000) at 21.5 per 1,000. Moreover, there are stark disparities in teen birth rates across the 77 Chicago community areas, ranging from 1.3 (Loop) to 116.9 (West Englewood) per 1,000 population respectively. 

Our analysis aims to identify social determinants that are associated with teen birth rates in Chicago. The social determinants identified would inform the characteristics of the community areas with higher teen birth rates, which could be used by authorities to develop programs that provide holistic support to these areas. We run a multiple linear regression model to identify the public health determinants associated with teenage pregnancy in Chicago using data for the 77 community areas in the City of Chicago. Our final model is fitted with the BIC stepwise procedure and has an adjusted R-squared value of 0.88. Out of the 33 predictors in our dataset, our model identifies birth rate, prenatal care beginning in the first trimester, breast cancer in females, diabetes-related deaths, firearm-related deaths, crowded housing, and unemployment as the most relevant variables to predict teen pregnancy birth rate. This model can be used to identify public health challenges affecting the quality of life of community areas with higher rates of teenage pregnancy.


### Final First-Order Model
![Final first-order model using public health variables to predict teen pregnancy](https://github.com/fmoyaj/TeenBirthRate-Chicago/blob/main/firstordermodel.png?raw=true)

### Final First-Order Model with Interactions
![Final first-order model using public health variables to predict teen pregnancy](https://github.com/fmoyaj/TeenBirthRate-Chicago/blob/main/interactionmodel.png)

