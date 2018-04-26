### Law School Numbers (LSN) admissions analysis ####

# Samuel Massie
# 7Sage Admissions
# April 26, 2018

# Data Source: MyLSN http://mylsn.info/r/pre-law/admissions/search/

### 1. Initialize environment and load data ###

# install.packages('dplyr')
setwd("~/Desktop/R stuff")
library(dplyr, rlist)

# Assumes there is a file 'myles_raw.csv' in the working directory
con <- file('mylsn_raw.csv')
rawdata <- read.csv(con, header = TRUE)

# Add binary colummn for "accepted."
rawdata <- mutate(rawdata, accepted = status %in% c('Ac','AcWa'))

# Add binary columns for second and third LSAT.  Commented out because no data after 1213 cycle.
# rawdata <- rawdata %>%
#     mutate(has_lsat2 = 1*(lsat2 > 0)) %>%
#     mutate(has_lsat3 = 1*(lsat3 > 0))


### 2. Process data ###

# Include only applications from past five cycles, exclude if sent month was blank
rawdata_no_blank_months <- filter(rawdata, sentmonth > 0, cycle %in% c(1314,1415,1516,1617,1718))

# Create matrices of dummy variables for sent month, cycle, and school
sentmonth_dummy <- data.frame(model.matrix(~factor(sentmonth), rawdata_no_blank_months))
cycle_dummy <- data.frame(model.matrix(~factor(cycle), rawdata_no_blank_months))
school_dummy <- data.frame(model.matrix(~school, rawdata_no_blank_months))

# Bind data together
modeldata <- bind_cols( select(rawdata_no_blank_months, accepted, lsat, gpa, urm, ed, international,
                               has_lsat2, has_lsat3),
                        select(sentmonth_dummy, -X.Intercept.),
                        select(cycle_dummy, -X.Intercept.),
                        select(school_dummy, -X.Intercept.) )


## Split dataset into training and test set

# Randomly generate a list of unique rows to sample (10% of total)
set.seed(41)
test_length <- nrow(modeldata) %/% 10
test_indices <- sample(1:nrow(modeldata), test_length)

# Split
test <- modeldata[test_indices,]
train <- modeldata[-test_indices,]

### 3. Model ###

# # 1. Simple model containing only LSAT and GPA
# model1 <- glm(accepted ~ lsat+gpa, family = 'binomial', data=train)
# summary(model1)
# 
# # 2. Adding URM and ED
# model2 <- glm(accepted ~ lsat + gpa + urm + ed, family='binomial', data=train)
# summary(model2)

# 3. Adding cycle and application month.
# model3 <- glm(accepted ~ lsat + gpa + urm + ed 
#                         + factor.cycle.1415 + factor.cycle.1516 + factor.cycle.1617 + factor.cycle.1718
#                         + factor.sentmonth.2 + factor.sentmonth.3 + factor.sentmonth.4
#                         + factor.sentmonth.5 + factor.sentmonth.6+ factor.sentmonth.7
#                         + factor.sentmonth.8 + factor.sentmonth.9+ factor.sentmonth.10
#                         + factor.sentmonth.11 + factor.sentmonth.12, 
#               family='binomial', data = train)
# summary(model3)

# 4. Adding school dummy variables.
model4 <- glm(accepted ~ ., family='binomial', data = train)

summary(model4)




## Estimate size of ED effect on a typical application

one_student <- train[47,]

one_student$lsat = round(mean(modeldata$lsat),0)
one_student$gpa = mean(modeldata$gpa)
one_student$urm = 0

one_student$schooliowa = 0
one_student$schoolgmu = 1

show_ed_boost <- function(model, student){
    student$ed = 0
    L <- predict.glm(model, student)
    no_ed <- 1/(1+exp(-L))
    
    student$ed = 1
    L <- predict.glm(model, student)
    ed <- 1/(1+exp(-L))
    
    return(c(no_ed, ed, ed-no_ed))
    }

show_urm_boost <- function(model, student){
    student$urm = 0
    L <- predict.glm(model, student)
    no_urm <- 1/(1+exp(-L))
    
    student$urm = 1
    L <- predict.glm(model, student)
    urm <- 1/(1+exp(-L))
    
    return(c(no_urm, urm, urm-no_urm))
}

one_student$factor.sentmonth.11 = 0
# October: 55.5, November: 48.4, December: 40.1, January: 37.2, February: 32.6

barplot(c(55.5,48.4,40.1,37.2,32.6), names.arg = c('Oct','Nov','Dec','Jan','Feb'), col = 'steelblue',
        ylab = 'Predicted acceptance rate (%)',
        xlab = 'Month applied')


show_ed_boost(model4, one_student)

## Use models to predict on test set, compare to actual results for accuracy

eval_model <- function(model){
    predictions <- predict.glm(model, test[,-1])
    # Convert to binary outcomes (convert logit to probability, then set TRUE if above cutoff)
    results <- sapply(predictions, function(y) 1/(1+exp(-y)) >= 0.5)
    # Count correct results    
    counter = 0
    for(i in 1:length(results)){
        counter <- counter + 1*(results[i] == test$accepted[i])
    }
    return(counter/nrow(test))
}

eval_model(model1)
eval_model(model2)
eval_model(model3)
eval_model(model4)


