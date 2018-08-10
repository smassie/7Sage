### Law School Numbers (LSN) admissions analysis ####

# 7Sage Admissions
# August 9, 2018

# Data Source: MyLSN http://mylsn.info/r/pre-law/admissions/search/ with below filters:
#   LSAT: 120-180
#   GPA: 1-4.4
#   Include all cycles from 2013-14 to 2017-18
#   Include URM and ED

### 1. Initialize environment ###

# install.packages('dplyr')
setwd("~/Desktop/R stuff")
library(dplyr, rlist)

# Assumes there is a file 'mylsn_raw.csv' in the working directory
con <- file('mylsn_raw.csv')
rawdata <- read.csv(con, header = TRUE)

### 2. Load data and define functions ###

# Remove 'Pe' statuses (pending, no decision date), add column for "Accepted"
rawdata <- rawdata %>%
    filter(status %in% c('Ac','AcWa','Re','ReWa','Wa')) %>%
    mutate(accepted = status %in% c('Ac','AcWa'))

# Functions to set up cycle & school variables
append_dummy_variables <- function(regression_data, dummy_data){
    # Create matrices of dummy variables for sent month, cycle, and school
    cycle_dummy <- data.frame(model.matrix(~factor(cycle), dummy_data))
    school_dummy <- data.frame(model.matrix(~school, dummy_data))
    
    # Bind data together
    modeldata <- bind_cols(regression_data,
                            select(cycle_dummy, -X.Intercept.),
                            select(school_dummy, -X.Intercept.) )
    return(modeldata)
}

# Function to split data into training & test sets
split_train_test <- function(mydata, seed = 41){
    # Randomly generate a list of unique rows to sample (10% of total)
    set.seed(seed)
    test_length <- nrow(mydata) %/% 10
    test_indices <- sample(1:nrow(mydata), test_length)
    
    # Split
    test <- mydata[test_indices,]
    train <- mydata[-test_indices,]

    return(list(test=test,train=train))
}

### 3. Prepare model ###

# Filter for data in last 5 cycles which has sent date
rawdata1 <- filter(rawdata, sent != '--', cycle %in% c(1314,1415,1516,1617,1718))

# Add a variable for days until July 1 of that cycle
rawdata1$sent <- strptime(rawdata1$sent, "%m/%d/%y")
julys <- strptime(paste("07/01/",substr(rawdata1$cycle,3,4),sep=""),"%m/%d/%y")
rawdata1$days_until_july <-as.integer(difftime(julys, rawdata1$sent, 
                                                units = "days"))
rm(julys)

# Remove records where sent date falls outside of the cycle or occurs after the dataset was created
rawdata1 <- rawdata1[rawdata1$days_until_july >= 0
                     & rawdata1$days_until_july < 365
                     & difftime(rawdata1$sent, strptime('06/12/18', "%m/%d/%y")) < 0
                         ,]

modeldata1 <- append_dummy_variables(
                    select(rawdata1,accepted,lsat,gpa,urm,ed,international,days_until_july),
                    rawdata1)
rm(rawdata1)

# Split into training and test
split1 <- split_train_test(modeldata1)

# Run regression
model1 <- glm(accepted ~ ., family='binomial', data = split1$train)

summary(model1)
write.csv(model1$coefficients, "coefficients_model1_20180809.csv")


# ## 2. With sent month.
# 
# # Filter for data in last 5 cycles which has sent date
# rawdata2 <- filter(rawdata, sent != '--', cycle %in% c(1314,1415,1516,1617,1718))
# 
# #  Add month dummary variable matrix
# sentmonth_dummy <- data.frame(model.matrix(~factor(sentmonth), rawdata2))
# 
# rawdata2 <- bind_cols(rawdata2, sentmonth_dummy)
# 
# modeldata2 <- append_dummy_variables(
#     select(rawdata2,accepted,lsat,gpa,urm,ed,international,
#            factor.sentmonth.2, factor.sentmonth.3, factor.sentmonth.4, factor.sentmonth.5, 
#            factor.sentmonth.6, factor.sentmonth.7, factor.sentmonth.8, factor.sentmonth.9,
#            factor.sentmonth.10, factor.sentmonth.11, factor.sentmonth.12),
#     rawdata2)
# rm(sentmonth_dummy, rawdata2)
# 
# # Split into training and test
# split2 <- split_train_test(modeldata2)
# 
# # Run regression
# model2 <- glm(accepted ~ ., family='binomial', data = split2$train)
# 
# # summary(model2)
# # write.csv(model1$coefficients, "coefficients_model1.csv")


### 4. Evaluate model ###

eval_model <- function(model, test){
    predictions <- predict.glm(model, test)
    # Convert to binary outcomes (convert logit to probability, then set TRUE if above cutoff)
    results <- sapply(predictions, function(y) 1/(1+exp(-y)) >= 0.5)
    # Count correct results    
    counter = 0
    for(i in 1:length(results)){
        counter <- counter + 1*(results[i] == test$accepted[i])
    }
    return(counter/nrow(test))
}

eval_model(model1, split1$test)
# eval_model(model2, split2$test)


### 5. Estimate effect sizes on a "typical" student ###

one_student <- split1$test[47,]

one_student$lsat = round(mean(modeldata1$lsat),0)
one_student$gpa = round(mean(modeldata1$gpa),2)
one_student$ed = 0
one_student$urm = 0
one_student$international = 0
one_student$days_until_july = round(median(modeldata1$days_until_july),0)
one_student$schoolarizona = 0
one_student$factor.cycle.1314 = 0
one_student$factor.cycle.1415 = 0
one_student$factor.cycle.1516 = 0
one_student$factor.cycle.1617 = 0
one_student$factor.cycle.1718 = 1




# Next: find a school which would accept this student about 50% of the time

one_student$schoolemory = 1

predict_adm <- function(model, student){
    L <- predict.glm(model, student)
    return(1/(1+exp(-L)))
}

show_ed_boost <- function(model, student){
    student$days_until_july = 227
    
    student$ed = 0
    no_ed = predict_adm(model, student)
    
    student$ed = 1
    ed = predict_adm(model,student)
    
    return(c(no_ed, ed, ed-no_ed))
    }

show_urm_boost <- function(model, student){
    student$urm = 0
    predict_adm(model,student)
    
    student$urm = 1
    predict_adm(model,student)
    
    return(c(no_urm, urm, urm-no_urm))
}

show_intl_boost <- function(model, student){
    student$international = 0
    no_intl = predict_adm(model,student)
    
    student$international = 1
    intl= predict_adm(model,student)
    
    return(c(no_intl, intl, intl-no_intl))
}

show_month_boost <- function(model, student){
    student$days_until_july = 259
    oct = predict_adm(model,student)
    
    student$days_until_july = 227
    nov = predict_adm(model,student)
    
    student$days_until_july = 197
    dec = predict_adm(model,student)
    
    student$days_until_july = 166
    jan = predict_adm(model,student)
    
    student$days_until_july = 135
    feb = predict_adm(model,student)
    
    barplot(100*c(oct,nov,dec,jan,feb), names.arg = c('Oct','Nov','Dec','Jan','Feb'), col = 'steelblue',
            ylab = 'Predicted acceptance rate (%)',
            xlab = 'Month applied')
    
    return(c(oct,nov,dec,jan,feb))
}


# barplot(c(55.5,48.4,40.1,37.2,32.6), names.arg = c('Oct','Nov','Dec','Jan','Feb'), col = 'steelblue',
#         ylab = 'Predicted acceptance rate (%)',
#         xlab = 'Month applied')


show_ed_boost(model1, one_student)
show_urm_boost(model1, one_student)

# ## Check which schools have less than 20 applications in the sample
# 
# dplyr_sucks <- select(rawdata1, -sent)
# dplyr_sucks <- dplyr_sucks %>% 
#     group_by(school)
# 
# school_counts <- summarize(dplyr_sucks, applications = n())
# 
# filter(school_counts, applications < 20)






