### Law school admissions analysis ####


## RAA FUCK SHIT UP

### Create crosstab of school, cycle, ED vs. non ED, and admissions rate

## Prepare crosstab for plot
rawdata <- ungroup(rawdata)
rawdata <- mutate(rawdata, accepted_ed = ed == 1 & accepted
                  ,accepted_urm = urm == 1 & accepted)
rawdata <- group_by(rawdata, school, cycle)
crosstab <- summarize(rawdata,   applied = n()
                      ,accepted = sum(accepted)
                      ,applied_ed = sum(ed)
                      ,accepted_ed = sum(accepted_ed)
                      ,applied_urm = sum(urm)
                      ,accepted_urm = sum(accepted_urm))
crosstab <- mutate(crosstab, applied_non_ed = applied - applied_ed
                   ,accepted_non_ed = accepted - accepted_ed
                   ,applied_non_urm = applied - applied_urm
                   ,accepted_non_urm = accepted - accepted_urm
                   ,rate = 100*round(accepted/applied, 3)
                   ,rate_ed = 100*round(accepted_ed/applied_ed,3)
                   ,rate_non_ed = 100*round(accepted_non_ed/applied_non_ed, 3)
                   ,rate_urm = 100*round(accepted_urm/applied_urm, 3)
                   ,rate_non_urm = 100*round(accepted_non_urm/applied_non_urm, 3))

# write.csv(crosstab, file = "crosstab.csv")

## Plot ED
ed_plot <- filter(crosstab, applied_ed >= 20, cycle %in% c(1314,1415,1516,1617,1718))
with(ed_plot,
     plot(rate_non_ed, rate_ed, col = 'firebrick',
          xlab = '% non-ED applications accepted',
          ylab = '% ED applications accepted',
          main = 'Early vs. regular acceptance rates, 2013-2018'))
abline(0,1, col = 'firebrick')

# Conclusion 
#  Not enough data to say for most schools
#  For few schools with enough data, doesn't seem to help
#  No evidence to say that it actually helps you
#  Purpose is to save you hassle, finish the process earlier if you've got a dream school.  
#   Otherwise, don't worry about it.

## Plot URM
urm_plot <- filter(crosstab, applied_urm >= 20, cycle %in% c(1314,1415,1516,1617,1718))
with(urm_plot, 
     plot(rate_non_urm, rate_urm, col = 'steelblue',
          xlab = '% non-URM applications accepted',
          ylab = '% URM applications accepted',
          main = 'URM vs. non-URM acceptance rates, 2013-2018'))
abline(0,1, col = 'steelblue')

# Superficially suggests that URM applications are weaker on average.  Need regression analysis to 
# see if this is stil true after controlling for LSAT, GPA 

### Compare by sent month ###

# rawdata$sentmonth <- factor(rawdata$sentmonth)

# Make sure applied month variable is stored correctly
# factor? (yes)
# integer?
# if integer, scale so that first month = 9

rawdata <- ungroup(rawdata)
rawdata <- group_by(rawdata, sentmonth)

tab_by_month <- summarize(filter(rawdata, cycle %in% c(1314,1415,1516,1617,1718), ed==0) 
                          ,applied = n()
                          ,accepted = sum(accepted)
                          ,rate = 100*round(accepted/applied,3))

with(filter(tab_by_month, sentmonth!=0), 
     barplot(rate, 
             names.arg=sentmonth,
             col='aquamarine', 
             xlab = 'Month applied',
             ylab = '% applications accepted',
             main='Avg acceptance rate (%) by month applied, 2013-18'))

# Shows dramatic difference in acceptance rates between months.
# Way to visualize change across multiple cycles?
# spread / whisker plot 
# animation
# panel




### 3. Model ###

# Basic mode: Logistic regression.  One single model (so that controlling for relevant factors)
# Warning: one applicant will have multiple applications.  Potential source of bias if some applicatns submit more applications.
# Note: waitlisting not considered; those that end in acceptance counted as acceptences, else, rejections

# Ideas for improving accuracy:
# Fancy random tree / machine learning stuff?  
# Does prediction improve vs. a model which DOESN'T include ED or URM?
# Create 218- separate models, one for each school with more than N members
    # risk of overfitting
    # can test by using previous 5 cycles to predict last cycle.


# Business application: build a web app that lets people compute probabilities of where they can apply?
# Safety
# Reach
# "Probability of getting in somewhere."


## Process data for ALL models

# Include only applications from past five cycles, exclude if sent month was blank
rawdata_no_blank_months <- filter(rawdata, sentmonth > 0, cycle %in% c(1314,1415,1516,1617,1718))

# Exclude schools having fewer than 16 total observations?

# Create matrices of dummy variables for sent month, cycle, and school
sentmonth_dummy <- data.frame(model.matrix(~factor(sentmonth), rawdata_no_blank_months))
cycle_dummy <- data.frame(model.matrix(~factor(cycle), rawdata_no_blank_months))
school_dummy <- data.frame(model.matrix(~school, rawdata_no_blank_months))

# Bind data together
modeldata <- bind_cols( select(rawdata_no_blank_months, accepted, lsat, gpa, urm, ed),
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

## Fit models on training set

# # 1. Simple model containing only LSAT and GPA
# model1 <- glm(accepted ~ lsat+gpa, family = 'binomial', data=train)
# summary(model1)
# 
# # 2. Adding URM and ED
# model2 <- glm(accepted ~ lsat + gpa + urm + ed, family='binomial', data=train)
# summary(model2)

# 3. Adding cycle and application month.
model3 <- glm(accepted ~ lsat + gpa + urm + ed 
                        + factor.cycle.1415 + factor.cycle.1516 + factor.cycle.1617 + factor.cycle.1718
                        + factor.sentmonth.2 + factor.sentmonth.3 + factor.sentmonth.4
                        + factor.sentmonth.5 + factor.sentmonth.6+ factor.sentmonth.7
                        + factor.sentmonth.8 + factor.sentmonth.9+ factor.sentmonth.10
                        + factor.sentmonth.11 + factor.sentmonth.12, 
              family='binomial', data = train)
summary(model3)

# 4. Adding school dummy variables.  Note: reuses data frames from part 3.
model4 <- glm(accepted ~ ., family='binomial', data = train)
summary(model4)


# Note: coefficient on URM and ED seems to swtich sign.
    # "Overall" ED and URM don't help
    # But once factor in school, they do: "Schools that get more URM applicants are more competitve."
    # Different schools vary by ED pool, vary by URM 

# Tentative conclusion:
    # LSAT and GPA do matter.
    # URM does help.
    # ED does help.
    # Applying October or earlier gives a significant boost to your application, even after controlling for ED.
     # Chances start to drop off starting in November, get worse as you wait longer

    # How to interpret significance?  A: assumes coefficients normally distributed, calc. from sample size

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

# What about other model structures?
# One model per school is actually a bad idea for small schools; noisiness of datasets will obscure
# underlying trend.

# What about using a machine learning package?  Purpose would be to predict, with greater accuracy,
# the outcome for one person.
# Note: not worth the extra investment of time for right now. 
# Note: also lose interpretability of coefficients.  


