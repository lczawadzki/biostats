EDIT AND POST AS R EXERCISE 6
#Chi-squared test

#1) Read in your data
#2) Transform into table (if necessary)
#3) Assign observed variable (obs)
#4) Calculate expected values and combine into expected variable (exp)
#5) Run chisq.test

#Probability distribution ----

#Read in data. It lists colours of 55 M and Ms from a bag of M and Ms.
MMlist <- read.csv("https://github.com/lczawadzki/biostats/raw/main/data/MandMlist.csv")

#If your dataframe is one long column, BUT has different categories,
#you can summarise it in a table, using the function table(). 
#Once printed, it will sum the frequency of each category for you!!!

MMtable <- table(MMlist$color)
MMtable

#We now need to specify our observed and expected values. 
#Assign obs to your observed values, and exp to your expected values. 
#Your values will be in a vector written as c(n, n, n, n). c means "contains". 

# Our observed values are from our table: 10, 5, 12, 11, 6, 11.
obs <- c(10,5,12,11,6,11)
obs

# The M&M company states that 24% are blue, 14% brown, 16% green, 20% orange, 
# 13% red, and 13% yellow. We need the expected frequency of each colour, so we
# need to multiply our proportions by the TOTAL number of MandMs in our sample.
sum(MMtable)
exp <- c(0.24, 0.14, 0.16, 0.20, 0.13, 0.13)


#Run your chi-squared test. Your first argument will be the vector of observed data, 
#(obs), your second argument will be your expected data (p = exp), and your third argument, 
#rescale.p = T, is used when your expected values do NOT sum up to 1.
chisq.test(obs, p = exp, rescale.p = T)


#Poisson distribution ----

#Read in the dataframe on number of extinctions during different periods in Earth's history.
extinct <- read.csv("https://github.com/lczawadzki/biostats/raw/main/data/MassExtinctions.csv")

#Summarise the frequency of extinctions in a table.
extincttab <- table(extinct$numberOfExtinctions)
extincttab

#Find the mean (µ) extinctions per time interval. 
#DO NOT CALCULATE DIRECTLY FROM THE TABLE.
mean(extinct$numberOfExtinctions)

##We now need to specify our observed and expected values. Notice, our 
#expected values are NOT provided in our word problem. We will calculate these

#a) However, if we look at our data, we see that we defy assumptions of a chi-squared test.
#That is, some categories have EXP <= 1 --> 8, 10, 11, 14, 20 AND more than 20% 
#have EXP < 5 --> 0, 8, 10, 11, 14, and 20. We need to combine some categories together 
#manually. So, we will have 0 or 1 extinctions with a frequency of 13, 
#and 8 or more with a frequency of 9. 

#b) Now, assign obs to your observed values in the table (keeping in mind your two
#new combined categories).
obs <- c(13, 15, 16, 7, 10, 4, 2, 9)

#c) Your expected values are not specified in the word problem. You will need to 
#calculate these first. The function we use is dpois. 
#The first argument is the number of possible values, which in our case is 0 to 20. 
#So x = 0:20. Lambda is our mean (µ), calculated above.
expp <- dpois(x = 0:20, lambda = 4.21)
expval <- length(extinct$numberOfExtinctions)*expp #total times proportions

#Now, since we combined categories, we will need to add together the expected of 
#0 and 1, and the expected for 8 or more.
exp <- c(5.878568, 9.999264, 14.032300, 14.768996, 12.435494,  8.725572, 5.247808, 4.911998)

#Run your chi-squared test. This will happen in two parts: chisq.test to calculate
#the X2 test statistic, and then pchisq to calculate the P-value as it relates to
#a Poisson distribution.
chisq.test(obs, p = exp, rescale.p = T) #you just want X2; 
    #warning is because one value is <5, but this is <20% cats so we're fine
pchisq(q = 23.939, df = 7, lower.tail = FALSE) #p for POISSON!

#What if you JUST wanted the probability of 3 extinctions in a given time period,
#rather than to conduct a full hypothesis test on whether this data is random in space?
#Use the function dpois to calculate the probability of a certain number of successes, X.
#In this case, x = 3 extinctions, and lambda is our mean, which equals 4.21.
dpois(x = 3, lambda = 4.21)

