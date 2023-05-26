# foodquotient
The foodquotient package was created to estimate food quotients and daily nutrient intake on a group and individual level from responses to the Harvard Service Food Frequency Questionnaire. 

# Usage
To perform FFQ analysis using the foodquotient package, the user must posess data gathered using the Harvard Service Food Frequency Questionaire (HSFFQ), including the age of each participant as well as each participant's coded responses to 85 food frequency questions. More information about gathering data using the HSFFQ can be found at the Harvard T.H. Chan School of Public Health Nutrition Department's file download site. An example dataframe titled "age_freq" is included in the foodquotient package as an example of the dataframe structure needed to perform analysis using the foodquotient package. It is reccomended that the user wrangle their data into the structure of this example dataframe in order to apply the functions included in the foodquotient package. 

## List of functions and their purpose

### fq()
The fq() function serves to convert coded frequency responses from the HSFFQ to the number of daily servings consumed for each food that each participant eats. fq() can be used on a single value and a vector, but it is reccomended that the user plugs a dataframe into this function. Like the "age_freq" example dataframe, each row should represent a participant, but unlike "age_freq" the age column should be deleted, leaving only the 85 frequency factors. Pluging a dataframe into this function will spit out another dataframe of the same dimensions, with the frequency factors replaced with average daily servings. 

### grams()
### macros()
### micros()
### nutrients()
### quotient()
### macquotient()



# Installation
You can install foodquotient from GitHub using the `devtools` package:
devtools::install_github("katepogue222/foodquotient")

# Examples

test <- c(1,5,7,3,9,2,4,3,6,8,9)
results<- fq(test)
results


# Contributors
# Contact
# License
