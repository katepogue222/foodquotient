# foodquotient
The foodquotient package was created to estimate food quotients and daily nutrient intake on a group and individual level from responses to the Harvard Service Food Frequency Questionnaire. The package produces reliable estimates for participants between 2 and 11 years old. 

# Usage
To perform FFQ analysis using the foodquotient package, the user must posess data gathered using the Harvard Service Food Frequency Questionaire (HSFFQ), including the age of each participant as well as each participant's coded responses to 85 food frequency questions. More information about gathering data using the HSFFQ can be found at the Harvard T.H. Chan School of Public Health Nutrition Department's file download site. An example dataframe titled "age_freq" is included in the foodquotient package as an example of the dataframe structure needed to perform analysis using the foodquotient package. It is reccomended that the user wrangle their data into the structure of this example dataframe in order to apply the functions included in the foodquotient package. 

## List of functions and their purpose

### fq()
The fq() function serves to convert coded frequency responses from the HSFFQ to the number of daily servings consumed for each food that each participant eats. fq() can be used on a single value and a vector, but it is reccomended that the user plugs a dataframe into this function. Like the "age_freq" example dataframe, each row should represent a participant, but unlike "age_freq" the age column should be deleted, leaving only the 85 frequency factors. Pluging a dataframe into this function will spit out another dataframe of the same dimensions, with the frequency factors replaced with average daily servings. 

### grams()
The grams() function uses the frequency responses and age of each participant to calculate the daily grams of each food consumed for each participant. Information about typical food consumption for different age groups and each respective food is used in this function and was gathered from open source USDA search tools. A dataframe identical in structure to the "age_freq" example dataframe is needed to plug into the grams() function. The function will output a dataframe with 85 columns and the same amount of inputed rows, with each value representing daily grams of each food consumed for that participant. 

### macros()
The macros() function uses the age of each participant as well as their 85 frequency responses to calculate the total amount of protein, carbohydrates, and fat consumed per day for the participant. A dataframe identical in structure to the "age_freq" example dataframe is needed to plug into the macros() function. The function will output a dataframe with 3 columns labeled "protein", "carbs", and "fat", with an identical number of rows to the dataframe that was plugged into the function. 

### micros()
### nutrients()
### quotient()
### macquotient()



# Installation
You can install foodquotient from GitHub using the `devtools` package:
devtools::install_github("katepogue222/foodquotient")

# Examples
## fq()
test <- c(1,5,7,3,9,2,4,3,6,8,9)
results<- fq(test)
results

## grams()
random_integers <- sample(1:9, 85, replace=TRUE)
vec <- c(6.2, random_integers)
grams(vec)

vec1 <- c(5.1, sample(1:8, 85, replace = TRUE))
row1 <- data.frame(t(vec1))
row2 <- data.frame(t(c(8.3, sample(1:8, 85, replace = TRUE))))
df <- rbind(row1, row2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- grams(df[i,])
df_results <- rbind(df_results, result)
}


}

## macros
random_integers <- sample(1:9, 85, replace=TRUE)
vec <- c(6.2, random_integers)
macros(vec)

vec1 <- c(5.1, sample(1:9, 85, replace = TRUE))
row1 <- data.frame(t(vec1))
row2 <- data.frame(t(c(8.3, sample(1:8, 85, replace = TRUE))))
df <- rbind(row1, row2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- macros(df[i,])
df_results <- rbind(df_results, result)
}


}

## micros
## nutrients()
## quotient()
## macquotient()




# Contributors
# Contact
# License
