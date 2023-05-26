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
The macros() function uses the age of each participant as well as their 85 frequency responses to calculate the total amount of calcium, 
iron, zinc, vitamin.c, vitamin.b6, vitamin.a, and folate consumed per day for each participant. A dataframe identical in structure to the "age_freq" example dataframe is needed to plug into the macros() function. The function will output a dataframe with 7 columns labeled "calcium", "iron", "zinc", "vitamin.c", "vitamin.b6", "vitamin.a", and "folate" with an identical number of rows to the dataframe that was plugged into the function.

### nutrients()
The nutrienets() uses the age of each participant as well as their 85 frequency responses to calculate total daily amounts of macronutrients, micronutrients, and calories consumed per day for each participant. A dataframe identical in structure to the "age_freq" example dataframe is needed to plug into the macros() function. The function will output a dataframe with 11 columns labeled "calories", "protein", "carbs", "fat", "calcium", "iron", "zinc", "vitamin.c", "vitamin.b6", "vitamin.a", and "folate" with an identical number of rows to the dataframe that was plugged into the function.

### quotient()
The quotient() function uses the age of each participant as well as their 85 frequency responses to calculate indevidual level food quotients, which are used to calculate Total Energy Explenditure using the Doubly Labeled Water method. The function will produce a dataframe with one column and an identical number of rows to the dataframe that was plugged into the function. Each entry represents the indevidual level food quotient for each participant. 

### macquotient()
The macquotient() function uses protein, fat, and carbohydrate values to calculate indevidual or group level food quotient esitmates. In contrast to the quotient() function, the macquotient() function is able to estimate group food quotients based on average daily macronutrients consumed on a population level (controlling for indevidual response bias, lack of information, etc.) The function operates on a dataframe of three columns representing daily grams of protein, fat, and carbs consumed. It outputs a dataframe of one column and an identical number of rows to the inputed dataframe. Each entry represtents the foodquotient for the respective participant or subpopulation. 


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


## micros
random_integers <- sample(1:9, 85, replace=TRUE)
vec <- c(6.2, random_integers)
micros(vec)

vec1 <- c(5.1, sample(1:9, 85, replace = TRUE))
row1 <- data.frame(t(vec1))
row2 <- data.frame(t(c(8.3, sample(1:8, 85, replace = TRUE))))
df <- rbind(row1, row2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- micros(df[i,])
df_results <- rbind(df_results, result)
}


## nutrients()
random_integers <- sample(1:9, 85, replace=TRUE)
vec <- c(6.2, random_integers)
nutrients(vec)

vec1 <- c(5.1, sample(1:9, 85, replace = TRUE))
row1 <- data.frame(t(vec1))
row2 <- data.frame(t(c(8.3, sample(1:8, 85, replace = TRUE))))
df <- rbind(row1, row2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- nutrients(df[i,])
df_results <- rbind(df_results, result)
}

## quotient()
random_integers <- sample(1:9, 85, replace=TRUE)
vec <- c(6.2, random_integers)
quotient(vec)

vec1 <- c(5.1, sample(1:9, 85, replace = TRUE))
row1 <- data.frame(t(vec1))
row2 <- data.frame(t(c(8.3, sample(1:8, 85, replace = TRUE))))
df <- rbind(row1, row2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- quotient(df[i,])
df_results <- rbind(df_results, result)
}

## macquotient()
vec <- c(34.5,43, 212.4)
macquotient(vec)


vec1 <- c(34.5,43, 212.4)
vec2 <- c(40.1,52, 240)
df <- rbind(vec1, vec2)

df_results <- data.frame()
for (i in 1:nrow(df)) {
result <- macquotient(df[i,])
df_results <- rbind(df_results, result)
}




# Contributors
Kate Pogue

# Contact
Email Kate Pogue at kate_pogue1@baylor.edu

# License
Copyright (c) 2023 Kate Pogue

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

