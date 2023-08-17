#' Frequency Factor
#'
#' The Frequency Factor function converts values 1-9, representing different
#' frequency factor responses from the hsffq, to average daily servings consumed
#' for that individual.
#'
#' @param f 1-9, representing different frequency factor responses from the
#' hsffq. These can be in a dataframe, vector, or just single values
#' @return a dataframe, vector, or single value of the same dimension as the
#' input, with each position holding the average daily servings consumed
#' for each food (columns) for each individual(rows).
#' @export
#' @examples
#' test <- c(1, 5, 7, 3, 9, 2, 4, 3, 6, 8)
#' fq(test)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(6)
#'
#' fq(df)
#'
fq <- function(f) {
  ifelse(f > 8, 6,
         ifelse(f > 7, 4.5,
                ifelse(f > 6, 2.5,
                       ifelse(f > 5, 1,
                              ifelse(f > 4, 0.8,
                                     ifelse(f > 3, 0.43,
                                            ifelse(f > 2, 0.14,
                                                   ifelse(f > 1, 0.08, 0)
                                            )
                                     )
                              )
                       )
                )
         )
  )
}

#' Grams
#'
#' The grams function takes the age of a participant and their responses on the
#' hsffq to generate an estimate of the participant's total daily grams consumed
#' for each food.
#'
#' @param row A numeric vector with components 'age', representing the age of the
#' participant, and 'f1' to 'f85', representing different frequency factor responses
#' from the hsffq.
#' @return A numeric vector of length 85, representing the estimated total daily
#' grams of each food consumed for the participant.
#' @export
#' @examples
#'
#' random_integers <- sample(1:9, 85, replace=TRUE)
#' vec <- c(6.2, random_integers)
#' grams(vec)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(2)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#'   result <- grams(df[i,])
#'   df_results <- rbind(df_results, result)
#' }
#'
#'
grams <- function(row) {

  a <- row[1]

  if (a >= 2 & a < 5) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_2.5
  } else if (a >= 5 & a <= 11) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_6.11
  } else {
    warning("Age is out of the expected range.")
    return(NULL)
  }

  output <- fq(v1) * v2

  return(output)
}
#' Macronutrients
#'
#' The Macronutrients function takes the age of a participant and their responses
#' on the hsffq to generate estimates of the participant's total daily protein,
#' carbohydrate, and fat consumed for each food.
#'
#' @param row vector with 86 entries consisting of 2 components f1:f85 1-9,
#'   representing different frequency factor responses from the hsffq. These will
#'   be stored in columns 2-86 in the row you plug in A value representing
#'   participant's age. This will be stored in column 1 of the input row
#' @return the row or dataframe returned will have 3 entries, representing total
#'   daily amounts of protein, carbohydrates, and fat for each participant
#' @export
#'
#' @examples
#'  random_integers <- sample(1:9, 85, replace=TRUE)
#' vec <- c(6.2, random_integers)
#' grams(vec)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(3)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#' result <- macros(df[i,])
#' df_results <- rbind(df_results, result)
#' }
#'
#'
macros <- function(row) {

  a <- row[1]

  if (a >= 2 & a <= 5) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_2.5 * hsffq()$protein
    v3 <- hsffq()$gm_per_serv_2.5 * hsffq()$carb
    v4 <- hsffq()$gm_per_serv_2.5 * hsffq()$fat
  } else if (a >= 5 & a <= 11) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_6.11 * hsffq()$protein
    v3 <- hsffq()$gm_per_serv_6.11 * hsffq()$carb
    v4 <- hsffq()$gm_per_serv_6.11 * hsffq()$fat
  } else {
    warning("Age is out of the expected range.")
    return(NULL)
  }

  protein_sum <- sum(fq(v1) * v2)
  carb_sum <- sum(fq(v1) * v3)
  fat_sum <- sum(fq(v1) * v4)

  data.frame(
    "protein" = protein_sum,
    "carbs" = carb_sum,
    "fat" = fat_sum
  )

}

#' Micronutrients
#'
#' The Micronutrients function takes the age of a participant and their responses
#' on the hsffq to generate an estimate of the participant's total daily
#' micronutrients consumed for each food.
#'
#' @param row contains two components. f1:f85 1-9, representing different
#'   frequency factor responses from the hsffq. These will be stored in columns
#'   2-86 in the row you plug in .   A value representing participant's age. This
#'   will be stored in column 1 of the input row
#' @return the row or dataframe returned will have 7 entries, representing total
#'   daily amounts of 7 micronutrients for each participant
#' @export
#' @examples
#' random_integers <- sample(1:8, 85, replace=TRUE)
#' vec <- c(6.2, random_integers)
#' micros(vec)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(4)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#' result <- micros(df[i,])
#' df_results <- rbind(df_results, result)
#' }
#'
#'
micros <- function(row) {


  a <- row[1]

  if (a >= 2 & a <= 5) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_2.5 * hsffq()$calcium
    v3 <- hsffq()$gm_per_serv_2.5 * hsffq()$iron
    v4 <- hsffq()$gm_per_serv_2.5 * hsffq()$zinc
    v5 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.c
    v6 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.b6
    v7 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.a
    v8 <- hsffq()$gm_per_serv_2.5 * hsffq()$folate
  } else if (a >= 5 & a <= 11) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_6.11 * hsffq()$calcium
    v3 <- hsffq()$gm_per_serv_6.11 * hsffq()$iron
    v4 <- hsffq()$gm_per_serv_6.11 * hsffq()$zinc
    v5 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.c
    v6 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.b6
    v7 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.a
    v8 <- hsffq()$gm_per_serv_6.11 * hsffq()$folate
  } else {
    warning("Age is out of the expected range.")
    return(NULL)
  }


  calcium_sum <- sum(fq(v1) * v2)
  iron_sum <- sum(fq(v1) * v3)
  zinc_sum <- sum(fq(v1) * v4)
  vitamin.c_sum <- sum(fq(v1) * v5)
  vitamin.b6_sum <- sum(fq(v1) * v6)
  vitamin.a_sum <- sum(fq(v1) * v7)
  folate_sum <- sum(fq(v1) * v8)

  macros_results <- data.frame(calcium = calcium_sum,
                               iron = iron_sum,
                               zinc = zinc_sum,
                               vitamin.c = vitamin.c_sum,
                               vitamin.b6 = vitamin.b6_sum,
                               vitamin.a = vitamin.a_sum,
                               folate = folate_sum)

  return(macros_results)
}

#' Nutrients
#'
#' The Nutrients function takes the age of a participant and their responses on
#' the hsffq to generate an estimate of the participant's total daily
#' micronutrients, macronutrients, and calories consumed for each food
#'
#' @param  row /contains two components. f1:f85 1-9, representing different
#'   frequency factor responses from the hsffq. These will be stored in columns
#'   2-86 in the row you plug in A value representing participant's age. This
#'   will be stored in column 1 of the input row
#' @return the row or dataframe returned will have 11 entries, representing
#'   total daily amounts of 7 micronutrients, 3 macronutrients, and calories for
#'   each participant. These columns will be labeled
#' @export
#' @examples
#' random_integers <- sample(1:8, 85, replace=TRUE)
#' vec <- c(6.2, random_integers)
#' nutrients(vec)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(5)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#' result <- nutrients(df[i,])
#' df_results <- rbind(df_results, result)
#' }
#'
#'

nutrients <- function(row) {

  a <- row[1]

  if (a >= 2 & a <= 5) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_2.5 * hsffq()$calories
    v3 <- hsffq()$gm_per_serv_2.5 * hsffq()$protein
    v4 <- hsffq()$gm_per_serv_2.5 * hsffq()$carb
    v5 <- hsffq()$gm_per_serv_2.5 * hsffq()$fat
    v6 <- hsffq()$gm_per_serv_2.5 * hsffq()$calcium
    v7 <- hsffq()$gm_per_serv_2.5 * hsffq()$iron
    v8 <- hsffq()$gm_per_serv_2.5 * hsffq()$zinc
    v9 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.c
    v10 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.b6
    v11 <- hsffq()$gm_per_serv_2.5 * hsffq()$vitamin.a
    v12 <- hsffq()$gm_per_serv_2.5 * hsffq()$folate
  } else if (a >= 5 & a <= 11) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_6.11 * hsffq()$calories
    v3 <- hsffq()$gm_per_serv_6.11 * hsffq()$protein
    v4 <- hsffq()$gm_per_serv_6.11 * hsffq()$carb
    v5 <- hsffq()$gm_per_serv_6.11 * hsffq()$fat
    v6 <- hsffq()$gm_per_serv_6.11 * hsffq()$calcium
    v7 <- hsffq()$gm_per_serv_6.11 * hsffq()$iron
    v8 <- hsffq()$gm_per_serv_6.11 * hsffq()$zinc
    v9 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.c
    v10 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.b6
    v11 <- hsffq()$gm_per_serv_6.11 * hsffq()$vitamin.a
    v12 <- hsffq()$gm_per_serv_6.11 * hsffq()$folate
  } else {
    warning("Age is out of the expected range.")
    return(NULL)
  }

  calories_sum <- sum(fq(v1) * v2)
  protein_sum <- sum(fq(v1) * v3)
  carb_sum <- sum(fq(v1) * v4)
  fat_sum <- sum(fq(v1) * v5)
  calcium_sum <- sum(fq(v1) * v6)
  iron_sum <- sum(fq(v1) * v7)
  zinc_sum <- sum(fq(v1) * v8)
  vitamin.c_sum <- sum(fq(v1) * v9)
  vitamin.b6_sum <- sum(fq(v1) * v10)
  vitamin.a_sum <- sum(fq(v1) * v11)
  folate_sum <- sum(fq(v1) * v12)

  nutrients_results <- data.frame(calories = calories_sum,
                               protein = protein_sum,
                               carbs = carb_sum,
                               fat = fat_sum,
                               calcium = calcium_sum,
                               iron = iron_sum,
                               zinc = zinc_sum,
                               vitamin.c = vitamin.c_sum,
                               vitamin.b6 = vitamin.b6_sum,
                               vitamin.a = vitamin.a_sum,
                               folate = folate_sum)

  return(nutrients_results)
}


#' Food Quotient Based on Macronutrients
#'
#' The macquotient function calculates a food quotient for a participant based
#' on average daily protein, carbs, and fat consumed for an individual or a
#' group. In contrast to the quotient function, macquotient is able to generate
#' reliable average food quotients for a group of people rather than only
#' individual level. Group level estimates are recomended in some studies to
#' control for response bias.
#'
#' @param  row contains three components. p average daily grams of protein
#'   consumed f average daily grams of fat consumed c/ average daily grams of
#'   carbohydrates consumed
#' @return one value per participant will be returned, representing the food
#'   quotient for the individual
#' @export
#'
#' @examples
#' vec <- c(34.5,43, 212.4)
#' macquotient(vec)
#'
#'
#' vec1 <- c(34.5,43, 212.4)
#' vec2 <- c(40.1,52, 240)
#' df <- rbind(vec1, vec2)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#' result <- macquotient(df[i,])
#' df_results <- rbind(df_results, result)
#' }
#'
#'

macquotient <- function(row) {


  p <- row[1]
  f <- row[2]
  c <- row[3]

  p1 <- p*4
  f1 <- f*9
  c1 <- c*4

  p2 <- p1/(p1 + f1 + c1)
  f2 <- f1/(p1 + f1 + c1)
  c2 <- c1/(p1 + f1 + c1)


  rq <- (p2*0.81 + f2*0.70 + c2*1.00)

  return(rq)

}


#' Food quotient based on hsffq results
#'
#' The quotient function calculates individual level food quotients based on the
#' individual's answers to the hsffq. This function is only recommended to
#' calculate at the individual level.
#'
#' @param  row contains two components. f1:f85 1-9, representing different
#'   frequency factor responses from the hsffq. These will be stored in columns
#'   2-86 in the row you plug in A value representing participant's age. This
#'   will be stored in column 1 of the input row/
#' @return one value per participant will be returned, representing the food
#'   quotient for the individual
#' @export
#'
#' @examples
#' random_integers <- sample(1:8, 85, replace=TRUE)
#' vec <- c(6.2, random_integers)
#' quotient(vec)
#'
#' rquestionnaire <- function(n, n_food_questions = 85) {
#'   mat <- matrix(
#'     sample(1:9, n_food_questions*n, replace = TRUE),
#'     nrow = n, ncol = n_food_questions
#'   )
#'   df <- data.frame( age = round(runif(n, 2, 11), digits = 1) )
#'   cbind(df, as.data.frame(mat))
#' }
#' df <- rquestionnaire(6)
#'
#' df_results <- data.frame()
#' for (i in 1:nrow(df)) {
#' result <- quotient(df[i,])
#' df_results <- rbind(df_results, result)
#' }
#'
#'
#'
#'
#'
quotient <- function(row) {


  a <- row[1]

  if (a >= 2 & a <= 5) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_2.5 * hsffq()$protein
    v3 <- hsffq()$gm_per_serv_2.5 * hsffq()$carb
    v4 <- hsffq()$gm_per_serv_2.5 * hsffq()$fat
  } else if (a >= 5 & a <= 11) {
    v1 <- row[2:86]
    v2 <- hsffq()$gm_per_serv_6.11 * hsffq()$protein
    v3 <- hsffq()$gm_per_serv_6.11 * hsffq()$carb
    v4 <- hsffq()$gm_per_serv_6.11 * hsffq()$fat
  } else {
    warning("Age is out of the expected range.")
    return(NULL)
  }

  protein_sum <- sum(fq(v1) * v2)
  carb_sum <- sum(fq(v1) * v3)
  fat_sum <- sum(fq(v1) * v4)

  macros_results <- data.frame(protein = protein_sum,
                               carbs = carb_sum,
                               fat = fat_sum)
  p <- macros_results$protein
  f <- macros_results$fat
  c <- macros_results$carbs

  p1 <- p*4
  f1 <- f*9
  c1 <- c*4

  p2 <- p1/(p1 + f1 + c1)
  f2 <- f1/(p1 + f1 + c1)
  c2 <- c1/(p1 + f1 + c1)


  rq <- (p2*0.81 + f2*0.70 + c2*1.00)

  return(rq)

}


#' Frequency Factors for CATALYST Project Children
#'
#' A small set of data including 32 children living in the united states. f1:f85
#' represents the frequency with which participants consumed 85 respective foods.
#' Numbers 1-9 correspond to the following:
#' 1: never
#' 2: 1-3 times per month
#' 3: once per week
#' 4: 2-4 times per week
#' 5: 5-6 times per week
#' 6: 1 per day
#' 7: 2-3 times per day
#' 8: 4-5 times per day
#' 9: 6 times per day
#'
#' @format ## `freq`
#' A data frame with 32 rows and 85 columns:
#' \describe{
#'   \item{f1}{milk frequency factor}
#'   \item{f2}{hot chocolate frequency factor}
#'   \item{f3}{cheese frequency factor}
#'   \item{f4}{yogurt frequency frequency factor}
#'   \item{f5}{ice cream frequency frequency factor}
#'   \item{f6}{pudding frequency factor}
#'   \item{f7}{orange juice frequency factor}
#'   \item{f8}{other juice frequency factor}
#'   \item{f9}{fruit drink frequency factor}
#'   \item{f10}{banana frequency factor}
#'   \item{f11}{peaches frequency factor}
#'   \item{f12}{mixed fruit frequency factor}
#'   \item{f13}{orange frequency factor}
#'   \item{f14}{apple and pear frequency factor}
#'   \item{f15}{applesauce frequency factor}
#'   \item{f16}{grapes frequency factor}
#'   \item{f17}{strawberries frequency factor}
#'   \item{f18}{melon frequency factor}
#'   \item{f19}{pineapple frequency factor}
#'   \item{f20}{raisins frequency factor}
#'   \item{f21}{corn frequency factor}
#'   \item{f22}{peas frequency factor}
#'   \item{f23}{tomato frequency factor}
#'   \item{f24}{peppers frequency factor}
#'   \item{f25}{carrot frequency factor}
#'   \item{f26}{broccoli frequency factor}
#'   \item{f27}{green beans frequency factor}
#'   \item{f28}{spinach frequency factor}
#'   \item{f29}{greens frequency factor}
#'   \item{f30}{mixed vegetable frequency factor}
#'   \item{f31}{squash frequency factor}
#'   \item{f32}{zucchini frequency factor}
#'   \item{f33}{fried potatoes frequency factor}
#'   \item{f34}{other potatoes frequency factor}
#'   \item{f35}{sweet potatoes frequency factor}
#'   \item{f36}{cabbage frequency factor}
#'   \item{f37}{lettuce frequency factor}
#'   \item{f38}{mayonnaise frequency factor}
#'   \item{f39}{chips frequency factor}
#'   \item{f40}{popcorn frequency factor}
#'   \item{f41}{crackers frequency factor}
#'   \item{f42}{nuts frequency factor}
#'   \item{f43}{cookies frequency factor}
#'   \item{f44}{cake frequency factor}
#'   \item{f45}{pie frequency factor}
#'   \item{f46}{jello frequency factor}
#'   \item{f47}{chocolate frequency factor}
#'   \item{f48}{candy frequency factor}
#'   \item{f49}{coffee frequency factor}
#'   \item{f50}{soda frequency factor}
#'   \item{f51}{sugarfree soda frequency factor}
#'   \item{f52}{beans frequency factor}
#'   \item{f53}{rice frequency factor}
#'   \item{f54}{pasta frequency factor}
#'   \item{f55}{pizza frequency factor}
#'   \item{f56}{tacos frequency factor}
#'   \item{f57}{mac and cheese frequency factor}
#'   \item{f58}{hot dogs frequency factor}
#'   \item{f59}{sausage frequency factor}
#'   \item{f60}{hamburger frequency factor}
#'   \item{f61}{tuna frequency factor}
#'   \item{f62}{fried fish frequency factor}
#'   \item{f63}{other fish frequency factor}
#'   \item{f64}{cold cuts frequency factor}
#'   \item{f65}{chicken nuggets frequency factor}
#'   \item{f66}{other chicken frequency factor}
#'   \item{f67}{pork frequency factor}
#'   \item{f68}{beef frequency factor}
#'   \item{f69}{organ meats frequency factor}
#'   \item{f70}{peanut butter frequency factor}
#'   \item{f71}{bread frequency factor}
#'   \item{f72}{butter frequency factor}
#'   \item{f73}{margarine frequency factor}
#'   \item{f74}{vegetabele soup frequency factor}
#'   \item{f75}{soup frequency factor}
#'   \item{f76}{tortilla frequency factor}
#'   \item{f77}{eggs frequency factor}
#'   \item{f78}{bacon frequency factor}
#'   \item{f79}{hot cereal frequency factor}
#'   \item{f80}{cold cereal frequency factor}
#'   \item{f81}{donuts frequency factor}
#'   \item{f82}{muffins frequency factor}
#'   \item{f83}{pancake frequency factor}
#'   \item{f84}{bagel frequency factor}
#'   \item{f85}{biscuit frequency factor}
#'
#'
#'
#'
#'
#'
#' }
#' @source <Baylor Human Evolutionary Biology Lab>
"freq"




#' Frequency Factors for CATALYST Project Children with Age of Participant
#'
#' A small set of food frequency questionnaire data including 32 children living
#'  in the United States. f1:f85 represents the frequency with which
#'  participants consumed 85 respective foods.
#' Numbers 1-9 correspond to the following:
#' 1: never
#' 2: 1-3 times per month
#' 3: once per week
#' 4: 2-4 times per week
#' 5: 5-6 times per week
#' 6: 1 per day
#' 7: 2-3 times per day
#' 8: 4-5 times per day
#' 9: 6 times per day
#'
#' @format ## `age_freq`
#' A data frame with 32 rows and 86 columns:
#' \describe{
#'   \item{a}{age of participant}
#'   \item{f1}{milk frequency factor}
#'   \item{f2}{hot chocolate frequency factor}
#'   \item{f3}{cheese frequency factor}
#'   \item{f4}{yogurt frequency frequency factor}
#'   \item{f5}{ice cream frequency frequency factor}
#'   \item{f6}{pudding frequency factor}
#'   \item{f7}{orange juice frequency factor}
#'   \item{f8}{other juice frequency factor}
#'   \item{f9}{fruit drink frequency factor}
#'   \item{f10}{banana frequency factor}
#'   \item{f11}{peaches frequency factor}
#'   \item{f12}{mixed fruit frequency factor}
#'   \item{f13}{orange frequency factor}
#'   \item{f14}{apple and pear frequency factor}
#'   \item{f15}{applesauce frequency factor}
#'   \item{f16}{grapes frequency factor}
#'   \item{f17}{strawberries frequency factor}
#'   \item{f18}{melon frequency factor}
#'   \item{f19}{pineapple frequency factor}
#'   \item{f20}{raisins frequency factor}
#'   \item{f21}{corn frequency factor}
#'   \item{f22}{peas frequency factor}
#'   \item{f23}{tomato frequency factor}
#'   \item{f24}{peppers frequency factor}
#'   \item{f25}{carrot frequency factor}
#'   \item{f26}{broccoli frequency factor}
#'   \item{f27}{green beans frequency factor}
#'   \item{f28}{spinach frequency factor}
#'   \item{f29}{greens frequency factor}
#'   \item{f30}{mixed vegetable frequency factor}
#'   \item{f31}{squash frequency factor}
#'   \item{f32}{zucchini frequency factor}
#'   \item{f33}{fried potatoes frequency factor}
#'   \item{f34}{other potatoes frequency factor}
#'   \item{f35}{sweet potatoes frequency factor}
#'   \item{f36}{cabbage frequency factor}
#'   \item{f37}{lettuce frequency factor}
#'   \item{f38}{mayonnaise frequency factor}
#'   \item{f39}{chips frequency factor}
#'   \item{f40}{popcorn frequency factor}
#'   \item{f41}{crackers frequency factor}
#'   \item{f42}{nuts frequency factor}
#'   \item{f43}{cookies frequency factor}
#'   \item{f44}{cake frequency factor}
#'   \item{f45}{pie frequency factor}
#'   \item{f46}{jello frequency factor}
#'   \item{f47}{chocolate frequency factor}
#'   \item{f48}{candy frequency factor}
#'   \item{f49}{coffee frequency factor}
#'   \item{f50}{soda frequency factor}
#'   \item{f51}{sugarfree soda frequency factor}
#'   \item{f52}{beans frequency factor}
#'   \item{f53}{rice frequency factor}
#'   \item{f54}{pasta frequency factor}
#'   \item{f55}{pizza frequency factor}
#'   \item{f56}{tacos frequency factor}
#'   \item{f57}{mac and cheese frequency factor}
#'   \item{f58}{hot dogs frequency factor}
#'   \item{f59}{sausage frequency factor}
#'   \item{f60}{hamburger frequency factor}
#'   \item{f61}{tuna frequency factor}
#'   \item{f62}{fried fish frequency factor}
#'   \item{f63}{other fish frequency factor}
#'   \item{f64}{cold cuts frequency factor}
#'   \item{f65}{chicken nuggets frequency factor}
#'   \item{f66}{other chicken frequency factor}
#'   \item{f67}{pork frequency factor}
#'   \item{f68}{beef frequency factor}
#'   \item{f69}{organ meats frequency factor}
#'   \item{f70}{peanut butter frequency factor}
#'   \item{f71}{bread frequency factor}
#'   \item{f72}{butter frequency factor}
#'   \item{f73}{margarine frequency factor}
#'   \item{f74}{vegetabele soup frequency factor}
#'   \item{f75}{soup frequency factor}
#'   \item{f76}{tortilla frequency factor}
#'   \item{f77}{eggs frequency factor}
#'   \item{f78}{bacon frequency factor}
#'   \item{f79}{hot cereal frequency factor}
#'   \item{f80}{cold cereal frequency factor}
#'   \item{f81}{donuts frequency factor}
#'   \item{f82}{muffins frequency factor}
#'   \item{f83}{pancake frequency factor}
#'   \item{f84}{bagel frequency factor}
#'   \item{f85}{biscuit frequency factor}
#'
#'
#'
#'
#'
#'
#' }
#' @source <Baylor Human Evolutionary Biology Lab>
"age_freq"



