choose_quiz <- function(probability_attendance){
  if (runif(1) < probability_attendance) "Attendance" else "Regular"
}

choose_quiz(.5)

prob_quiz <- function(total_quiz, number_attendance){
  (13 - number_attendance) / (25 - total_quiz)
}

prob_quiz(16, 6)

quiz <- function(total_quiz, number_attendance){
  prob <- prob_quiz(total_quiz, number_attendance)
  choose_quiz(prob)
}

quiz(16, 6)
quiz(16, 10)

quiz <-
  function(total_quiz,
           quiz_to_date,
           number_attendance,
           target) {
    if(number_attendance > quiz_to_date){
      stop("Number of attendance quizzes should not exceed possible quizzes")
    }
    if (number_attendance > target) {
      prob <- 0.25
    } else {
      prob <- (target - number_attendance) / (total_quiz - quiz_to_date)
    }
    paste("The probability of an attendance quiz is:", prob %>%
            round(2)) %>%
      print()
    random_draw <- runif(1)
    paste("The random uniform number is: ", random_draw %>%
            round(2)) %>%
      print()
    quiz_type <- if (random_draw < prob)
      "Attendance"
    else
      "Regular"
    paste("Today's quiz is: ", quiz_type)
  }

quiz(25, 7, 3, 10)

