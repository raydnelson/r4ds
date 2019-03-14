# Vectors
# Initial: 7 Nov 2018
# Revision: 12 Mar 2019
# Ray Nelson

# Libraries
library(tidyverse)

# 20.2 Vector basics

letters
typeof(letters)
typeof(1:10)

(x <- list("a", "b", 1:10))
length(x)

# 20.3 Important types of atomic vector

1:10 %% 3 == 0

c(TRUE, TRUE, TRUE, FALSE, NA)

typeof(1)
typeof(1L)

1.5L

x <- sqrt(2) ^ 2
x
x - 2

c(-1, 0, 1) / 0

is.finite(1 / 0)
is.infinite(1 / 0)
is.nan(0 / 0)

x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

# 20.3.4 Missing values
NA

NA_integer_

NA_real_

NA_character_

# 20.4.1 Coercion

(x <- sample(20, 100, replace = TRUE))
(y <- x > 10)
sum(y)
mean(y)

typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

# 20.4.2 Test functions
(x <- "This is a reasonably long string.")
(y <- rep(x, 1000))

is_logical(y)
is_logical(x)
is_integer(3L)
is_double(3L)
is_numeric(x)
is_character(x)
is_atomic(mtcars$mpg)
is_list(mtcars)

# 20.4.3 Scalars and recycling rules
sample(10, 10) + 100

(runif(10) > 0.5) %>% sum()

1:10 + 1:2
rep(1:2, 5)

1:10 + 1:3

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))


# 20.4.4 naming vectors
(named_vector <- c(x = 1, y = 2, z = 4))

(named_vector <- set_names(1:3, c("a", "b", "c")))

# 20.4.5 Subsetting

(x <- c("one", "two", "three", "four", "five"))
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]

(x <- c(10, 3, NA, 5, 8, 1, NA))
typeof(x)
!is.na(x)
x[!is.na(x)]
x %% 2
x %% 2 == 0
x[x %% 2 == 0]

(x <- c(abc = 1, def = 2, xyz = 5))
x[c("xyz", "def")]

(x <- list(1, 2, 3))

str(x)

(x_named <- list(a = 1, b = 2, c = 3))

(y <- list("a", 1L, 1.5, TRUE))
str(y)

(z <- list(list(1, 2), list(3, 4)))
str(z)

(x1 <- list(c(1, 2), c(3, 4)))
str(x1)
(x2 <- list(list(1, 2), list(3, 4)))
str(x2)
(x3 <- list(1, list(2, list(3))))
str(x3)

# 20.5.2 Subsetting
(a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5)))
str(a)
str(a[1:2])
str(a[4])

str(a[[1]])
str(a[[4]])
a[[4]][[2]]

a$a
a[["a"]]

# 20.6 Attributes
(x <- 1:10)

attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

as.Date

methods("as.Date")

getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

# 20.7 Augmented vectors

# 20.7.1 Factors
(x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef")))
typeof(x)
attributes(x)

# 20.7.1 Dates and date-times

(x <- as.Date("1971-01-01"))
unclass(x)

typeof(x)
attributes(x)


(x <- lubridate::ymd_hm("1970-01-01 01:00"))
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)


# 0.7.3 Tibbles

(tb <- tibble::tibble(x = 1:5, y = 5:1))
typeof(tb)
attributes(tb)

(df <- data.frame(x = 1:5, y = 5:1))
typeof(df)
attributes(df)


# Create objects

individuals1 <- c(
  "Derrik Adams",
  "Devin Adams",
  "Mike Carpenter",
  "Filipe Dias Bezerra",
  "Kiersten Ernstrom",
  "Ruben Ferreyra",
  "Jonathan Fortuna",
  "Matt Hubbard",
  "Teresa Hwang",
  "Nate Lambert",
  "Ryan Lancaster",
  "Matt Lipps",
  "Luciano Peixoto",
  "Alex Rwankuba",
  "Shantel Sanders",
  "Hayden Smith",
  "Uros Stampe",
  "Bruno Torres",
  "Tiago Triumpho",
  "Ray Nelson"
)

individuals2 <- c(
  "Heidy Comish",
  "Eveline De Medeiros Miranda",
  "Kyle Finneman",
  "Chris Holdaway",
  "Taylor King",
  "Trevor Lake",
  "Travis Lovell",
  "Hope Morrison",
  "Sarah Nielsen",
  "You Yeong Oh",
  "Chad Olesiak",
  "Jonathan Perkins",
  "Jameson Ranck",
  "Seth Randall",
  "Amanda Spencer",
  "Joshua Thornock",
  "Matt Widmer",
  "Nathan Zick",
  "Ray Nelson"
)

program_names <- c("MIS", "MBA", "MPA", "MACC")
program_codes <- c(1:4)

programs1 <- c(2, 2,	2,	2,	1,	1,	2,	2,	3,	3,	3,	2,	2,	2,	3,	3,	2,	2,	2, NA)
programs2 <- c(3,	3,	1,	2,	1,	2,	2,	3,	3,	3,	2,	3,	2,	2,	2	, 2,	2,	1, NA)

typeof(individuals1)
typeof(individuals2)
typeof(programs1)
typeof(programs2)
typeof(program_names)
typeof(program_codes)

(programs <- tibble(program_names, program_codes))
(programs_list <- list(program_names = program_names, program_codes = program_codes))

length(individuals1)
length(individuals2)
(classes_tibble) <- tibble(individuals1, individuals2)
(class_list <- list(individuals2 = individuals1, individuals2 = individuals2))

(section1 <- rep("section1", length(individuals1)))
(section2 <- rep("section2", length(individuals2)))

section1 <- tibble(individuals1, programs1, section1)
colnames(section1) <- c("individual", "program", "section")
section1

section2 <- tibble(individuals2, programs2, section2)
colnames(section2) <- c("individual", "program", "section")
section2

(classes <- rbind(section2, section1))

(section_left <- section1 %>% 
  left_join(programs, by = c("program" = "program_codes")))

(section_inner <- section1 %>% 
  inner_join(programs, by = c("program" = "program_codes")))

(section_full <- section1 %>% 
  full_join(programs, by = c("program" = "program_codes")))

programs$program_names
programs$program_codes  

classes <- classes %>% 
  left_join(programs, by = c("program" = "program_codes"))
classes

classes <- classes %>% 
  select(individual, section, program_names) %>% 
  rename(program = program_names)
classes

classes %>% 
  filter(section == "section1")

classes %>% 
  filter(program == "MPA")

program <- tibble(program = "MBA")

classes %>% semi_join(program)
classes %>% anti_join(program)

section <- tibble(section = "section1")
classes %>% semi_join(section)

program_section <- tibble(section = "section2", program = "MPA")
classes %>% semi_join(program_section)
classes %>% anti_join(program_section)

colnames(classes)
dim(classes)
nrow(classes)
ncol(classes)
str(classes)

factor(classes$section)

factor(classes$section, levels = c("section2", "section1"))

factor(classes$program)
factor(classes$program) %>% fct_rev()
factor(classes$program, levels = c("MIS", "MPA", "MBA"))
factor(classes$program) %>% fct_inorder()

#
sections <- list(section1 = section1, section2 = section2)
typeof(classes)
is_tibble(classes)
typeof(sections)
is_tibble(sections)
typeof(programs)
is_tibble(programs)

rdn_teaching <- list(classes = classes, sections = sections, programs = programs)
typeof(rdn_teaching)
is_tibble(rdn_teaching)

rdn_teaching$classes
rdn_teaching$sections
rdn_teaching$programs

rdn_teaching$programs$program_names
rdn_teaching$classes$individual
rdn_teaching$sections$section1

# choose someone at random from section 2

students <- rdn_teaching$sections$section2$individual
length(students)
(student_number <- runif(1, 0, length(students)) %>% ceiling())
students[student_number]

guest <- function(students){
  student_number <- runif(1, 0, length(students)) %>% ceiling()
  students[student_number]
}

guest(students)
guest(section2$individual)
