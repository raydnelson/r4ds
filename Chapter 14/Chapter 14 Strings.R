# Chapter 14 Strings
# Initial: 14 December 2018
# Revision: 14 December 2018
# Ray Nelson

# Libraries
library(tidyverse)
library(stringr)

# 14.2 String basics

string1 <- "This is a string"
string2 <- 'IF I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\""
single_quote <- '\''

x <- c("\"", "\\")
x
writeLines(x)

?'"'
?"'"

x <- "\u00b5"
x

c("one", "two", "three")

# 14.2.1 String length

str_length(c("a", "R for data science", NA))

# 14.2.2 Combining strings

str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")

# 14.2.3 Subsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

# 14.2.4 Locales

## Turkish has two i's: with and without a dot, and it
## has a different rule for capitalising them:
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
str_sort(x, locale = "haw") # Hawaiian


# 14.3 Matching patterns with regular expressions

x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.")

dot <- "\\."
dot
writeLines(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

# 14.3.2 Anchors

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

# 14.3.3 Character classes and alternatives

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")

# 14.3.4 Repetition

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

# 14.3.5 Grouping and backreferences

fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "salal berry")
str_view(fruit, "(..)\\1", match = TRUE)

# 14.4 Tools

# 14.4.1 Detect matches

x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of comon words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")

# find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")

identical(no_vowels_1, no_vowels_2)