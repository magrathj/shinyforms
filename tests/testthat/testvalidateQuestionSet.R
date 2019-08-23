
library(shiny)

context("Testing validateQuestionSet Func")

test_that("questions is valid - id is null for a question", {
  
  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms")
  )
  
  expect_error(validateQuestionSet(questions))
  
})


test_that("questions is valid - type is null for a question", {
  
  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", title = "I agree to the terms")
  )
  
  expect_error(validateQuestionSet(questions))
  
})


test_that("questions is valid - title is null for a question", {
  
  questions <- list(
    list(id = "name", type = "text", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms")
  )
  
  expect_error(validateQuestionSet(questions))
  
})


test_that("questions is valid - type is not valid", {
  
  questions <- list(
    list(id = "name", type = "textbox", title = "Name", mandatory = TRUE)
  )
  
  expect_error(validateQuestionSet(questions))
  
})


