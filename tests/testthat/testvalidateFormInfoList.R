
library(shiny)

context("Testing validateFormInfoList Func")


test_that("formInfo is valid - questions not filled in", {
  questions <- list(
    
  )
  formInfo <- list(
    id = "basicinfo",
    questions = questions,
    storage = list(
      # Right now, only flat file storage is supported
      type = STORAGE_TYPES$FLATFILE,
      # The path where responses are stored
      path = "responses"
    )
  )
  
  expect_error(validateFormInfoList(formInfo))
})


test_that("formInfo is valid - storage not filled in", {
  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms")
  )
  formInfo <- list(
    id = "basicinfo",
    questions = questions
  )
  
  expect_error(validateFormInfoList(formInfo))
})


test_that("formInfo is valid - id not filled in", {
  questions <- list(
    list(id = "name", type = "text", title = "Name", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age"),
    list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
    list(id = "terms", type = "checkbox", title = "I agree to the terms")
  )
  formInfo <- list(
    questions = questions,
    storage = list(
      # Right now, only flat file storage is supported
      type = STORAGE_TYPES$FLATFILE,
      # The path where responses are stored
      path = "responses"
    )
  )
  
  expect_error(validateFormInfoList(formInfo))
})




test_that("formInfo is valid - formInfo empty list", {

  formInfo <- list(
    
  )
  
  expect_error(validateFormInfoList(formInfo))
})




