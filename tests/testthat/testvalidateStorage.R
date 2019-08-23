
library(shiny)

context("Testing validateStorage Func")

test_that("storage valid - type is null for a storage set", {
  
  storage = list(
    # The path where responses are stored
    path = "responses"
  )

  
  expect_error(validateStorage(storage))
  
})



test_that("storage is valid - path is null for a storage set", {
  
  storage = list(
    # Right now, only flat file storage is supported
    type = STORAGE_TYPES$FLATFILE
  )
  
  
  expect_error(validateStorage(storage))
  
})
