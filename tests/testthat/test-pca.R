# TODO: test load_images()

######################### testing load_dataset() #########################

test_that("load_dataset() throws error if no images found", {
  expect_error(load_dataset(""), "No matching images found. Make sure that your path and pattern are correct and max images is > 0.")
})

test_that("load_dataset() can handle wrong input", {
  expect_error(load_dataset(path = 42L), "path must be of type character")
  expect_error(load_dataset("", pattern = 42L), "pattern must be of type character")
  expect_error(load_dataset("", max_images = "foo"), "max_images must be numeric")
  expect_error(load_dataset("", max_eigenfaces = "bar"), "max_eigenfaces must be numeric")
})

test_that("load_dataset() returns correct data type and class", {
  expect_type(dataset, "list")
  expect_s3_class(dataset, "eigenface")
  expect_length(dataset, 7)
})

empty_dataset <- list()

######################### testing show_most_important_eigenfaces() #########################

test_that("show_most_important_eigenfaces can handle wrong input", {
  expect_error(show_most_important_eigenfaces(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(show_most_important_eigenfaces(dataset, "foo"), "max_count must be numeric")
})

######################### testing show_similar_faces() #########################

test_that("show_similar_faces can handle wrong input", {
  expect_error(show_similar_faces(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(show_similar_faces(dataset, "foo"), "image must be a double vector")
  expect_error(show_similar_faces(dataset, c(1,2,3), "bar"), "max_count must be numeric")
})

######################### testing reconstruct_dataset_images() #########################

test_that("reconstruct_dataset_images can handle wrong dataset", {
  expect_error(reconstruct_dataset_images(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(reconstruct_dataset_images(dataset, "foo"), "indices must be numeric")
})

######################### testing change_max_eigenfaces() #########################

test_that("change_max_eigenfaces can handle wrong dataset", {
  expect_error(change_max_eigenfaces(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(change_max_eigenfaces(dataset, "foo"), "max_eigenfaces must be numeric")
})

test_that("change_max_eigenfaces produces correct output", {
  copy1 <- dataset
  copy2 <- dataset

  copy1 <- change_max_eigenfaces(copy1, length(copy1$values)-1)
  expect_equal(length(copy1$values)+1, length(copy2$values))
})
