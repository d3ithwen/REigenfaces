test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#testing load_dataset()
test_that("load_dataset() throws error if no images found", {
  expect_error(load_dataset(""), "No matching images found")
})

images <- load_images("../../dataset/", pattern="(0010)", max_images = 100L)
dataset <- load_dataset("../../dataset/", pattern="(0009)", max_eigenfaces=100L)

test_that("load_dataset() returns correct data type and class", {
  expect_type(dataset, "list")
  expect_length(dataset, 7)
})

empty_dataset <- list()

#testing show_most_important_eigenfaces()

test_that("show_most_important_eigenfaces can handle wrong input", {
  expect_error(show_most_important_eigenfaces(empty_dataset), "dataset must be list of 7 and of class 'eigenface'")
  expect_error(show_most_important_eigenfaces(dataset[1,2]), "falsche Anzahl von Dimensionen")
  expect_error(show_most_important_eigenfaces(dataset, "1"), "max_count must be of number type")
})

#testthat("show_most_important_eigenfaces produces correct output")

#testing show_similar_faces()

test_that("show_similar_faces can handle wrong input", {
  expect_error(show_similar_faces(empty_dataset), "dataset must be list of 7 and of class 'eigenface'")
  expect_error(show_similar_faces(dataset[1,2]), "falsche Anzahl von Dimensionen")
  expect_error(show_similar_faces(dataset, "1"), "image must be double vector")
  expect_error(show_similar_faces(dataset, c(1L,2L,3L,4L)), "image must be double vector")
  expect_error(show_similar_faces(dataset, images[,1], "2"), "max_count must be numeric")
})

#testthat("show_similar_faces() produces correct output")

#testing reconstruct_dataset_images()

test_that("reconstruct_dataset_images can handle wrong dataset", {
  expect_error(reconstruct_dataset_images(empty_dataset), "dataset must be list of 7 and of class 'eigenface'")
  expect_error(reconstruct_dataset_images(dataset[1,2]), "falsche Anzahl von Dimensionen")
  expect_error(reconstruct_dataset_images(dataset, "1"), "indices must be numeric")
})

#test_that("reconstruct_dataset_images produces correct output")

#testing change_max_eigenfaces()

test_that("change_max_eigenfaces can handle wrong dataset", {
  expect_error(change_max_eigenfaces(empty_dataset), "dataset must be list of 7 and of class 'eigenface'")
  expect_error(change_max_eigenfaces(dataset[1,2]), "falsche Anzahl von Dimensionen")
  expect_error(change_max_eigenfaces(dataset, "2"), "max_eigenfaces must be numeric")
})
