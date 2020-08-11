######################### testing load_pgm_images() #########################

test_that("load_pgm_images() can handle wrong input", {
  expect_error(load_pgm_images(2L), "path must be of type character")
  expect_error(load_pgm_images("", pattern = 42L), "pattern must be of type character")
  expect_error(load_pgm_images("", max_images = "foo"), "max_images must be numeric")
  expect_error(load_pgm_images("", max_images = -10), "max_images must be positive or 0")
})

test_that("load_pgm_images() throws error if no images found", {
  expect_error(load_pgm_images(""), "No matching images found. Make sure that your path and pattern are correct and max images is >= 0.")
})

######################### testing load_dataset() #########################

mat_images <- matrix(0, nrow=2, ncol=2)
mat2_images <- mat_images
mat3_images <- mat_images
mat4_images <- mat_images
attr(mat2_images, "size") <- 2L
attr(mat3_images, "size") <- c(2L,3L)
attr(mat4_images, "size") <- c(2L,1L)

test_that("load_dataset() can handle wrong input", {
  expect_error(load_dataset(""), "images must be numeric")
  expect_error(load_dataset(c(1,2,3)), "images must be a matrix")
  expect_error(load_dataset(mat_images), "images must have size attribute set")
  expect_error(load_dataset(mat2_images), "size attribute of images must be 2 integers corresponding to height and width")
  expect_error(load_dataset(mat3_images), "Each column of images must be a greyscale image with a length corresponding to the size attribute.")
  expect_error(load_dataset(mat4_images, max_eigenfaces = "foo"), "max_eigenfaces must be numeric")
  expect_error(load_dataset(mat4_images, max_eigenfaces = -3), "max_eigenfaces must be greater or equal 0")
})

test_that("load_dataset() returns correct data type and class", {
  expect_type(dataset, "list")
  expect_s3_class(dataset, "eigenface")
  expect_length(dataset, 5)
})

empty_dataset <- list()

######################### testing most_important_eigenfaces() #########################

test_that("show_most_important_eigenfaces can handle wrong input", {
  expect_error(most_important_eigenfaces(empty_dataset), "dataset must be of class eigenface and type list")
  expect_error(most_important_eigenfaces(dataset, "foo"), "max_count must be numeric")
})

######################### testing similar_faces_indices() #########################

test_that("show_similar_faces can handle wrong input", {
  expect_error(similar_faces_indices(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(similar_faces_indices(dataset, "foo"), "image must be a double vector")
  expect_error(similar_faces_indices(dataset, c(1,2,3), "bar"), "max_count must be numeric")
})

######################### testing reconstructed_dataset_images() #########################

test_that("reconstruct_dataset_images can handle wrong dataset", {
  expect_error(reconstructed_dataset_images(empty_dataset), "dataset must be of class eigeface and type list")
  expect_error(reconstructed_dataset_images(dataset, "foo"), "indices must be numeric")
})
