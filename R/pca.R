norm <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) > 0)

  return(sqrt(sum(x^2)))
}

norm_squared <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) > 0)

  return(sum(x^2))
}

normalize <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) > 0)

  return(x/norm(x))
}

# Return width and height of image
load_pgm_size <- function(path) {
  image <- pixmap::read.pnm(path)
  return(attr(image,"size"))
}

load_pgm_image <- function(path, expected_size) {
  image <- pixmap::read.pnm(path)
  stopifnot("Unexpected image size" = attr(image,"size") == expected_size)

  data <- as.vector(attr(image,"grey"))

  return(data)
}

project_faces <- function(dataset, faces) {
  stopifnot(is.numeric(faces))

  faces <- faces - dataset$mean

  std_matrix <- diag(dataset$std_values)

  if(!is.matrix(faces)) {
    faces <- std_matrix %*% matrix(faces, ncol=1)
    faces <- matrix(faces, ncol=1)
  } else {
    faces <- std_matrix %*% faces
  }

  return(t(dataset$eigenfaces)%*%faces)
}

pca <- function(images, max_eigenfaces, standardized) {
  stopifnot(is.numeric(max_eigenfaces))
  stopifnot(max_eigenfaces >= 0)

  max_eigenfaces <- as.integer(max_eigenfaces)
  num_images <- ncol(images)
  mean_image <- 1/num_images * apply(images, 1, sum)
  diff_to_mean <- images - mean_image

  if(standardized) {
    N <- sqrt(num_images / apply(diff_to_mean, 1, norm_squared))
    diff_to_mean <- diag(N, nrow=length(N)) %*% diff_to_mean
  }

  # AA^t and A^tA have the same eigenvalues and eigenvectors can be easily
  # converted, so take the smaller one
  if(length(mean_image) <= num_images) {
    covariance_matrix <- diff_to_mean %*% t(diff_to_mean)
  } else {
    covariance_matrix <- t(diff_to_mean) %*% diff_to_mean
  }

  dataset <- eigen(covariance_matrix, symmetric=TRUE)
  names(dataset)[2] <- "eigenfaces"
  dataset$mean <- mean_image
  dataset$image_size <- attr(images, "size")

  if(standardized) {
    dataset$std_values <- N
  } else {
    dataset$std_values <- rep(1, times=nrow(diff_to_mean))
  }

  # Order by eigenvalues, since the highest contribute the most to the variance
  permutation <- order(dataset$values, decreasing=TRUE)
  dataset$values <- dataset$values[permutation]
  if(length(mean_image) <= num_images) {
    dataset$eigenfaces <- dataset$eigenfaces[,permutation]
  } else {
    # Convert A^tA eigenvectors to AA^t eigenvectors
    dataset$eigenfaces <- diff_to_mean %*% (dataset$eigenfaces[,permutation])
  }
  dataset$eigenfaces <- apply(dataset$eigenfaces, 2, normalize)

  if(max_eigenfaces > 0) {
    num_eigenfaces <- min(max_eigenfaces, ncol(dataset$eigenfaces)-3)
    if(length(dataset$eigenfaces) > 0) {
      if(num_eigenfaces >= 3) {
        # Remove the first 3 eigenfaces since those probably correspond to
        # differences in lighting
        dataset$eigenfaces <- dataset$eigenfaces[,4:(3+num_eigenfaces)]
      } else {
        dataset$eigenfaces <- dataset$eigenfaces[,0:num_eigenfaces]
      }
    }
  } else {
    total_variance <- sum(dataset$values[-(1:3)])
    index <- 4
    summed_variance <- 0

    while(index <= length(dataset$values) & summed_variance / total_variance < 0.95) {
      summed_variance <- summed_variance + dataset$values[index]
      index <- index + 1
    }
    dataset$eigenfaces <- dataset$eigenfaces[,4:(index-1)]
  }


  dataset$image_coef <- project_faces(dataset, images)

  class(dataset) <- "eigenface"

  # Eigenvalues are no longer needed
  dataset$values <- NULL

  return(dataset)
}

closest_matches_coefficients <- function(dataset, coefficients) {
  if(is.matrix(coefficients)) stopifnot(ncol(coefficients) == 1)
  coefficients <- as.vector(coefficients)

  dataset_diff <- dataset$image_coef - coefficients
  norms <- unname(apply(dataset_diff, 2, norm_squared)/(length(dataset$eigenfaces)*prod(dataset$image_size)))
  indexing <- order(norms, decreasing=FALSE)
  attr(indexing, "norms") <- norms

  return(indexing)
}

closest_matches_image <- function(dataset, image) {
  return(closest_matches_coefficients(dataset, project_faces(dataset, image)))
}

reconstruct_dataset_image <- function(dataset, index) {
  stopifnot(ncol(dataset$eigenfaces) == length(dataset$image_coef[,index]))
  std_inverse_matrix <- diag(1/dataset$std_values)
  return(as.vector(std_inverse_matrix%*%(dataset$eigenfaces %*% matrix(dataset$image_coef[,index], ncol=1))+dataset$mean))
}

################################### Functions to export ###################################

#' Load pgm Images.
#'
#' Load (new) image(s) to compute eigenfaces or search similar faces in the training data set. Only images in the pgm image format (greymap) are supported.
#'
#' @param path character; Specify the path to the image(s). (required)
#' @param pattern character; Regular expression used to filter images of the data set. Only image files matching the RegEx will be used. (optional)
#' @param max_images integer, Number of images that will be loaded. (optional)
#' @return A double vector containing the loaded image(s).
#' @examples
#' \dontrun{
#' images <- load_pgm_images("C:/Users/me/data/lfwcrop_grey/faces",
#'                           pattern="(0001)|(0002)|(0003)|(0004)",
#'                           max_images=1000L)
#' }
#' @export
load_pgm_images <- function(path, pattern=NULL, max_images=0L) {
  stopifnot("path must be of type character" = is.character(path))
  if(!is.null(pattern)) stopifnot("pattern must be of type character" = is.character(pattern))
  stopifnot("max_images must be numeric" = is.numeric(max_images))
  stopifnot("max_images must be positive or 0" = max_images >= 0)
  max_images <- as.integer(max_images)

  image_files <- list.files(path, pattern, full.names=TRUE)

  if(max_images > 0) {
    num_images <- min(max_images, length(image_files))
  } else {
    num_images <- length(image_files)
  }
  stopifnot("No matching images found. Make sure that your path and pattern are correct and max images is >= 0." = num_images > 0)

  image_files <- image_files[1:num_images]

  size <- load_pgm_size(image_files[[1]])
  images <- sapply(image_files, load_pgm_image, size)
  attr(images, "size") <- size

  colnames(images) <- stringr::str_extract(colnames(images), "([a-zA-Z-_]+)(?=_[0-9]+)")
  colnames(images) <- stringr::str_replace_all(colnames(images), "_", " ")

  return(images)
}

#' Load Training Data and Compute Eigenfaces.
#'
#' The specified data set is loaded (images that have been loaded with \code{load_pgm_images} before) and face images are converted to face vectors. The mean face is subtracted from all face vectors. Subsequently, the covariance matrix is computed and eigenvalues and corresponding eigenvectors (the eigenfaces) are determined.
#'
#' @param images double array; Images for which eigenfaces will be computed. (required)
#' @param max_eigenfaces integer; Number of eigenfaces that will be computed. (optional)
#' @param standardized logical; Specify whether or not images should be standerdized (subtract mean image from every image). (optional)
#' @return A list containing the eigenfaces and other information (\code{?dataset} for more information).
#' @examples
#' \dontrun{
#' my_dataset <- load_dataset(my_dataset_images, max_eigenfaces=100L, standardized=TRUE)
#' }
#' @export
load_dataset <- function(images, max_eigenfaces=0L, standardized=TRUE) {
  stopifnot("images must be numeric" = is.numeric(images))
  stopifnot("images must be a matrix" = is.matrix(images))
  stopifnot("images must have size attribute set" = !is.null(attr(images,"size")))
  stopifnot("size attribute of images must be 2 integers corresponding to height and width" = is.integer(attr(images,"size")) & length(attr(images,"size")) == 2)
  stopifnot("Each column of images must be a greyscale image with a length corresponding to the size attribute." = nrow(images) == prod(attr(images, "size")))

  stopifnot("max_eigenfaces must be numeric" = is.numeric(max_eigenfaces))
  stopifnot("max_eigenfaces must be greater or equal 0" = max_eigenfaces >= 0)

  max_eigenfaces <- as.integer(max_eigenfaces)

  pca(images, max_eigenfaces, standardized)
}

#' Most Important Eigenfaces.
#'
#' The \code{max_count} most important eigenfaces (eigenvectors corresponding to the \code{max_count} eigenvalues with highest absolute value) are returned. They can be displayed using the \code{show_images} function.
#'
#' @param dataset list; List returned by load_dataset() with computed eigenfaces. (required)
#' @param max_count integer; Number of eigenfaces that will be displayed. (optional)
#' @return double vector containing the \code{max_count} most important eigenfaces.
#' @examples
#' faces <- most_important_eigenfaces(dataset, 16L)
#' @export
most_important_eigenfaces <- function(dataset, max_count=1L) {
  stopifnot("dataset must be of class eigenface and type list" = (class(dataset) == "eigenface" & is.list(dataset)))
  stopifnot("max_count must be numeric" = is.numeric(max_count))

  count <- min(max_count, ncol(dataset$eigenfaces)-3)
  return(dataset$eigenfaces[,4:(count+3)])
}

#' Similar Faces.
#'
#' The indices of \code{max_count} similar faces of the training data set are returned.
#'
#' @param dataset list; List returned by load_dataset() with computed eigenfaces. (required)
#' @param image numeric; Image, for which similar faces are determined. (required)
#' @param max_count integer; Number of similar faces that will be displayed. (optional)
#' @return todo
#' @examples
#' similar_faces_indices(dataset, billy, max_count=9L)
#' @export
similar_faces_indices <- function(dataset, image, max_count=1L) {
  stopifnot("dataset must be of class eigeface and type list" = (class(dataset) == "eigenface" & is.list(dataset)))
  stopifnot("image must be a double vector" = is.double(image))
  stopifnot("max_count must be numeric" = is.numeric(max_count))

  closest <- closest_matches_image(dataset, image)

  count <- min(max_count, length(closest))
  closest <- structure(closest[1:count], "norm"=attr(closest, "norm")[closest[1:count]])

  names(closest) <- colnames(dataset$image_coef)[closest]

  invisible(closest)
}

#' Reconstruct Faces Using Eigenfaces.
#'
#' Reconstruct the original training data faces by linear combinations of eigenfaces.
#'
#' @param dataset list; List returned by load_dataset() with computed eigenfaces. (required)
#' @param indices integer; Indices of the original images that will be reconstructed. (required)
#' @return double vector containing the eigenface reconstruction of the original images.
#' @examples
#' faces <- reconstructed_dataset_images(dataset, 1:16)
#' @export
reconstructed_dataset_images <- function(dataset, indices) {
  stopifnot("dataset must be of class eigeface and type list" = (class(dataset) == "eigenface" & is.list(dataset)))
  stopifnot("indices must be numeric" = is.numeric(indices))

  indices <- indices[indices <= nrow(dataset$image_coef)]
  reconstructions <- sapply(indices, function(x) reconstruct_dataset_image(dataset, x))
  colnames(reconstructions) <- colnames(dataset$image_coef)[indices]
  return(reconstructions)
}

#' Show Images.
#'
#' Show images that have been loaded via \code{load_pgm_images} or eigenfaces that have been computed using \code{load_dataset} or \code{most_important_eigenfaces} or \code{reconstructed_dataset_images}.
#'
#' @param images double; Vector containing the images that will be displayed. (required)
#' @param size integer; Resolution of the image(s). (required)
#' @examples
#' show_images(billy, dataset$image_size)
#' @export
show_images <- function(images, size) {
  stopifnot(length(size) == 2)
  stopifnot(prod(size) == nrow(images))

  if(!is.matrix(images)) images <- matrix(images, ncol=1)

  hor <- 4L
  ver <- 4L
  par(mfrow=c(ver,hor),mar=c(0,0,0,0))

  count <- min(16, ncol(images))
  for(i in 1:count) pixmap::plot(pixmap::pixmapGrey(images[,i], nrow=size[1]))
}

#dataset_pattern <- "(0001)|(0002)|(0003)|(0004)|(0005)|(0006)|(0007)|(0008)|(0009)|(0010)|(0011)|(0012)|(0013)|(0014)|(0015)|(0016)|(0017)|(0018)"
#test_pattern <- "(0019)|(0020)"
#dataset_images <- load_pgm_images("G:\\Uni\\R Kurs\\Project\\datasets\\LFWcrop_sub\\", pattern=dataset_pattern, max_images=1000L)
#test_images <- load_pgm_images("G:\\Uni\\R Kurs\\Project\\datasets\\LFWcrop_sub\\", pattern=test_pattern)
#
#dataset <- load_dataset(dataset_images, max_eigenfaces=0L)

#correct <- 0
#count <- 40
#for(i in 1:count) {
#  image <- test_images[,i,drop=FALSE]
#  image_name <- colnames(image)[1]
#  faces_indices <- similar_faces_indices(dataset, image, 15)
#  faces_names <- names(faces_indices)
#  #show_images(cbind(image, dataset_images[,faces_indices]), dataset$image_size)
#  name_found <- stringr::str_detect(faces_names, image_name)
#  if(!any(name_found)) {
#    print("Not recognized")
#  } else {
#    print(which(name_found)[1])
#    if(which(name_found)[1] == 1) correct <- correct+1
#  }
#}
#cat("Correct: ", correct, "/", count, ", ", 100*correct/count, "%")


#faces <- most_important_eigenfaces(dataset, 16L)
#faces <- reconstructed_dataset_images(dataset, 1:16)

#show_images(faces, dataset$image_size)
