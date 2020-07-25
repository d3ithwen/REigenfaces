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


load_image <- function(path) {
  as.vector(attr(pixmap::read.pnm(path),which="grey"))
}

load_images <- function(path, pattern=NULL, max_images=0L) {
  stopifnot(is.numeric(max_images))
  stopifnot(max_images >= 0)
  max_images <- as.integer(max_images)

  image_files <- list.files(path, pattern, full.names=TRUE)

  num_images <- min(max_images, length(image_files))
  if(num_images > 0) image_files <- image_files[1:num_images]

  images <- sapply(image_files, load_image)

  return(images)
}

project_faces <- function(pca, face) {
  stopifnot(is.numeric(face))

  face <- face - pca$mean

  if(!is.matrix(face)) face <- matrix(face, ncol=1)

  return(t(pca$vectors)%*%face)
}

pca <- function(images, max_eigenfaces = 0L, standardized=FALSE) {
  stopifnot(is.numeric(max_eigenfaces))
  stopifnot(max_eigenfaces >= 0)
  max_eigenfaces <- as.integer(max_eigenfaces)

  n <- ncol(images)

  mean_image <- 1/n * apply(images, 1, sum)
  diff_to_mean <- images - mean_image

  if(standardized) {
    N <- sqrt(n / apply(diff_to_mean, 1, norm_squared))
    diff_to_mean <- diag(N, nrow=length(N)) %*% diff_to_mean
  }

  # AA^t and A^tA have the same eigenvalues and eigenvectors can be easily
  # converted, so take the smaller one
  if(length(mean_image) <= n) {
    covariance_matrix <- diff_to_mean %*% t(diff_to_mean)
  } else {
    covariance_matrix <- t(diff_to_mean) %*% diff_to_mean
  }

  ptm <- proc.time()
  dataset <- eigen(covariance_matrix, symmetric=TRUE)
  dataset$mean <- mean_image
  dataset$images <- images

  # Order by eigenvalues, since the highest contribute the most to the variance
  permutation <- order(dataset$values, decreasing=TRUE)
  dataset$values <- dataset$values[permutation]
  if(length(mean_image) <= n) {
    dataset$vectors <- dataset$vectors[,permutation]
  } else {
    # Convert A^tA eigenvectors to AA^t eigenvectors
    dataset$vectors <- diff_to_mean %*% (dataset$vectors[,permutation])
  }
  dataset$vectors <- apply(dataset$vectors, 2, normalize)

  dataset$all_values <- dataset$values
  dataset$all_vectors <- dataset$vectors

  num_eigenfaces <- min(max_eigenfaces, length(dataset$values)-3)
  if(length(dataset$values) > 0) {
    if(num_eigenfaces >= 3) {
      # Remove the first 3 eigenfaces since those probably correspond to
      # differences in lighting
      dataset$values <- dataset$values[4:(3+num_eigenfaces)]
      dataset$vectors <- dataset$vectors[,4:(3+num_eigenfaces)]
    } else {
      dataset$values <- dataset$values[0:num_eigenfaces]
      dataset$vectors <- dataset$vectors[,0:num_eigenfaces]
    }
  }

  ptm <- proc.time()
  dataset$dataset_coef <- project_faces(dataset, dataset$images)

  class(dataset) <- "eigenface"

  return(dataset)
}

closest_matches_coefficients <- function(dataset, coefficients) {
  if(is.matrix(coefficients)) stopifnot(ncol(coefficients) == 1)
  coefficients <- as.vector(coefficients)

  dataset_diff <- dataset$dataset_coef - coefficients
  norms <- apply(dataset_diff, 2, norm_squared)/length(dataset$values)
  indexing <- order(norms, decreasing=FALSE)
  attr(indexing, "norms") <- norms
  return(indexing)
}

closest_matches_image <- function(dataset, image) {
  return(closest_matches_coefficients(dataset, project_faces(dataset, image)))
}

reconstruct_dataset_image <- function(dataset, index) {
  stopifnot(ncol(dataset$vectors) == length(dataset$dataset_coef[,index]))
  return(as.vector(dataset$vectors %*% matrix(dataset$dataset_coef[,index], ncol=1))+dataset$mean)
}

# Functions to export
load_dataset <- function(path, pattern=NULL, max_images=0L, max_eigenfaces=0L) {
  images <- load_images(path, pattern, max_images)
  stopifnot("No matching images found" = length(images)>0)
  pca(images, max_eigenfaces)
}

show_most_important_eigenfaces <- function(dataset, max_count=1) {
  stopifnot("dataset must be list of 7 and of class 'eigenface'" = typeof(dataset)=="list" & length(dataset)==7 & class(dataset)=="eigenface")
  stopifnot("max_count must be of number type" = typeof(max_count) == "double")
  hor <- 4L
  ver <- 4L
  par(mfrow=c(ver,hor),mar=c(0,0,0,0))
  count <- min(16, max_count, ncol(dataset$vectors))

  for(i in 1:(hor*ver)) plot(pixmapGrey(dataset$vectors[,i], nrow=64))
}

show_similar_faces <- function(dataset, image, max_count=1) {
  stopifnot("dataset must be list of 7 and of class 'eigenface'" = typeof(dataset)=="list" & length(dataset)==7 & class(dataset)=="eigenface")
  stopifnot("image must be double vector" = typeof(image) == "double" & is.vector(image))
  stopifnot("max_count must be numeric" = class(max_count) == "numeric")
  closest <- closest_matches_image(dataset, image)

  hor <- 4L
  ver <- 4L
  par(mfrow=c(ver,hor),mar=c(0,0,0,0))
  count <- min(15, max_count, length(closest))

  plot(pixmapGrey(image, nrow=64))
  for(i in 1:(hor*ver - 1)) plot(pixmapGrey(dataset$images[,closest[i]], nrow=64))
}


reconstruct_dataset_images <- function(dataset, indices) {
  stopifnot("dataset must be list of 7 and of class 'eigenface'" = typeof(dataset)=="list" & length(dataset)==7 & class(dataset)=="eigenface")
  stopifnot("indices must be numeric" = class(indices) == "numeric" & is.vector(indices))
  indices <- indices[indices <= nrow(dataset$dataset_coef)]
  reconstructions <- sapply(indices, function(x) reconstruct_dataset_image(dataset, x))

  hor <- 4L
  ver <- 4L
  par(mfrow=c(ver,hor),mar=c(0,0,0,0))
  count <- min(16, length(indices))
  for(i in 1:(hor*ver)) plot(pixmapGrey(reconstructions[,i], nrow=64))
}

change_max_eigenfaces <- function(dataset, max_eigenfaces=0L) {
  stopifnot("dataset must be list of 7 and of class 'eigenface'" = typeof(dataset)=="list" & length(dataset)==7 & class(dataset)=="eigenface")
  stopifnot("max_eigenfaces must be numeric" = class(max_eigenfaces) == "numeric")
  num_eigenfaces <- min(max_eigenfaces, length(dataset$all_values))
  if(length(dataset$all_values) > 0) {
    if(num_eigenfaces >= 3) {
      dataset$values <- dataset$all_values[4:(3+num_eigenfaces)]
      dataset$vectors <- dataset$all_vectors[,4:(3+num_eigenfaces)]
    } else {
      dataset$values <- dataset$all_values[1:num_eigenfaces]
      dataset$vectors <- dataset$all_vectors[,1:num_eigenfaces]
    }
  }

  dataset$dataset_coef <- project_faces(dataset, dataset$images)

  return(dataset)
}


#images <- load_images("dataset/", pattern="(0005)", max_images=1000L)
#dataset <- load_dataset("dataset/", pattern="(0001)|(0002)|(0003)|0004)", max_eigenfaces=100L)
#
#show_most_important_eigenfaces(dataset, 16)
#show_similar_faces(dataset, images[,1], 16)
#reconstruct_dataset_images(dataset, 1:16)
