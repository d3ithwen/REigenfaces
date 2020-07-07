load_dataset <- function(path, pattern=NULL, max_images=0L) {
  stopifnot(is.numeric(max_images))
  stopifnot(max_images >= 0)
  max_images <- as.integer(max_images)

  image_files <- list.files(path, pattern, full.names=TRUE)

  num_images <- min(max_images, length(image_files))
  if(num_images > 0) image_files <- image_files[1:num_images]

  images <- lapply(image_files, read.pnm)

  image_data <- lapply(images, attr, which="grey")

  image_vectors <- sapply(image_data, as.vector)

  return(image_vectors)
}

pca <- function(dataset, max_eigenfaces = 0L, standardized=FALSE) {
  stopifnot(is.numeric(max_eigenfaces))
  stopifnot(max_eigenfaces >= 0)
  max_eigenfaces <- as.integer(max_eigenfaces)

  n <- ncol(dataset)

  mean_image <- 1/n * apply(dataset, 1, sum)
  diff_to_mean <- apply(dataset, 2, `-`, mean_image)

  if(standardized) {
    N <- sqrt(n / apply(diff_to_mean, 1, function(x) return(sum(x^2))))
    diff_to_mean <- diag(N, nrow=length(N)) %*% diff_to_mean
  }

  # AA^t and A^tA have the same eigenvalues and eigenvectors can be easily
  # converted, so take the smaller one
  if(length(mean_image) <= n) {
    covariance_matrix <- diff_to_mean %*% t(diff_to_mean)
  } else {
    covariance_matrix <- t(diff_to_mean) %*% diff_to_mean
  }


  print("Calculating eigenfaces")
  eig <- eigen(covariance_matrix, symmetric=TRUE)


  # Order by eigenvalues, since the highest contribute the most to the variance
  permutation <- order(eig$values, decreasing=TRUE)
  eig$values <- eig$values[permutation]
  if(length(mean_image) <= n) {
    eig$vectors <- eig$vectors[,permutation]
  } else {
    # Convert A^tA eigenvectors to AA^t eigenvectors
    eig$vectors <- diff_to_mean %*% (eig$vectors[,permutation])
  }

  num_eigenfaces <- min(max_eigenfaces, length(eig$values))
  if(num_eigenfaces > 0) {
    eig$values <- eig$values[0:num_eigenfaces]
    eig$vectors <- eig$vectors[,0:num_eigenfaces]
  }

  eig$vectors <- apply(eig$vectors, 2, function(x) x/sqrt(sum(x^2)))

  print("Projecting dataset")
  eig$dataset_coef <- apply(diff_to_mean, 2, function(x) {
    return(matrix(x,nrow=1) %*% eig$vectors)
  })

  return(eig)
}

reconstruct_face <- function(eigenfaces, coefficients, mean_image) {
  stopifnot(ncol(eigenfaces) == length(coefficients))
  return(as.vector(eigenfaces %*% matrix(coefficients, ncol=1))+mean_image)
}

#dataset <- load_dataset("C:/Users/eding/Downloads/lfwcrop_grey/faces", pattern="0001", max_images=1000L)
#mean_image <- 1/ncol(dataset) * apply(dataset, 1, sum)

#dataset_pca <- pca(dataset, max_eigenfaces=1000, standardized=TRUE)

#hor <- 4L
#ver <- 4L
#par(mfrow=c(ver,hor),mar=c(0,0,0,0))

# Show most important eigenfaces
#for(i in 1:(hor*ver)) plot(pixmap::pixmapGrey(dataset_pca$vectors[,i], nrow=64))

# Show approximation of dataset faces with eigenfaces
#for(i in 1:(hor*ver/2L)) {
#  plot(pixmap::pixmapGrey(dataset[,i+0], nrow=64))
#  reconstruction <- reconstruct_face(dataset_pca$vectors, dataset_pca$dataset_coef[,i+0], mean_image)
#  plot(pixmap::pixmapGrey(reconstruction,nrow=64))
#}
