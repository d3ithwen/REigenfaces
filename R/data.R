#' Eigenfaces of LFWcrop data set.
#'
#' 50 eigenfaces of 100 LSWcrop images of celebrities. Please refer to \code{?images} for more information.
#'
#' @format A list with 5 entires:
#' \describe{
#'   \item{eigenfaces}{n = max_eigenfaces eigenfaces/eigenvectors}
#'   \item{mean}{the mean image of the data set}
#'   \item{image_size}{resolution of the images}
#'   \item{std_values}{normalized pixel values (original - mean)}
#'   \item{image_coef}{coefficients to reconstruct original face image from eigenfaces}
#' }
"dataset"

#' Subset of LFWcrop data set.
#'
#'
#' About 100 LSWcrop images of celebrities loaded with \code{load_pgm_images}.
#' LFWcrop is a cropped version of the Labeled Faces in the Wild (LFW)
#' dataset, keeping only the center portion of each image (i.e. the face).
#' In the vast majority of images almost all of the background is omitted.
#'
#' LFWcrop was created due to concern about the misuse of the original
#' LFW dataset, where face matching accuracy can be unrealistically
#' boosted through the use of background parts of images (i.e.
#' exploitation of possible correlations between faces and backgrounds).
#'
#' For each LFW image, the area inside a fixed bounding box was extracted.
#' The bounding box was at the same location for all images, with the
#' upper-left and lower-right corners being (83,92) and (166,175),
#' respectively. The extracted area was then scaled to a size of 64x64
#' pixels. The selection of the bounding box location was based on the
#' positions of 40 randomly selected LFW faces (1).
#'
#' As the location and size of faces in LFW was determined through
#' the use of an automatic face locator (detector), the cropped faces
#' in LFWcrop exhibit real-life conditions, including mis-alignment,
#' scale variations, in-plane as well as out-of-plane rotations.
#'
#'
#'
#' @format double array containing the pixel values of 100 LFWcrop images.
#'
#' @source \url{https://towardsdatascience.com/eigenfaces-recovering-humans-from-ghosts-17606c328184}
#'
#' @references
#' (1) C. Sanderson, B.C. Lovell.
#' Multi-Region Probabilistic Histograms for Robust
#' and Scalable Identity Inference.
#' ICB 2009, LNCS 5558, pp. 199-208, 2009.
#'
#' (2) G.B. Huang, M. Ramesh, T. Berg, E. Learned-Miller.
#' Labeled Faces in the Wild: A Database for Studying
#' Face Recognition in Unconstrained Environments.
#' University of Massachusetts, Amherst,
#' Technical Report 07-49, 2007.
"images"

#' Billy Joel.
#'
#' Billy Joels image of the LFWcrop data set. Please refer to \code{?images} for more information.
#'
#'@format double array containing the pixel values of Billy Joels image of the LFWcrop data set.
#'
#'@source \url{https://towardsdatascience.com/eigenfaces-recovering-humans-from-ghosts-17606c328184}
"billy"
