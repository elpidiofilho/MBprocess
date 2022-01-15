Mode <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
}

## number of periods (years) from last change
LastChange <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {

    m = rle(x)
    l = length(m$values)
    ch = m$length[l]
    return(ch)
  }
}


#Maximum years of Sequence without LU change
max_class_freq <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {
    mx = rle(x)
    max = max(mx$lengths)
  }
  return(max[1])
}


##  Class Maximum Sequence without LU change
max_class_id <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {
    mx = rle(x)
    max = max(mx$values)
    return(max[1])
  }
}

#Number of LU changes
nchange <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {
    mx = rle(x)
    nc = length(mx$lengths) - 1
    return(nc)}
}

## number of diferent class by pixel
diversity <- function(x) {
  if (any(is.na(x)) == TRUE) {
    return(NA)
  } else {
    mx = rle(x)
    nc = length(unique(mx$values))
    return(nc)}
}


#https://stackoverflow.com/questions/16244006/matching-a-sequence-in-a-larger-vector
#determines by pixel how many times a class a has changed to class b
vecIn <- function(a, b){
  x = NULL
  if (any(is.na(a)) == TRUE) {
    return(NA)
  } else {
    ll = which(
      Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){
        y[[x]][x:(length(a) - length(b) + x)]
      }
      )
      ) == length(b)
    )
    return(as.integer(length(ll)))
  }
}



#' lu_change_classAB
#' @description Determines by pixel how many times a class A has changed
#'     to a class B. Can be used with more than two class
#' @param r spatRaster or RasterStack with LU layers
#' @param classab vector 2 class id ex : c(4,6) or c('forest', 'pasture)
#' @param cores  integer number of CPU cores
#' @return statRaster
#' @export
#' @examples
#' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' rm1 <- MBprocess::lu_change_classAB(r = mb, classab = c(6,4), cores = 0)
#' terra::freq(rm1)
#' terra::plot(rm1)

lu_change_classAB <- function(r, classab, cores) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rm1 = terra::app(r, vecIn,  b = classab,  cores = cores)
  return(rm1)
}



#' Land Use Last Change
#' @description number of periods (years) from last land use change
#' @param r SpatRaster
#' @param cores num of processor cores to be used
#' @importFrom terra app
#' @return SpatRaster
#' @export
#'
#' @examples
#' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' plot(mb[[1]], main = names(mb)[1])
#' last_ch = lu_last_change(r = mb, cores = 0)
#' plot(last_ch)
#' writeRaster(last_ch, filename = 'last_change.tif', overwrite = TRUE,
#'             gdal = c("COMPRESS=LZW"), datatype = "INT1U")
#'
#' df = terra::freq(last_ch) |>
#'   data.frame() |>
#'   subset(select = -layer) |>
#'   transform(freq_100 = count/sum(count) * 100)
#'
#' ggplot(df, aes(x = as.factor(value), y = count)) +
#'   geom_col() + labs(x = 'year of change', y = 'number of pixels',
#'                     title = 'year of last change',
#'                     subtitle = 'by pixel',
#'                     caption = 'data by MapBiomas(2021)') +
#'   theme_bw()
#'
#' ggplot(df, aes(x = as.factor(value), y = freq_100)) +
#'   geom_col() +
#'   geom_text(aes(label = round(freq_100, 1) ), nudge_y = 2, size = 3) +
#'   labs(x = 'year of change', y = '%',
#'        title = 'year of last change',
#'        subtitle = 'by pixel',
#'        caption = 'data by MapBiomas(2021)') +
#'   theme_bw()

lu_last_change <- function(r, cores = 0) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rm1 = terra::app(r, LastChange, cores = cores)
  return(rm1)
}


#' Land Use Mode
#' @description Calculate Mode - the most frequent class in a stack or land use rasters
#' @param r SpatRaster
#' @param cores num of processors cores to be used
#' @return SpatRaster with the id of modal class
#' @export
#' @importFrom terra app
#' @examples
#' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' plot(mb[[1]], main = names(mb)[1])
#' class_mode = lu_mode(r = mb, cores = 0)
#' plot(class_mode)
#' writeRaster(class_mode, filename = 'class_mode.tif', overwrite = TRUE,
#'             gdal = c("COMPRESS=LZW"), datatype = "INT1U")
#'
#' df = terra::freq(class_mode) |>
#'   data.frame() |>
#'   subset(select = -layer) |>
#'   transform(freq_100 = count/sum(count) * 100)
#'
#' ggplot(df, aes(x = as.factor(value), y = count)) +
#'   geom_col() + labs(x = 'LU Class', y = 'number of pixels',
#'                     title = 'Class Mode',
#'                     subtitle = 'by pixel',
#'                     caption = 'data by MapBiomas(2021)') +
#'   theme_bw()
#'
#' ggplot(df, aes(x = as.factor(value), y = freq_100)) +
#'   geom_col() +
#'   geom_text(aes(label = round(freq_100, 1) ), nudge_y = 1, size = 3) +
#'   labs(x = 'LU Class', y = '%',
#'        title = 'Class Mode',
#'        subtitle = 'by pixel',
#'        caption = 'data by MapBiomas(2021)') +
#'   theme_bw()

lu_mode <- function(r, cores = 0) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rm1 = terra::app(r, Mode, cores = cores)
  return(rm1)
}




#' Maximum years of Sequence without LU change
#' @description Determines the maximum sequence of years without land use change
#' @param r SpatRaster input rasters with one layer by year
#' @param cores integer number of cores to be used dot multitasking
#' @importFrom terra app
#' @return SpatRaster
#' @export
#' @examples
#' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' plot(mb[[1]], main = names(mb)[1])
#' max_seq = lu_max_sequence_cont(r = mb, cores = 0)
#' plot(max_seq)
#' writeRaster(max_seq, filename = 'max_seq.tif', overwrite = TRUE,
#'            gdal = c("COMPRESS=LZW"), datatype = "INT1U")
#'
#' df = terra::freq(max_seq) |>
#'  data.frame() |>
#'  subset(select = -layer) |>
#'  transform(freq_100 = count/sum(count) * 100)
#'
#' ggplot(df, aes(x = as.factor(value), y = count)) +
#'  geom_col() + labs(x = 'years', y = 'area (pixels)',
#'                    title = 'Maximum sequence of years without LU change',
#'                    caption = 'data by MapBiomas(2021)') +
#'  theme_bw()
#'
#'ggplot(df, aes(x = as.factor(value), y = freq_100)) +
#'  geom_col() +
#'  geom_text(aes(label = round(freq_100, 1) ), nudge_y = 2, size = 3) +
#'  labs(x = 'years', y = 'area (%)',
#'       title = 'Maximum sequence of years without LU change',
#' caption = 'data by MapBiomas(2021)') +
#' theme_bw()

lu_max_sequence_cont <- function(r, cores = 0) {
   if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rmaxfreq = terra::app(r, max_class_freq, cores = cores)
  names(rmaxfreq) = 'maximum year of continuos sequence'
  return(rmaxfreq)
}


#' Class Maximum Sequence without LU change
#' @description Determines the Class id with  maximum sequence of years without land use change
#' @param r SpatRaster input rasters with one layer by year
#' @param cores integer number of cores to be used dot multitasking
#' @importFrom terra app
#' @return SpatRaster
#' @importFrom terra app
#' @export
#' @examples
#' library(MBprocess)
#' library(terra)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' mmax = lu_class_max_sequence_cont(r = mb, cores = 0)
#' fr = terra::freq(mmax)
#' print(fr)

lu_class_max_sequence_cont <- function(r, cores = 0) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rclassmaxseq = terra::app(r, max_class_id, cores = cores)
  names(rclassmaxseq) = 'id class maximum sequence'
  return(rclassmaxseq)
}

#' Number of LU changes
#' @description Determines the number of times that land use change in each pixel
#' @param r SpatRaster input rasters with one layer by year
#' @param cores integer number of cores to be used to do multitasking
#' @importFrom terra app
#' @return SpatRaster
#' @export
#' @examples
#' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' plot(mb[[1]], main = names(mb)[1])
#' num_change = lu_n_change(r = mb, cores = 0)
#' plot(num_change)
#' writeRaster(num_change, filename = 'num_change.tif', overwrite = TRUE,
#' gdal = c("COMPRESS=LZW"), datatype = "INT1U")
#'
#' df = terra::freq(num_change) |>
#' data.frame() |>
#'   subset(select = -layer) |>
#'   transform(freq_100 = count/sum(count) * 100)
#'
#' ggplot(df, aes(x = as.factor(value), y = count)) +
#'   geom_col() + labs(x = 'Number of changes', y = 'number of pixels',
#'                     title = 'Number of Land Use change',
#'                     subtitle = 'by pixel',
#'                     caption = 'data by MapBiomas(2021)') +
#'   theme_bw()
#'
#' ggplot(df, aes(x = as.factor(value), y = freq_100)) +
#'   geom_col() +
#'   geom_text(aes(label = round(freq_100, 1) ), nudge_y = 2, size = 3) +
#'   labs(x = 'number of changes', y = '%',
#'        title = 'Number of Land Use change',
#'        subtitle = 'by pixel',
#'        caption = 'data by MapBiomas(2021)') +
#'   theme_bw()

lu_n_change <- function(r, cores = 0) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rnchange = terra::app(r, nchange, cores = cores)
  names(rnchange) = 'class number of changes'
  return(rnchange)
}



#' Diversity
#' @description calculates the different number os class by cell/pixel
#' @param r SpatRaster input rasters with one layer by year
#' @param cores  integer number of cores to be used to do multitasking
#' @return SpatRaster
#' @importFrom terra app
#' @export
#' @examples
#' #' library(MBprocess)
#' library(terra)
#' library(ggplot2)
#' lu <- system.file("extdata", 'lu_test.tif', package = "MBprocess")
#' mb <- terra::rast(lu)
#' plot(mb[[1]], main = names(mb)[1])
#' diversity = MBprocess::lu_diversity(r = mb, cores = 0)
#' plot(diversity)
#' writeRaster(diversity, filename = 'diversity.tif', overwrite = TRUE,
#' gdal = c("COMPRESS=LZW"), datatype = "INT1U")
#'
#' df = terra::freq(diversity) |>
#' data.frame() |>
#'   subset(select = -layer) |>
#'   transform(freq_100 = count/sum(count) * 100)
#'
#' ggplot(df, aes(x = as.factor(value), y = count)) +
#'   geom_col() + labs(x = 'diversity', y = 'number of pixels',
#'                     title = 'Diversity',
#'                     subtitle = 'by pixel',
#'                     caption = 'data by MapBiomas(2021)') +
#'   theme_bw()
#'
#' ggplot(df, aes(x = as.factor(value), y = freq_100)) +
#'   geom_col() +
#'   geom_text(aes(label = round(freq_100, 1) ), nudge_y = 2, size = 3) +
#'   labs(x = 'number of changes', y = '%',
#'        title = 'Number of different Land Use',
#'        subtitle = 'by pixel',
#'        caption = 'data by MapBiomas(2021)') +
#'   theme_bw()

lu_diversity <- function(r, cores = 0) {
  if (class(r) == 'RasterStack') {
    r = terra::rast(r)
  } else {
    if (class(r) != 'SpatRaster') {
      stop('r must be a SpatRaster or a RasterStack')
    }
  }
  rdiversity = terra::app(r, diversity, cores = cores)
  names(rdiversity) = 'diversity'
  return(rdiversity)
}
