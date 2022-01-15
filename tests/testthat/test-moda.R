a <- terra::rast(matrix(c(12,11,11,
                          12,11,11,
                          11,NA,13), ncol = 3, byrow = TRUE))

b <- terra::rast(matrix(c(12,12,12,
                          11,12,12,
                          14,12,13), ncol = 3, byrow = TRUE))

c <- terra::rast(matrix(c(13,9,13,
                          11,13,13,
                          13,10,13), ncol = 3, byrow = TRUE))

d <- terra::rast(matrix(c(10,10,10,
                          12,10,13,
                          10,10,10), ncol = 3, byrow = TRUE))

md = terra::rast(matrix(c(12,11,11,
                          12,11,13,
                          11,NA,13), ncol = 3, byrow = TRUE))

st <- c(a, b, c, d)
md <- MBprocess::lu_mode(st, cores = 0)
fr <- terra::freq(md)
testthat::test_that(desc = 'Mode',
                    code = {
                      v4 = fr[1,3]
                      names(v4) = NULL
                      testthat::expect_equal(v4, 4)
                    })
