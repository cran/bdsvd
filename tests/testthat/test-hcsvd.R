require(mvtnorm)
set.seed(1)
Rho <- hcsvd.cor.sim(p = 40, b = 5, design = "a")
X <- scale(rmvnorm(500, mean = rep(0, 40), sigma = Rho, checkSymmetry = FALSE))
R <- cor(X)

cluster.5  <- rep(1:5,  each = 8)
cluster.10 <- rep(1:10, times = ifelse(1:10 %in% seq(1, 10, 2), 6, 2))
cluster.15 <- rep(1:15, times = ifelse(1:15 %in% seq(1, 15, 3), 4, 2))
cluster.20 <- rep(1:20, each = 2)

hcsvd.obj <- hcsvd(R, q = "Kaiser", linkage = "single")
hc <- hcsvd.obj$hclust

test_that("hcsvd cluster detection works for single linkage and design (a)",{
  expect_equal(as.vector(cutree(hc, k =  5)), cluster.5)
  expect_equal(as.vector(cutree(hc, k = 10)), cluster.10)
  expect_equal(as.vector(cutree(hc, k = 15)), cluster.15)
  expect_equal(as.vector(cutree(hc, k = 20)), cluster.20)
})

hcsvd.obj <- hcsvd(R, q = "Kaiser", linkage = "average")
hc <- hcsvd.obj$hclust

test_that("hcsvd cluster detection works for average linkage and design (a)",{
  expect_equal(as.vector(cutree(hc, k =  5)), cluster.5)
  expect_equal(as.vector(cutree(hc, k = 10)), cluster.10)
  expect_equal(as.vector(cutree(hc, k = 15)), cluster.15)
  expect_equal(as.vector(cutree(hc, k = 20)), cluster.20)
})




set.seed(1)
Rho <- hcsvd.cor.sim(p = 60, b = 20, design = "b")
X <- scale(rmvnorm(500, mean = rep(0, 60), sigma = Rho, checkSymmetry = FALSE))
R <- cor(X)

cluster.20 <- rep(1:20, each = (3))
cluster.40 <- rep(1:40, times = ifelse(1:40 %in% seq(1, 39, 2), 2, 1))

hcsvd.obj <- hcsvd(R, q = "Kaiser", linkage = "single")
hc <- hcsvd.obj$hclust

test_that("hcsvd cluster detection works for single linkage and design (b)",{
  expect_equal(as.vector(cutree(hc, k = 20)), cluster.20)
  expect_equal(as.vector(cutree(hc, k = 40)), cluster.40)
})

hcsvd.obj <- hcsvd(R, q = "Kaiser", linkage = "average")
hc <- hcsvd.obj$hclust

test_that("hcsvd cluster detection works for average linkage and design (b)",{
  expect_equal(as.vector(cutree(hc, k = 20)), cluster.20)
  expect_equal(as.vector(cutree(hc, k = 40)), cluster.40)
})

