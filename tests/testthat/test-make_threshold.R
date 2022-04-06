test_that("the output proper", {
  percentile <- seq(0.01,0.99,0.01)
  pval <- rnorm(99)
  hazard.ratio <- rnorm(99)
  tmp <- data.frame(percentile, pval, hazard.ratio)
  tmp.list <- list(tmp,tmp,tmp)
  names(tmp.list) <- c("test1","test2","test3")
  test <- lapply(names(tmp.list), function(x) make_threshold(tmp.list,x))
  expect_is(test, "list")
  expect_is(test[[1]], c("gg","ggplot"))
})

