test_that("theme_gc() works", {
  p <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point() + theme_gc()
  #expect_true(p$theme$axis.title.y$inherit.blank)
  expect_equal(p$axis.title.y, NULL)
})
