test_that("multiplication works", {
  library(ggplot2)

  # p1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  #   geom_point() +
  #   labs(
  #     title = "A ROSE BY ANY OTHER NAME",
  #     subtitle = "IRISES PETAL ATTRIBUTE",
  #     caption = "SOURCE: R.A. FISHER",
  #     x = "PETAL LENGTH",
  #     y = "PETAL WIDTH"
  #     )
  # gcplot(p1)
  expect_equal(2 * 2, 4)
})
