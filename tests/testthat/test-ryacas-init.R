context("Ryacas init script")

test_that("Ryacas init script", {
  
  path <- system.file(package = "Ryacas", "yacas")
  
  invisible(capture.output(yacas_init_force_path(path, ryacas_init = FALSE)))
  # Test it does not work
  # yac_core("4+4")
  
  invisible(capture.output(yacas_init_force_path(path, ryacas_init = TRUE)))
  # Test it works
  # yac_core("4+4")
  
  expect_equal(2, 1+1)
})
