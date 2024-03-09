test_that("pkg extractors yield the same outputs", {
  expect_equal(getpkgsv6(), getpkgsv7())
  expect_equal(getpkgsv1(), getpkgsv2())
  expect_equal(getpkgsv1(), getpkgsv3())
})
