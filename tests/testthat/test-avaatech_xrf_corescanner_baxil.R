test_that("correct baxil csv file is parsed", {
  correct_baxil_csv_a <- carrrotter:::parse_avaatech_baxil_csv(test_path("fixtures", "testthat_fixture_correctbAXILcsv.csv"))
  expected_colnames <- c("CoreID", "Voltage", "Current", "Spectrum", "Depth", "User",
                         "Live time", "Real time", "Excitation", "Throughput", "Element",
                         "AbsLine", "Scattering", "Chi2", "Area", "AreaStd", "cps", "cpsStd"
  )

  expect_s3_class(correct_baxil_csv_a, "data.frame")
  expect_equal(names(correct_baxil_csv_a), expected_colnames)
  expect_type(correct_baxil_csv_a[["Depth"]], "double")
  expect_type(correct_baxil_csv_a[["cps"]], "double")
})

test_that("correct baxil csv file b is parsed", {
  correct_baxil_csb_b <- carrrotter:::parse_avaatech_baxil_csv(test_path("fixtures", "testthat_fixture_correctbAXILcsv_coreB.csv"))

  expected_colnames <- c("CoreID", "Voltage", "Current", "Spectrum", "Depth", "User",
                         "Live time", "Real time", "Excitation", "Throughput", "Element",
                         "AbsLine", "Scattering", "Chi2", "Area", "AreaStd", "cps", "cpsStd"
  )

  expect_s3_class(correct_baxil_csb_b, "data.frame")
  expect_equal(names(correct_baxil_csb_b), expected_colnames)
  expect_type(correct_baxil_csb_b[["Depth"]], "double")
  expect_type(correct_baxil_csb_b[["cps"]], "double")
})

test_that("batch parsing works", {
  correct_csv_files_paths <- test_path("fixtures", c("testthat_fixture_correctbAXILcsv.csv", "testthat_fixture_correctbAXILcsv_coreB.csv"))
  correct_batch_parsed_rds <- readRDS(test_path("fixtures", "testthat_fixture_correctbAXIL_batch.rds"))

  correct_baxil_batch_parsed <- carrrotter::batch_process_avaatech_baxil_csv(correct_csv_files_paths)

  expect_s3_class(correct_baxil_batch_parsed, "data.frame")
  expect_type(correct_baxil_batch_parsed[["Depth"]], "double")
  expect_type(correct_baxil_batch_parsed[["cps"]], "double")
  expect_equal(nrow(correct_baxil_batch_parsed), 180L)
  expect_equal(length(unique(correct_baxil_batch_parsed[["CoreID"]])), 2L)
  expect_equal(correct_batch_parsed_rds, correct_baxil_batch_parsed)
})

test_that("old winaxil files dont work", {
  old_winaxil_fpath <- test_path("fixtures", "testthat_fixture_old_winaxil_file.csv")
  expect_error(carrrotter:::parse_avaatech_baxil_csv(old_winaxil_fpath))
})

test_that("corrupt spectrum field fails parsing", {
  corrupt_spectrum_baxil_fpath <- test_path("fixtures", "testthat_fixture_corrupt_spectrumfield.csv")
  expect_error(carrrotter:::parse_avaatech_baxil_csv(corrupt_spectrum_baxil_fpath))
})

test_that("missing sample field fails parsing", {
  missing_sample_field_fpath <- test_path("fixtures", "testthat_fixture_missing_sample_field.csv")
  expect_error(carrrotter:::parse_avaatech_baxil_csv(missing_sample_field_fpath))
})
