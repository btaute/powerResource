context('SMS')

test_that("sms files import", {
  sms_flagged <- readr::read_csv('../testdata/sms_flagged.csv')
  sms_files <- dir(path = "../testdata/Groundwork/", pattern = "*.csv") %>%
    paste0('../testdata/Groundwork/', .)

  expect_equivalent(read_groundwork(sms_files), sms_flagged)
  expect_error(read_groundwork('no_file'), paste0(getwd(), 'no_file cannot be found.'))
  expect_error(read_groundwork('../testdata/excel.xlsx'), 'File is not a CSV.')
})


test_that('prepare for megaframe', {
  sms_flagged <- readr::read_csv('../testdata/sms_flagged.csv')
  sms <- readr::read_csv('../testdata/sms.csv')

  expect_equivalent(prepare_sms_flagged_for_megaframe(sms_flagged), sms)
})
