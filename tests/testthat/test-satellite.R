test_that('read_cpr works', {
  cpr_files <- dir(path = "../testdata/CPR/", pattern = "*.csv") %>%
    paste('../testdata/CPR/', ., sep = '')

  cpr_list <- list()
  cpr_list[['cpr3.3']] <- read_csv('../testdata/tidycpr/cpr33.csv')
  cpr_list[['cpr3.4']] <- read_csv('../testdata/tidycpr/cpr34.csv')

  expect_equivalent(read_cpr(cpr_files), cpr_list)
  expect_equivalent(read_cpr(cpr_files[1]), cpr_list[1])
  expect_error(read_cpr('no_file'), paste0(getwd(), 'no_file cannot be found.'))
  expect_error(read_cpr('../testdata/excel.xlsx'), 'File is not a CSV.')
})


test_that("read_vaisala works", {
  vaisala_files <- dir(path = "../testdata/Vaisala/", pattern = "*.csv") %>%
    paste('../testdata/Vaisala/', ., sep = '')

  vaisala_list <- list()
  vaisala_list[['vaisala1.2.3']] <- read_csv('../testdata/tidyvaisala/vaisala123.csv')
  vaisala_list[['vaisala2.1.3']] <- read_csv('../testdata/tidyvaisala/vaisala213.csv')

  expect_equivalent(read_vaisala(vaisala_files), vaisala_list)
  expect_equivalent(read_vaisala(vaisala_files[1]), vaisala_list[1])
  expect_error(read_vaisala('no_file'), paste0(getwd(), 'no_file cannot be found.'))
  expect_error(read_vaisala('../testdata/excel.xlsx'), 'File is not a CSV.')
})
