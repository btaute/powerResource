test_that("megaframe builder works", {
  sms <- readr::read_csv('../testdata/sms.csv')

  cpr_list <- list()
  cpr_list[['cpr3.4']] <- read_csv('../testdata/tidycpr/cpr34.csv')
  cpr_list[['cpr3.3']] <- read_csv('../testdata/tidycpr/cpr33.csv')

  vaisala_list <- list()
  vaisala_list[['vaisala1.2.3']] <- read_csv('../testdata/tidyvaisala/vaisala123.csv')
  vaisala_list[['vaisala2.1.3']] <- read_csv('../testdata/tidyvaisala/vaisala213.csv')

  mf <- read_csv('../testdata/megaframe.csv',
                 col_types = cols(.default = col_double(),
                                  datetime = col_datetime(),
                                  model = col_character()))

  expect_equivalent(compile_megaframe(sms, cpr_list, vaisala_list), mf)
})
