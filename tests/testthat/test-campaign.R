test_that('campaign works', {
  mf <- read_csv('../testdata/megaframe.csv',
                 col_types = cols(.default = col_double(),
                                  datetime = col_datetime(),
                                  model = col_character()))
  results <- read_csv('../testdata/campaign_results.csv')

  expect_equivalent(run_sms_campaign(mf), results)
})
