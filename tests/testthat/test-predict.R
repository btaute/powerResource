test_that('adjusting tmy works', {
  tmy <- read_csv('../testdata/cpr_tmy.csv', skip = 1) %>%
    tidy_cpr()
  mcp_results <- read_csv('../testdata/mcp_results.csv')
  tmy_adj_ghi <- read_csv('../testdata/tmy_adj_ghi.csv')
  tmy_adj_ghiws <- read_csv('../testdata/tmy_adj_ghiws.csv')
  tmy_adj_ghiwstemp <- read_csv('../testdata/tmy_adj_ghiwstemp.csv')

  expect_equivalent(scale_tmy(mcp_results, tmy, 'cpr3.4', 'daily', 'ghi'), tmy_adj_ghi)
  expect_equivalent(scale_tmy(mcp_results, tmy, 'cpr3.4', 'daily', c('ghi', 'ws')), tmy_adj_ghiws)
  expect_equivalent(scale_tmy(mcp_results, tmy, 'cpr3.4', 'daily', c('ghi', 'ws', 'temp')), tmy_adj_ghiwstemp)
})
