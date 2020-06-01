test_that("run_mcp works", {
  mf <- read_csv('../testdata/megaframe.csv',
                 col_types = cols(.default = col_double(),
                                  datetime = col_datetime(),
                                  model = col_character()))
  results <- read_csv('../testdata/mcp_results.csv')

  test_mcp <- MCP(mf)
  test_mcp$run()

  expect_equivalent(test_mcp$results, results)
})
