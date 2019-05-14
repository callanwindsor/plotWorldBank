
  test_that("Plot layers match expectations",{
    p <- plotWorldBank(country='US', indicator='GC.DOD.TOTL.GD.ZS', start_year='2010', end_year='2018', color='blue')
    expect_is(p, 'ggplot')
  })
