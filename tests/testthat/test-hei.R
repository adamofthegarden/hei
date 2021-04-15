test_that("hei() outputs the input data frame with additional HEI scoring variables", {

  x <- read.csv(".\\tests\\testthat\\asa_example_df.csv")
  names.components <- c("C1_TOTALVEG",
                        "C2_GREEN_AND_BEAN",
                        "C3_TOTALFRUIT",
                        "C4_WHOLEFRUIT",
                        "C5_WHOLEGRAIN",
                        "C6_TOTALDAIRY",
                        "C7_TOTPROT",
                        "C8_SEAPLANT_PROT",
                        "C9_FATTYACID",
                        "C10_SODIUM",
                        "C11_REFINEDGRAIN",
                        "C12_SFAT",
                        "C13_ADDSUG")
  names.hei <- c("HEI2015_TOTAL_SCORE",
                 paste0("HEI2015_", names.components))
  names.new.totals <- c("FWHOLEFRT",
                        "MONOPOLY",
                        "VTOTALLEG",
                        "VDRKGRLEG",
                        "PFALLPROTLEG",
                        "PFSEAPLANTLEG")
  names.density <- c("VEGDEN",
                      "GRBNDEN",
                      "FRTDEN",
                      "WHFRDEN",
                      "WGRNDEN",
                      "DAIRYDEN",
                      "PROTDEN",
                      "SEAPLDEN")
  all.new.names <- c(names.hei,
                     names.new.totals,
                     names.density)

  expect_type(hei(x),
           "list")
  expect_s3_class(hei(x),
          "data.frame")
  expect_length(hei(x), length(x) + length(all.new.names))


})

#> Test passed 😸
