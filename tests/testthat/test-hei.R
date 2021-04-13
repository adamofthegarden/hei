test_that("hei() creates HEI variables", {
  asa.df <- read.csv("asa_example_df.csv")
#
#   hei.comp <- c("C1_TOTALVEG",
#                 "C2_GREEN_AND_BEAN",
#                 "C3_TOTALFRUIT",
#                 "C4_WHOLEFRUIT",
#                 "C5_WHOLEGRAIN",
#                 "C6_TOTALDAIRY",
#                 "C7_TOTPROT",
#                 "C8_SEAPLANT_PROT",
#                 "C9_FATTYACID",
#                 "C10_SODIUM",
#                 "C11_REFINEDGRAIN",
#                 "C12_SFAT",
#                 "C13_ADDSUG")
#
#   hei.names <- c("HEI2015_TOTAL_SCORE",
#                  paste0("HEI2015_",
#                         hei.comp))

  expect_type(hei(asa.df),
           "list")
  expect_s3_class(hei(asa.df),
          "data.frame")


})
