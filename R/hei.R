#' Calculate Healthy Eating Index (HEI) 2015 scores:
#' total and components 1 through 13
#' @param df data.frame
#'
#' @return data.frame
#' @export
#'
#'
#'
#'
#'
hei <- function(df){

  # sums and names#####
  sum.in.asa <- c("F_TOTAL",
                  "G_WHOLE",
                  "D_TOTAL")
  intakes <- c("SODI",
               "G_REFINED",
               "SFAT",
               "ADD_SUGARS")

  to.numeric <- c(sum.in.asa,
                  "KCAL",
                  intakes,
                  "F_CITMLB",
                  "F_OTHER",
                  "MFAT",
                  "PFAT",
                  "V_TOTAL",
                  "V_LEGUMES",
                  "V_DRKGR",
                  "PF_MPS_TOTAL",
                  "PF_EGGS",
                  "PF_NUTSDS",
                  "PF_SOY",
                  "PF_LEGUMES",
                  "PF_SEAFD_HI",
                  "PF_SEAFD_LOW")

  df[, to.numeric] <- sapply(df[, to.numeric],
                             as.numeric)

  df$FWHOLEFRT <- with(df, F_CITMLB+
                         F_OTHER)
  df$MONOPOLY <- with(df, MFAT+
                        PFAT)
  df$VTOTALLEG <- with(df, V_TOTAL+
                         V_LEGUMES)
  df$VDRKGRLEG <- with(df, V_DRKGR+
                         V_LEGUMES)
  df$PFALLPROTLEG <- with(df,
                          PF_MPS_TOTAL+
                            PF_EGGS+
                            PF_NUTSDS+
                            PF_SOY+
                            PF_LEGUMES)
  df$PFSEAPLANTLEG <- with(df,
                           PF_SEAFD_HI+
                             PF_SEAFD_LOW+
                             PF_NUTSDS+
                             PF_SOY+
                             PF_LEGUMES)

  # components 1 to 8####
  tnames <- c("VTOTALLEG",
              "VDRKGRLEG",
              "F_TOTAL",
              "FWHOLEFRT",
              "G_WHOLE",
              "D_TOTAL",
              "PFALLPROTLEG",
              "PFSEAPLANTLEG")
  dnames <- c("VEGDEN",
              "GRBNDEN",
              "FRTDEN",
              "WHFRDEN",
              "WGRNDEN",
              "DAIRYDEN",
              "PROTDEN",
              "SEAPLDEN")
  cnames1to8 <- c("C1_TOTALVEG",
                  "C2_GREEN_AND_BEAN",
                  "C3_TOTALFRUIT",
                  "C4_WHOLEFRUIT",
                  "C5_WHOLEGRAIN",
                  "C6_TOTALDAIRY",
                  "C7_TOTPROT",
                  "C8_SEAPLANT_PROT")

  # loop
  dfac <- c(1.1,
            0.2,
            0.8,
            0.4,
            1.5,
            1.3,
            2.5,
            0.8)

  for (i in seq(dnames)){
    kcal <- df$KCAL
    den <- df[, tnames[i]]/(kcal/1000)
    mfac <- ifelse(dnames[i] %in% c("WGRNDEN", "DAIRYDEN"),
                   10,
                   5)
    c <- mfac*(den/dfac[i])
    c <- ifelse(c > mfac,
                mfac,
                ifelse(den %in% 0,
                       0,
                       c))
    df[, paste0("HEI2015_", cnames1to8[i])] <- c
    df[, dnames[i]] <- den
  }

  # component 9####
  FARATIO <-  df$MONOPOLY/df$SFAT
  FARMIN <- 1.2
  FARMAX <- 2.5
  c9 <- paste0("HEI2015_",
               "C9_FATTYACID")
  df[, c9] <- with(df,
                   ifelse(SFAT == 0 & MONOPOLY == 0, 0,
                          ifelse(SFAT == 0 & MONOPOLY > 0, 10,
                                 ifelse(FARATIO >= FARMAX, 10,
                                        ifelse(FARATIO <= FARMIN, 0,
                                               10*((FARATIO - FARMIN)/(FARMAX - FARMIN)))))))

  # components 10 to 13####
  mins <- c(1.1,
            1.8,
            8,
            6.5)

  maxs <- c(2.0,
            4.3,
            16,
            26)

  cnames10to13 <- c("C10_SODIUM",
                    "C11_REFINEDGRAIN",
                    "C12_SFAT",
                    "C13_ADDSUG")

  for (i in seq(intakes)){
    intake <- intakes[i]
    percentify <- ifelse(intake %in% c("SODI", "G_REFINED"),
                         1,
                         100)
    k.units <- ifelse(intake %in% c("G_REFINED"),
                      1000,
                      1)
    mfac <- ifelse(intake %in% c("SFAT"), 9,
                   ifelse(intake %in% c("ADD_SUGARS"), 16,
                          1))

    den <- percentify*mfac*df[, intakes[i]]/(kcal/k.units)
    df[, paste0("HEI2015_", cnames10to13[i])] <- ifelse(den <= mins[i], 10,
                                                       ifelse(den >= maxs[i], 0,
                                                              10 - (10*(den - mins[i])/(maxs[i] - mins[i]))))
  }

  components <- c(paste0("HEI2015_",
                         c(cnames1to8,
                           "C9_FATTYACID",
                           cnames10to13)))
  df[df$KCAL %in% 0, components] <- 0
  df$HEI2015_TOTAL_SCORE <- rowSums(df[, components])

  df
}
