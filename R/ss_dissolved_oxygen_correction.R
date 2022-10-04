#' Salinity correction factor for dissolved oxygen concentration measurements
#'
#' @details Dissolved oxygen concentration measured by HOBO sensors needs to be
#'   corrected for salinity (see manual:
#'   \url{https://www.onsetcomp.com/files/manual_pdfs/15603-B-MAN-U26x.pdf}).
#'
#'   The salinity correction factor can be calculated using the Benson and
#'   Krause 1984 equations or the Garcia and Gordon 1992 equations. These
#'   equations should only be used when 0 < Temperature < 40 degrees Celcius, 0
#'   < Salinity < 40 PSU and 0.5 < Pressure < 1.1 atm.
#'
#'   \bold{Benson & Krause, 1984}:
#'
#'   Used by the USGS DOTables (USGS, 2011).
#'
#'   Results from this equation should match those from the USGS DOTABLES (part
#'   C) at \url{https://water.usgs.gov/water-resources/software/DOTABLES/}.
#'
#'   Final term in Equation 32:
#'
#'   \deqn{F_{s} = exp(-Salinity * (0.017674 - 10.754 / T_{Kelvin} + 2140.7 /
#'   T_{Kelvin}^2))}
#'
#'   \bold{Garcia &  Gordon, 1992:}
#'
#'   Garcia & Gordon re-fit the Benson & Krause data with a higher order
#'   polynomial and defined a scaled temperature (\eqn{T_{s}}).
#'
#'   This correction factor is used in the SCOR WG 142 (Part C).
#'
#'   Results from this equation are very similar to the Benson & Krause
#'   correction factor (to ~4 decimal places).
#'
#'   \deqn{B0 = -6.24523E-3} \deqn{B1 = -7.37614E-3} \deqn{B2 = -1.03410E-2}
#'   \deqn{B3 = -8.17083E-3} \deqn{C0 = -4.88682E-7}
#'
#'   \deqn{F_{s} = exp(Salinity * (B0 + B1 * T_{s} + B2 * T_{s}^2 + B3 *
#'   T_{s}^3) + C0 * Salinity^2)}
#'
#'   \deqn{T_{s} = log((298.15 - Temperature) / (273.15 + Temperature))}
#'
#'   Note: the HOBO Dissolved Oxygen Assistant Software salinity correction
#'   factor uses the same form as the Garcia and Gordon equation, but different
#'   coefficients (although the results are similar).
#'
#'   \bold{References}
#'
#'   Benson, Bruce B., Krause, Daniel, (1984), \emph{The concentration and
#'   isotopic fractionation of oxygen dissolved in freshwater and seawater in
#'   equilibrium with the atmosphere, Limnology and Oceanography}, 3, doi:
#'   10.4319/lo.1984.29.3.0620.
#'
#'   Bittig, H. &.-J. (2016). \emph{SCOR WG 142: Quality Control Procedures for
#'   Oxygen and Other Biogeochemical Sensors on Floats and Gliders.
#'   Recommendations on the conversion between oxygen quantities for Bio-Argo
#'   floats and other autonomous sensor platforms.}
#'   \url{https://repository.oceanbestpractices.org/handle/11329/417}.
#'
#'   Garcia, H., and L. Gordon (1992), \emph{Oxygen solubility in seawater:
#'   Better fitting equations}, Limnol. Oceanogr., 37(6).
#'
#'   USGS. \emph{Change to Solubility Equations for Oxygen in Water.} Technical
#'   Memorandum 2011.03. USGS Office of Water Quality, 2011.
#'
#' @param dat_wide Data frame with at least one column:
#'   \code{temperature_degree_C}. Corresponding salinity (psu) data may be
#'   included in column \code{salinity_psu}. Additional columns will be ignored and
#'   returned.
#'
#' @param sal A single value of salinity (psu). This value must be specified if
#'   there is no \code{Salinity} column in \code{dat_wide}. Default is \code{Sal
#'   = NULL}. Note: if \code{Sal} is specified when there is a \code{Salinity}
#'   column in \code{dat_wide}, the function will stop with an error.
#'
#' @param method Equation to use to calculate salinity correction factor.
#'   Options are \code{method = "garcia-gordon"} (the default) and \code{method
#'   = "benson-krause"}.
#'
#' @return Returns \code{dat_wide} with additional column(s), \code{F_s} and
#'   \code{Salinity}.
#'
#' @family Dissolved Oxygen
#'
#' @export


ss_dissolved_oxygen_salinity_correction <- function(
  dat_wide,
  sal = NULL,
  method = "garcia-gordon"
){

# Error Messages ----------------------------------------------------------

  cols <- colnames(dat_wide)

  if(!(tolower(method) %in% c("garcia-gordon", "benson-krause"))){
    stop("Method argument not recognized.
         \nHINT: method must be 'garcia-gordon' or 'benson-krause'")
  }

  if("salinity_psu" %in% cols & !is.null(sal)){
    stop("Conflicting salinity values.
         \nHINT: Remove column Salinity or set Sal argument to NULL.")
  }

# calculate F_s -----------------------------------------------------------

  if(!is.null(sal)) dat_wide <- mutate(dat_wide, salinity_psu = sal)

# Benson-Krause Method ----------------------------------------------------

  if(tolower(method) == "benson-krause"){

    B0_BK <- 0.017674
    B1_BK <- -10.754
    B2_BK <-  2140.7

    dat_out <- dat_wide %>%
      mutate(
        temperature_degree_C = as.numeric(temperature_degree_C),

        # temperature in Kelvin
        T_Kelvin = temperature_degree_C + 273.15,

        # correction factor
        F_s = exp(-salinity_psu * (B0_BK + B1_BK / T_Kelvin + B2_BK / T_Kelvin^2)),
        F_s = round(F_s, digits = 4)
      ) %>%
      select(-T_Kelvin)
  }

# Garcia-Gordon Method ----------------------------------------------------

  if(tolower(method) == "garcia-gordon"){

    B0_GG = -6.24523E-3
    B1_GG = -7.37614E-3
    B2_GG = -1.03410E-2
    B3_GG = -8.17083E-3
    C0_GG = -4.88682E-7

    dat_out <- dat_wide %>%
      mutate(
        temperature_degree_C = as.numeric(temperature_degree_C),

        # scaled temperature
        T_s = log((298.15 - temperature_degree_C) / (273.15 + temperature_degree_C)),

        # correction factor
        F_s = exp(salinity_psu * (B0_GG + B1_GG * T_s + B2_GG * T_s^2 + B3_GG * T_s^3)
                    + C0_GG * salinity_psu^2),
        F_s = round(F_s, digits = 4)
      ) %>%
      select(-T_s)

  }

  dat_out

}


#' Pressure correction factor for dissolved oxygen measurements
#'
#' @details Dissolved oxygen concentration (and partial pressure?) measurements
#'   should be corrected for atmospheric pressure if the pressure deviates from
#'   substantially from 1 atm (Benson & Krause, 1984).
#'
#'   This function calculates pressure correction factor following Benson and
#'   Krause (1984), as suggested by USGS (2011). This is similar to what is
#'   described in the HOBO manual HOBO U26 Percent Saturation Calculation.pdf
#'   (except this function accounts for salinity in the water vapour
#'   calculation).
#'
#'   Equation 24 in Benson & Krause 1984:
#'
#'   \deqn{C_{p} = C_{star} * Pressure * (((1 - P_{wv} / Pressure) * (1 - theta
#'   * Pressure)) / ((1 - P_{wv}) * (1 - theta)))}
#'
#'   \deqn{F_{p} = Pressure * (((1 - P_{wv} / Pressure) * (1 - theta *
#'   Pressure)) / ((1 - P_{wv}) * (1 - theta)))}
#'
#'   \eqn{P_{wv}} is water vapour pressure, which depends on temperature and
#'   salinity and \eqn{theta} depends on the second virial coefficient of
#'   oxygen, and is calculated using temperature (see Table 2 of Benson and
#'   Krause 1984).
#'
#'   \bold{References}
#'
#'   Benson, Bruce B., Krause, Daniel, (1984), \emph{The concentration and
#'   isotopic fractionation of oxygen dissolved in freshwater and seawater in
#'   equilibrium with the atmosphere, Limnology and Oceanography}, 3, doi:
#'   10.4319/lo.1984.29.3.0620.
#'
#'   USGS. \emph{Change to Solubility Equations for Oxygen in Water.} Technical
#'   Memorandum 2011.03. USGS Office of Water Quality, 2011.
#'
#' @param dat_wide Data frame with at least one column:
#'   \code{temperature_degree_C}. Corresponding pressure (atm) data may be
#'   included in column \code{pressure_atm}. Additional columns will be ignored
#'   and returned.
#'
#' @param p_atm A single value of barometric pressure (atm). This value should
#'   be specified if there is no \code{Pressure} column in \code{dat_wide}.
#'   Default is \code{p_atm = NULL}. Note: if \code{p_atm} is specified when
#'   there is a \code{pressure_atm} column in \code{dat_wide}, function will
#'   stop with an error.
#'
#' @param sal A single value of salinity (psu). This value must be specified if
#'   there is no \code{salinity_psu} column in \code{dat_wide}. Default is
#'   \code{sal = NULL}. Note: if \code{sal} is specified when there is a
#'   \code{salinity_psu} column in \code{dat_wide}, the function will stop with
#'   an error.
#'
#' @return Returns \code{dat_wide} with additional column(s), \code{F_p} and
#'   \code{pressure_atm}.
#'
#' @family Dissolved Oxygen
#'
#' @importFrom dplyr mutate
#'
#' @export


ss_dissolved_oxygen_pressure_correction <- function(dat_wide, sal = NULL, p_atm = NULL){

  # Error Messages ----------------------------------------------------------

  cols <- colnames(dat_wide)

  if("pressure_atm" %in% cols & !is.null(p_atm)){

    stop("Conflicting pressure values.
         \nHINT: Remove column pressure_atm or set p_atm argument to NULL.")

  }

  if("salinity_psu" %in% cols & !is.null(sal)){

    stop("Conflicting salinity values.
         \nHINT: Remove column salinity_psu or set sal argument to NULL.")

  }

# Calculate F_p -----------------------------------------------------------

  if(!is.null(p_atm)) dat_wide <- mutate(dat_wide, pressure_atm = p_atm)
  if(!is.null(sal)) dat_wide <- mutate(dat_wide, salinity = sal)

  dat_wide %>%
    mutate(
      temperature_degree_C = as.numeric(temperature_degree_C),

      # temperature in Kelvin
      T_Kelvin = temperature_degree_C + 273.15,

      # Coefficient that depends on the Second Virial Coefficient of oxygen
      theta = 0.000975 - (1.426e-5) * temperature_degree_C +
        (6.436e-8) * temperature_degree_C^2,

      # partial pressure of water vapour in atm
      P_wv = (1 - 5.370e-4 * salinity_psu) *
        exp(
          18.1973 * (1 - 373.16 / T_Kelvin) +
            3.1813e-7 * (1 - exp(26.1205 * (1 - T_Kelvin / 373.16))) -
            1.8726e-2 * (1 - exp(8.03945 * (1 - 373.16 / T_Kelvin))) +
            5.02802 * log(373.16 / T_Kelvin)
        ),

      # square brackets term in Equation 24
      alt_correction = (((1 - P_wv /pressure_atm) * (1 - theta * pressure_atm)) /
        ((1 - P_wv) * (1 - theta))),

      F_p = pressure_atm * alt_correction,
      F_p = round(F_p, digits = 4)

    )

}















