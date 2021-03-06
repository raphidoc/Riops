% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_hl_inputs.R
\name{generate.HL.inputs}
\alias{generate.HL.inputs}
\title{Produce Hydrolight-compatible input files for RTE simulations using measured IOPs}
\usage{
generate.HL.inputs(
  IOP,
  Absorption.meter = "ASPH",
  Attenuation.meter = "HS6",
  Backscattering.meter = "HS6",
  bbp.tilde = 0.018,
  waves = seq(400, 700, 5),
  Zmax = 50,
  delta.Z = 1,
  Station = "ST"
)
}
\arguments{
\item{IOP}{is a list returned by the function \code{\link{correct.merge.IOP.profile}}.}

\item{Absorption.meter}{is the instrument acronyme for the absorption meter (i.e. "ASPH", "AC9", "ACS").
Default is Absorption.meter="ASPH"}

\item{Attenuation.meter}{is the instrument acronyme for the attenuation meter (i.e. "AC9", "ACS", "BB9", "HS6").
Note that if a backscattering meter is put for attenuation, c is computed using a and bbp. In this case,
the user need to provide the bacskatteting ratio (bbp.tilde) to estimate bp (bp = bbp/bbp.tilde)
Default is Attenuation.meter="HS6"}

\item{Backscattering.meter}{is the instrument acronyme for the backscattering meter (i.e. BB9, BB3, HS6).
If NA, then no bbp file is output.
Default is Backscattering.meter="HS6"}

\item{bbp.tilde}{is the backscattering ratio need to compute the beam attenuation if only bbp is measured.
Default is bbp.tilde=0.018  (i.e. from Petzold VSF)}

\item{waves}{is a vector of wavelengths to include in the output file}

\item{Zmax}{is the maximum depth to include in the output file}

\item{delta.Z}{is the depth interval}

\item{Station}{is the station ID}
}
\description{
Produce Hydrolight-compatible input files for RTE simulations using measured IOPs
}
\author{
Simon Belanger
}
