% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitlsmeans.R
\name{fitlsmeans}
\alias{fitlsmeans}
\title{fitlsmeans function}
\usage{
fitlsmeans(form, effect, meanType = "intercept", alpha = 0.05, ...)
}
\arguments{
\item{form}{object of class formula}

\item{meanType}{mean to use for calculating CV. Options include 'raw' = mean of y, 'effect' = mean of estimated effects, and 'intercept', the estimated intercept of th emodel fit. Default is "intercept". To reproduce CVs calculated by proc glm in SAS, use "intercept", or set contrasts to use sum contrsaints with 'options(contrasts = c("contr.sum", "contr.sum")'}

\item{alpha}{numeric. Significance threshold. Default is 0.05}

\item{data}{data.frame.  Name of effect for which lsmeans should be calculated}
}
\value{
list. Returns a list of BLUEs, mean used for CV, CV, and LSD.
}
\description{
function to fit a linear model and calculate lsmeans, CV and LSD.
}
\details{
This function is a wrapper for lsmeans, and was made to calculate effects, CV and LSD as proc glm in SAS does. CVs are not reliable statistics for determining field trial quality.
}
\examples{
fitlsmeans(mpg ~ factor(cyl), effect = "factor(cyl)", data = mtcars)
fitlsmeans(Sepal.Length ~ Species, "Species", data = iris)
}
