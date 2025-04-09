\name{plotStds}
\alias{plotStds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Raw Median Fluorescent Intensity of Standard Curve Data
}
\description{
This function gets the standards data and plots the standard curves.
}
\usage{
plotStds(antigen_output, experiment_name, location)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_output}{Output from `readAntigens` (reactive).}
  \item{experiment_name}{User-input experiment name (reactive).}
  \item{location}{"PNG" or "ETH" to filter WEHI standard curve data (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Dot and line plot of standard curves (S1-S10) with PNG or Ethiopia stds underneath (ggplot). WEHI-acceptable standard curve data on background of plot with user data.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Shazia Ruybal-Pesantez, Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plotStds(antigen_output, "experiment_test", "ETH")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
