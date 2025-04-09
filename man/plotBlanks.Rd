\name{plotBlanks}
\alias{plotBlanks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Raw Median Fluorescent Intensity Blanks Data
}
\description{
This function gets the blank sample data and plots the blank sample Median Fluorescent Intensity (MFI) values.
}
\usage{
plotBlanks(antigen_output, experiment_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_output}{Output from `readAntigens` (reactive).}
  \item{experiment_name}{User-input experiment name (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Bar plot showing whether MFI values for the blanks for each antigen per plate is above or below the threshold MFI = 50 (ggplot).
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
plotBlanks(antigen_output, "experiment_test")
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
