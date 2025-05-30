\name{plotModel_PNG}
\alias{plotModel_PNG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) Results Data (PNG)
}
\description{
This function gets the Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) model results data and plots the model fits based on MFItoRAU_PNG.
}
\usage{
plotModel(mfi_to_rau_output, antigen_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_output}{Output from `readAntigens` (reactive).}
  \item{mfi_to_rau_output}{Output from `MFItoRAU_PNG` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of dot and line plots of MFI to RAU model standard curve, with each one representing an individual plate (ggplots).
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
plotModel(mfi_to_rau_output, antigen_output)
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
