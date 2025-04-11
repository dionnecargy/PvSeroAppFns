\name{MFItoRAU_ETH}
\alias{MFItoRAU_ETH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Median Fluorescent Intensity (MFI) to Relative Antibody Units (RAU) conversion (ETH)
}
\description{
This function fits a 5-parameter logistic standard curve to the dilutions of the positive controls for each protein and converts the MFI values into relative antibody units (RAU) written by Eamon Conway.
}
\usage{
MFItoRAU_ETH(antigen_output, plate_list, counts_QC_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigen_output}{Output from `readAntigens` (reactive).}
  \item{plate_list}{Output from `readPlateLayout` (reactive).}
  \item{counts_QC_output}{Output from `getCountsQC` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of three data frames:
  \item{1}{Data frame with  MFI data, converted RAU data and matched SampleID's.}
  \item{2}{Plot information for `plotModel` function.}
  \item{3}{Data frame of RAU data for random forest classification use. }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eamon Conway, Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MFItoRAU_ETH(antigen_output, plate_list, counts_QC_output)
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
