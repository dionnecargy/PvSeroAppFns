\name{getCounts}
\alias{getCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Count Data from Raw Median Fluescent Itensity Data
}
\description{
This function obtains the count data from the raw Median Fluescent Itensity (MFI). This is an interim function used for the plotCounts function. This function relies on the `readAntigens` and `readSeroData` data processing functions.
}
\usage{
getCounts(raw_data, raw_data_filenames, platform)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw_data}{String with the raw data path (reactive).}
  \item{raw_data_filenames}{String with the raw data filenames (reactive).}
  \item{platform}{"magpix" or "bioplex" (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame providing bead counts per well per plate. Designates whether wells should be repeated if there are ≤ 15 beads (repeat) or if they are sufficient with > 15 beads (sufficient beads).
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
getCounts(raw_data, raw_data_filenames, platform)
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
