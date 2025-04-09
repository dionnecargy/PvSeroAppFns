\name{getAntigenCounts}
\alias{getAntigenCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Counts per Antigen from readAntigens Output
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAntigenCounts(antigens_output, plate_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{antigens_output}{Output from `readAntigens` (reactive).}
  \item{plate_name}{Plate name inside of the plate layout file. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Long list of bead counts per antigen
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
getAntigenCounts(antigens_output, plate_list)
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
