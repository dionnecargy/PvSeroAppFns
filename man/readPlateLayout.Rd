\name{readPlateLayout}
\alias{readPlateLayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Plate Layout/s
}
\description{
This function imports the plate layout. Each sheet of the plate layout ".xlsx" file must contain 13 columns (labelled Plate, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) (columns A-M) and 9 rows (Plate, A, B, C, D, E, F, G, H) (rows 1-9). *Note that the first row/column
i.e., the A1 cell in excel is called "Plate". This function also checks that the plate sheet labels are consistent with the MAGPIX file input names, as a check prior to merging downstream.
}
\usage{
readPlateLayout(plate_layout, antigen_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plate_layout_file}{An ".xlsx" file with sheets labelled plate1, plate2... etc. (reactive).}
  \item{antigen_output}{antigen_output: Output from `readAntigens` (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of data frames, with each one representing an individual plate.
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
readPlateLayout(plate_layout, antigen_output)
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
