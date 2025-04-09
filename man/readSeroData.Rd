\name{readSeroData}
\alias{readSeroData}
\title{
Read Serological Data
}
\description{
This function imports the raw data from the Magpix or Bioplex machine and matches the sample names from the plate layout based on their plate/well location.
}
\usage{
readSeroData(raw_data, raw_data_filenames, platform)
}
\arguments{
  \item{raw_data}{String with the raw data path (reactive).}
  \item{raw_data_filenames}{String with the raw data filenames (reactive).}
  \item{platform}{"magpix" or "bioplex" (reactive).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of data frames:
  \item{raw_data}{Raw data input.}
  \item{results}{Cleaned MFI of all samples.}
  \item{counts}{Count data for all samples.}
  \item{blanks}{Blanks data.}
  \item{stds}{Standards only.}
  \item{run}{Run information.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Shazia Ruybal-Pesántez, Dionne Argyropoulos
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

######### First Download Data #########
# Download Data
utils::download.file(
  "https://github.com/dionnecargy/PvSeroApp/blob/main/data/example_data.zip?raw=TRUE",
  "data/example_data.zip",
  mode = "wb"
)
# Unzip Data
utils::unzip("data/example_data.zip")

######### When using MAGPIX data #########
magpix_examples_data <- c(
  "data/example_MAGPIX_plate1.csv",
  "data/example_MAGPIX_plate2.csv",
  "data/example_MAGPIX_plate3.csv"
)

magpix_filenames <- c(
  "example_MAGPIX_plate1.csv",
  "example_MAGPIX_plate2.csv",
  "example_MAGPIX_plate3.csv"
)

readSeroData(
  raw_data = magpix_examples_data,
  raw_data_filenames = magpix_filenames,
  platform = "magpix"
)

######### When using Bioplex data #########
bioplex_examples_data <- c(
  "data/example_BioPlex_plate1.xlsx",
  "data/example_BioPlex_plate2.xlsx",
  "data/example_BioPlex_plate3.xlsx"
)

bioplex_filenames <- c(
  "example_BioPlex_plate1.xlsx",
  "example_BioPlex_plate2.xls",
  "example_BioPlex_plate3.xls"
)

readSeroData(
  raw_data = bioplex_examples_data,
  raw_data_filenames = bioplex_filenames,
  platform = "bioplex"
)

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
