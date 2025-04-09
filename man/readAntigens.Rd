\name{readAntigens}
\alias{readAntigens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardise Antigen Names
}
\description{
This function ensures that the antigens in the raw data adheres to our nomenclature format in the data processing and model steps. This function calls `readSeroData` first to read the serological raw data and
then use our nomenclature for the eight antigens of interest in PvSeroApp.
}
\usage{
readAntigens(raw_data, raw_data_filenames, platform)
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
List of data frames with relabelled column names for our antigen names.
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
######### First Download Data #########
# Download Data
utils::download.file(
  "https://github.com/dionnecargy/PvSeroApp/blob/main/data/example_data.zip?raw=TRUE",
  "data/example_data.zip",
  mode = "wb"
)
# Unzip Data
utils::unzip("data/example_data.zip")

######### For example, when using MAGPIX data #########
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

serodata_output <- readSeroData(
  raw_data = magpix_examples_data,
  raw_data_filenames = magpix_filenames,
  platform = "magpix"
)

antigen_output <- readAntigens(serodata_output)

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
