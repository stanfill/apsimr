\name{edit_apsim}
\alias{edit_apsim}
\title{Edit an APSIM simulation}
\usage{
  edit_apsim(file, varP, varC, value, overwrite = T)
}
\arguments{
  \item{file}{file ending in ".apsim" to be edits}

  \item{varP}{parent variables to be edited}

  \item{varC}{child variable to be edited}

  \item{value}{new values for the defined variables}

  \item{overwrite}{T/F depending on if the old file should
  be over written or a new one should be saved}
}
\value{
  nothing, new .apsim file
}
\description{
  Edit bits and pieces of an APSIM simulation to automate
  running
}

