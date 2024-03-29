% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{assemble_diff_command}
\alias{assemble_diff_command}
\title{Assembles the diff command between two code files}
\usage{
assemble_diff_command(
  code_path_left,
  code_path_right,
  output_left,
  output_right
)
}
\arguments{
\item{code_path_left}{path to "left" file}

\item{code_path_right}{path to "right" file}

\item{output_left}{piece of output file name representing left}

\item{output_right}{piece of output file name representing right}
}
\value{
git diff command
}
\description{
Assembles the diff command between two code files
}
