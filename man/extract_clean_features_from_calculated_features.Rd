% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{extract_clean_features_from_calculated_features}
\alias{extract_clean_features_from_calculated_features}
\title{Extract only the features from the structure generated by calculate_versions_from_versions}
\usage{
extract_clean_features_from_calculated_features(calculated_features)
}
\arguments{
\item{calculated_features}{output from calculate_versions_from_versions}
}
\value{
dataframe with a line for each combination of alerts only with features
}
\description{
Extract only the features from the structure generated by calculate_versions_from_versions
}
