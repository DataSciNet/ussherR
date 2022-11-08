#' Expanded data drawn from "ussher" Archbishop James Ussher's chronology of ancient history known popularly as The Annals of the World (1658).
#'
#' "usshfull" file - Expanded historical chronological textual historic events are classified by index, year, epoch (or one of the 7 ancient "Ages of the World", Biblical source book if referenced (rarely), as well as alternate dating mechanisms, such as "Anno Mundi" (age of the world) or Julian Period. Additional file "ussh.full" includes variables that may be of further interest to historians, such as Southern Kingdom and Northern Kingdom discrepant dates, and the original amalgamated Dating mechanic used by Ussher in the original text. The raw data can also be called using "ussh.raw"
#'
#' @format A tibble with 5011 rows and 8 variables:
#' \describe{
#'   \item{Index}{dbl The indexed number of Ussher's paragraphs, in ascending chronological order, added by later editors.}
#'   \item{EventTxt}{chr Ussher's original historical paragraph, cleaned for text processing}
#'   \item{YearBCAD}{dbl Harmonized year BC or AD proposed by Ussher. BC is a negative number. AD is positive.}
#'   \item{Epoch}{chr denoting which of the 7 ancient "Ages of the World" the event falls into}
#'   \item{BibBk1}{chr a Biblical Book reference, if one is included in the paragraph}
#'   \item{AnnoMund}{dbl denoting "Anno Mundus" or age of the world from Creation, or 4004 BC}
#'   \item{Season}{chr denoting which quarter of the year - Autumn, Winter, Spring or Summer that event was calculated to occur}
#'   \item{SKing}{chr denoting the year of the respective Southern Kingdom reign of the sitting king. This is a chr file because it is easily confused for a chronological date. Convert to numeric and associate with sitting king if necessary for more advanced date comparisons and algorithms}
#'   \item{NKing}{chr denoting the year of the respective Northern Kingdom reign of the sitting king. This is a chr file because it is easily confused for a chronological date. Convert to numeric and associate with sitting king if necessary for more advanced date comparisons and algorithms}
#'   \item{JulPer}{dbl denoting Julian Period date. Julian Period was backdated to creation by Ussher, following its adoption by Caesar in 43 BC}
#'   \item{Dating}{chr the original full-text string originally created by Ussher to identify, compare and harmonize the various dating methods in expressing the algorithms an historical analysis. All numeric date variables are drawn from this original date grouping}
#' }
#' @source \url{https://archive.org/stream/AnnalsOfTheWorld/Annals_djvu.txt}
#' @examples
#' data_summary(usshfull)
"usshfull"
