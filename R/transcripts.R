#  SeqLengthHist  
#' Creates a histogram of the sequence lengths from sequences in a FASTA file
#'
#' @param fastaFile filepath of target FASTA file
#' @export
#' @author Matthew Bernstein \email{matthewb@@cs.wisc.edu}
histSeqLengths <- 
function(fastaFile) {
    lines <- readLines(fastaFile)
    seq_lengths <- getSeqLengthsInFileContent(lines)
    createHist(seq_lengths)
}


#  getSeqLengths  
#' Returns sequence lengths from sequences in a FASTA file
#'
#' @param fastaFile filepath of target FASTA file
#' @export
#' @author Matthew Bernstein \email{matthewb@@cs.wisc.edu}
getSeqLengths <-
function(fastaFile) {
    lines <- readLines(fastaFile)
    return(getSeqLengthsInFileContent(lines))
}

getSeqLengthsInFileContent <-
function(lines) {
    seq_lengths <- vector(mode="numeric", length=0)
    seq <- ""
    for (line in lines)
    {
        if(substr(line, 0, 1) == ">") {
            # If line is a new sequence, we process the current sequence
            if (!is.null(seq) && nchar(seq) != 0) {
                seq_lengths <- c(seq_lengths, nchar(seq))
                seq <- ""
            }
        }
        else {
            # Append current line of the file to the sequence 
            # under construction
            if (is.null(seq) || nchar(seq) == 0) {
                seq <- line
            }
            else {
                seq <- paste(seq, line, sep="")
            }
        }
    }
    # Process the last sequence
    seq_lengths <- c(seq_lengths, nchar(seq))
    return(seq_lengths)
}

createHist <-
function(seq_lengths) {
     hist(seq_lengths, main="Histogram of Sequence Lengths",
         xlab="Sequence Length", col="blue",
         breaks=100)
}

