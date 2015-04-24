#  SeqLengthHist  
#' Creates a histogram of the sequence lengths from sequences in a FASTA file
#'
#' @param fastaFile filepath of target FASTA file
#' @author Matthew Bernstein \email{matthewb@@c.wisc.edu}
histSeqLengths <- 
function(fastaFile) {
    seq_lengths <- vector(mode="numeric", length=0)
    lines <- readLines(fastaFile)
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

    hist(seq_lengths, main="Histogram of Sequence Lengths", 
         xlab="Sequence Length", col="blue",
         breaks=100)
}

