library("SeqLengthHist")

test_that("getSeqLengthsInFileContent function correctly retrieves sequence lenghts", {
  fasta_content <- c(">seq1", "AAAAA", "CCCCC", ">seq2", "", "AAA")
  expect_equal(getSeqLengthsInFileContent(fasta_content), c(10, 3))
})
