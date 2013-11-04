multi_gene_paths <-
function(in_network, interaction_list) {
	used_network <- in_network
	i_matrix <- interaction_list
	test_num <- length(i_matrix[, 1])
	out_vector <- array(data = NA, dim = test_num)
	for (i in 1:test_num) {
		i_path <- find_path(used_network, i_matrix[i, 1], i_matrix[i, 2], quiet = T)
		out_vector[i] <- i_path$degree_of_separation
	}
	out_text <- "  Mean degree of separation: "
	out_text <- paste(out_text, mean(out_vector, na.rm = T), sep = "")
	out_text <- paste(out_text, "\n  Number of unconnected pairs: ", sep = "")
	out_text <- paste(out_text, sum(is.na(out_vector)), sep = "")
	cat(out_text)
	return(out_vector)
}
