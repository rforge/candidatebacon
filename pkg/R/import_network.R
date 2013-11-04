import_network <-
function(infile, columns = c(1, 2)) {
	filename <- infile

	in_network <- as.matrix(read.delim(filename, header = F, na.strings = "", stringsAsFactors = F, sep = "\t", fill = F))

	#the columns of the interaction pairs
	sub_col_a <- columns[1] #the columns in the file to use 
	sub_col_b <- columns[2]

	in_network <- in_network[!duplicated(in_network[, c(sub_col_a, sub_col_b)]), c(sub_col_a, sub_col_b)]
	in_network <- rbind(in_network, in_network[, c(2, 1)])

	first_col <- 1
	second_col <- 2

	node_array <- unique(array(in_network[, first_col]))
	complete_number_of_nodes <- length(node_array)

	max_single_node_interactions <- max(unlist(lapply(split(in_network[, first_col], f = in_network[, first_col]), length))) #this splits the first column and count the number of each occurence of each element and get the max number of interactions that a single node have.

	#a data frame that will be transposed
	network <- matrix(data = NA, nrow = complete_number_of_nodes, ncol = max_single_node_interactions)
	rownames(network) <- node_array

	node_stat <- array(data = NA, dim = complete_number_of_nodes)

	for (i in 1:complete_number_of_nodes) {
		network[i, 1:length(in_network[which(in_network[, first_col] == node_array[i]), second_col])] <- in_network[which(in_network[, first_col] == node_array[i]), 
			second_col]
		node_stat[i] <- length(which(!is.na(unique(network[i, ]))))
	}
	cat(" NETWORK STATS")
	cat(paste("\n  Network imported: ", filename, sep = ""))
	cat(paste("\n  Number of nodes: ", length(network[, 1]), sep = ""))
	cat(paste("\n  Max interactions of a node: ", length(network[1, ]), sep = ""))
	cat(paste("\n  Min interactions of a node: ", min(node_stat), sep = ""))
	cat(paste("\n  Mean interactions per node: ", mean(node_stat), sep = ""))

	return(list(r_network = network, interactions_per_node = node_stat))
}
