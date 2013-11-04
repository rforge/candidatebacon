degree_of_separation <-
function(in_network, repeats = 100) {

	re_samping <- repeats
	bacon_numbers <- array(data = FALSE, dim = re_samping)
	network <- in_network$r_network
	complete_number_of_nodes <- dim(network)[1]

	for (counter in 1:re_samping) {
		node_sample <- sample(complete_number_of_nodes, size = 2, replace = F) #sampling 
		node_A <- rownames(network)[node_sample[1]]
		node_T <- rownames(network)[node_sample[2]]

		targets <- c(node_A)
		t_length <- 0
		search_set <- as.matrix(network[targets, ])

		node_count <- 1
		found <- 0

		while (found == 0) {
			if ((node_T %in% search_set) | (t_length == length(targets))) {
				found <- 1
				if (t_length == length(targets)) {
					node_count <- NA
				}
			} else {
				t_length <- length(targets)
				targets <- unique(c(targets, unique(array(search_set))))
				targets <- targets[!is.na(targets)]
				search_set <- network[targets, ]
				node_count <- node_count + 1
				#print(node_count)	
				}
		}
		bacon_numbers[counter] <- node_count
	}
	out_text <- "  Mean degree of separation: "
	out_text <- paste(out_text, mean(bacon_numbers, na.rm = T), sep = "")
	out_text <- paste(out_text, "\n  Number of unconnected pairs: ", sep = "")
	out_text <- paste(out_text, sum(is.na(bacon_numbers)), sep = "")
	cat(out_text)
	return(bacon_numbers)
}
