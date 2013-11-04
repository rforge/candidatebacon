find_path <-
function(in_network, node1, node2, quiet = F) {

	node_A <- node1
	node_T <- node2
	network <- in_network$r_network

	if (!node_A %in% rownames(network) | !node_T %in% rownames(network)) { #node(s) not in network
		if (quiet == F) {
			out_text <- "  The node \""
			out_text <- paste(out_text, c(node_T, node_A)[which(!c(node_T, node_A) %in% rownames(network))], sep = "")
			out_text <- paste(out_text, "\" was not found in the network.", sep = "")
			cat(out_text)
		}
		node_count <- NA
		pathset <- NA
		search_set <- NA
	} else {

		targets <- c(node_A)
		iteration <- c(1)
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
				iteration[(length(iteration) + 1):length(targets)] <- node_count
			}
		}

		if (is.na(node_count)) { #unconnected network
			if (quiet == F) {
				out_text <- "  The nodes \""
				out_text <- paste(out_text, node_A, sep = "")
				out_text <- paste(out_text, "\" and \"", sep = "")
				out_text <- paste(out_text, node_T, sep = "")
				out_text <- paste(out_text, "\" are not connected (i.e. different subnetworks)", sep = "")
				cat(out_text)
			}
			pathset <- NA
			search_set <- NA

		} else { #find the path, only if the nodes are connected

			#getting the nodes in the path
			pathset <- matrix(, nrow = (node_count + 1), ncol = 1)

			pathset[1, 1] <- node_T
			counter <- 1


			for (i in node_count:1) {
				rowlist <- targets[which(iteration == i)][targets[which(iteration == i)] %in% (network[unique(na.omit(pathset[counter, ])), ])] #most tricky part
				pathset <- cbind(pathset, matrix(ncol = (max(dim(pathset)[2], length(rowlist)) - dim(pathset)[2]), nrow = (node_count + 1)))

				pathset[counter + 1, 1:length(rowlist)] <- rowlist[1:length(rowlist)]
				counter <- counter + 1
			}
			if (quiet == F) {
				out_text <- "  Degrees of separation between \""
				out_text <- paste(out_text, node_A, sep = "")
				out_text <- paste(out_text, "\" and \"", sep = "")
				out_text <- paste(out_text, node_T, sep = "")
				out_text <- paste(out_text, "\": ", node_count, sep = "")
				out_text <- paste(out_text, "\n  Nodes in path: \n", sep = "")
				cat(out_text)
				print(pathset)
			}

		} #only if the nodes are connected
	} #only if nodes are in network
	return(list(degree_of_separation = node_count, path_between_nodes = pathset, sub_network = search_set))
}
