draw_network <-
function(in_network, pathway, outfile = "pathway.gv", level = "zero") {
	if (!grepl(".gv", outfile)) {
		gv_name <- paste(outfile, ".gv", sep = "")
	} else {
		gv_name <- outfile
	}
	w_text <- ("digraph {")
	write(w_text, file = gv_name, append = F, )

	quoter <- "\""
	extra_row_text <- "[dir=none,weight=2];"

	in_network <- in_network$r_network
	node_set <- pathway$path_between_nodes

	counter <- 1
	path_vec <- unique(na.omit(as.vector(node_set)))
	w_tab <- matrix(, nrow = (length(na.omit(as.vector(in_network[path_vec, ])))), ncol = 3) #is this too big?
	if (level == "two") {
		path_vec <- 1# target_vec
	}


	for (i in 1:length(path_vec)) {
		for (j in 1:length(unique(na.omit(in_network[path_vec[i], ])))) {
			if (in_network[path_vec[i], j] %in% node_set | level == "one") {
				if (!paste(path_vec[i], in_network[path_vec[i], j], sep = "") %in% w_tab[, 3] & !paste(in_network[path_vec[i], j], path_vec[i], sep = "") %in% 
					w_tab[, 3] & in_network[path_vec[i], j] != path_vec[i]) {
					w_tab[counter, 1] <- path_vec[i]
					w_tab[counter, 2] <- in_network[path_vec[i], j]
					w_tab[counter, 3] <- paste(in_network[path_vec[i], j], path_vec[i], sep = "")
					counter <- counter + 1
				}
			}
		}
	}

	w_tab <- w_tab[1:(length(na.omit(w_tab[, 1]))), 1:2]
	write.table(w_tab, file = gv_name, quote = T, append = T, row.names = F, col.names = F, sep = " -> ", eol = paste(extra_row_text, "\r\n", sep = ""))

	#adding the colours
	w_text <- quoter
	w_text <- paste(w_text, unique(na.omit(node_set[1, ])), sep = "") #first node
	w_text <- paste(w_text, quoter, sep = "")
	w_text <- paste(w_text, " [style=filled,color=black,fillcolor= cornflowerblue];", sep = "")
	write(w_text, file = gv_name, append = T, )

	w_text <- quoter
	w_text <- paste(w_text, unique(na.omit(node_set[length(node_set[, 1]), ])), sep = "") #last node
	w_text <- paste(w_text, quoter, sep = "")
	w_text <- paste(w_text, " [style=filled,color=black,fillcolor= firebrick];", sep = "")
	write(w_text, file = gv_name, append = T, )

	if (length(node_set[, 1]) > 2) {
		mid_col <- unique(na.omit(as.vector(node_set[2:(length(node_set[, 1]) - 1), ])))
		for (i in 1:length(mid_col)) { #mid colors
			w_text <- quoter
			w_text <- paste(w_text, mid_col[i], sep = "")
			w_text <- paste(w_text, quoter, sep = "")
			w_text <- paste(w_text, " [style=filled,color=black,fillcolor= grey60];", sep = "")
			write(w_text, file = gv_name, append = T, )
		}
	}
	w_text <- ("}")
	write(w_text, file = gv_name, append = T, )
}
