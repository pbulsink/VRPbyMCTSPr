vrp<-function(tour, base, distance_matrix, n_vehicles, ...){
    #where tour is a list of integers, base is the starting point.
    tot_distances<-list(rep(0, n_vehicles))

    stops<-tour[tour!= base]
    sub_tours<-split(stops, cut(seq_along(stops), n_vehicles, labels=FALSE))

    for (i in 1:n_vehicles){
        tour<-c(base, unlist(sub_tours[[i]]), base)
        tour<-c(tour, base)
        route_length<-0
        for(j in 1:length(tour)){
            route_length<-route_length+distance_matrix[tour[i], tour[i+1]]
        }
        tot_distances[i]<-route_length
    }
    return(tot_distances)
}

vrp.fitness <- function(tour, ...){
    d<-vrp(tour, ...)
    fit<-sum(unlist(d)) + range(d)[2]-range(d)[1]
    return(1/fit)
}

GA.tours<-ga(type="permutation", fitness=vrp.fitness, base=4, distance_matrix=distance_matrix, n_vehicles=2,
             min=1, max=nrow(distance_matrix), popSize = 10, maxiter=1000, run = 100, pmutation=0.2, monitor = TRUE)
