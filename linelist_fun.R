library(dplyr)

## Generate a bunch of biters at the beginning of the simulation

generate_cycle <- function(linelist, R0, mean_inc, mean_latent, mean_infectious){

	old_biters <- (linelist
		%>% filter(!is.na(num_bitee))
	)

	new_biters <- (linelist
		%>% filter(is.na(num_bitee))
		%>% rowwise() ## assuming independent incubation and R
		%>% mutate(num_bitee = rpois(1,lambda=R0)
			, incubation = rpois(1,lambda=mean_inc)
			, latent = rpois(1,lambda=mean_latent)
			, infectious = rpois(1,lambda=mean_infectious)
			, index = 1
		)
		%>% ungroup()
		%>% mutate(index = cumsum(index))
	)

	## biters to bitees

	bitee <- (new_biters[rep(new_biters$index,new_biters$num_bitee),]
		%>% rowwise()
		%>% mutate(biter_id = id
			, biter_infectious_wait = sample(x=1:infectious,size=1)
			, day = day + latent + biter_infectious_wait
			, date_bitten = day
			, num_bitee = NA
			, incubation = NA
			, latent = NA
			)
		%>% ungroup()
		%>% mutate(id = paste(biter_id,1:nrow(.),sep="_")
			, infectious = NA
			)
		%>% select(day, biter_id, id, date_bitten, num_bitee, incubation, latent, infectious, biter_infectious_wait)
	)

	clean_biters <- new_biters %>% select(-index)

	combodat <- (rbind(old_biters, clean_biters, bitee)
		%>% ungroup()
	)
	return(combodat)
}


linelist_generator <- function(ll,numGens, R0, mean_inc, mean_latent, mean_infectious){
	if(numGens < 0){
		return("numGens cannot be negative")
	}
	if(numGens == 0){
		return(ll)
	}
	if(numGens > 0){
	current_ll <- ll
	i = 1
	while(i <= numGens){
		new_ll <- generate_cycle(linelist=current_ll, R0=R0, mean_inc=mean_inc, mean_latent=mean_latent, mean_inf=mean_infectious)
		i = i + 1
		current_ll <- new_ll
	}
	}
	
	linelist <- current_ll
	ids <- unique(linelist$id)
	id_frame <- data.frame(id = ids
		, new_id = 1:length(ids)
	)

	biter_frame <- data.frame(biter_id = ids
		, new_biter_id = 1:length(ids)
	)


	linelist <- (linelist
		%>% left_join(.,id_frame)
		%>% left_join(.,biter_frame)
		%>% select(-id,-biter_id)
		%>% rename(id = new_id
			, biter_id = new_biter_id
			)
		%>% select(day, id, biter_id, everything())
	)

	return(linelist)
}
