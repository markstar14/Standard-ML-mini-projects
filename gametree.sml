datatype 'a gametree = node of 'a * 'a gametree list

fun prune (0, node(n, glist)) = node(n, [])
	| prune (n, node(p, h::glist)) = prune ((n-1), h); 
type 'a gametree = node of 'a * 'a gametree list

fun prune (0, node(n, glist)) = node(n, [])
        | prune (n, node(p, h::glist)) = prune ((n-1), h);


fun treemap (s, node(p, glist)) = node(s(p), tmap(s, glist))


and tmap(s, [])= []
        | tmap(s, h::glist) = treemap(s, h)::tmap(s, glist);





fun treemap (s, node(p, glist)) = node(s(p), tmap(s, glist))


and tmap(s, [])= []
	| tmap(s, h::glist) = treemap(s, h)::tmap(s, glist);





