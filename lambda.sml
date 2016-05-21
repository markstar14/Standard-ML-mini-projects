
datatype term = V of string | L of string * term | A of term * term

fun show (V (s)) = s
        | show (L (s, t)) = "L"^s^"."^show(t)
        | show (A (t1, t2)) = "("^show(t1)^" "^show(t2)^")";

 


fun check_variables ( x:string, y:string, [] ) = if x = y then true else  false
        | check_variables( x, y, (v1, v2)::t) = if x = v1 andalso y = v2 then true else 
		if x = v1 andalso not(y = v2) then false else
		if not(x = v1) andalso y = v2 then false else
		check_variables( x, y, t);




fun alpha2(V((s1:string)), V((s2:string)), env) = check_variables( s1, s2, env)

   | alpha2(L((s1:string), t1), L((s2:string), t2), env) = alpha2( t1, t2, (s1, s2)::env)

   | alpha2(A(t1, t2), A(t3, t4), env) = if alpha2( t1, t3, env) andalso alpha2(t2, t4, env) then true else false

   | alpha2( (t1:term), (t2:term), env ) = false;

fun alpha( t1:term, t2:term) = alpha2(t1, t2, [])



