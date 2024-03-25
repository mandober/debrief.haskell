# Closed type families

* Closed Type Family (CTF) consists of the header (head) and a set of clauses introduced after the `where` keyword (which Open TF do not have).

* In a CTF, If none of the clauses match, the type family just gets *stuck*, not reducing any further.
