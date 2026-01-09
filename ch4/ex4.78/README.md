# To run the query system using the amb evaluator
`scheme --load ch4-ambeval.scm`

This will initialize the amb evaluator will interpret the ch4-query.scm query system
in the non-deterministic language.

# Differences

The order of output is different for disjoins, my current
implementation will display all database matches for the
first clause, before moving onto the subsequent clauses.

Negate works differently with the usage of amb. I implemented
qeval to work with amb, so that when no database values exist
for a pattern, the empty amb expression is evaluated (amb).
This causes the process to backtrack outside of negate, and
into the initial failure handler to print there are no more
options.
To fix this, we would have to detect when a qeval fails, which
would allow our negate to be true, and allow the frame to pass
and instantiate the original query or to continue to the next
query to evaluate if there are more.
However, there is not a builtin way to detect a failure. I could
wrap the qeval usage in negate in another amb, and have null
as the second choice for when qeval fails. However, we still
need a empty (amb) usage for when there are results, telling
the system to ignore this frame and to get the next one. This
would cause a problem because it would backtrack to our new
wrapping amb and return null, allowing the qeval with results
to pass the negate which ruins the purpose of negate.
