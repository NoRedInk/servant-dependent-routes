# servant-dependent-routes

This library provides a new Servant combinator called `DepReqBody` that allows for dependently typed routing.  In this context, dependently typed means that the *type* of the parsed body can influence the rest of the route.