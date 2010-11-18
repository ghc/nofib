<nickel>  ::= ["n" .x. "c"^5];
<dime>    ::= ["d" .x. "c"^10];
<quarter> ::= ["q" .x. "c"^25];
<cent>    ::= ["c" .x. "c"];
<money>   ::= [<nickel> | <dime> | <quarter> | <cent>]*;
<drink>   ::= ["c"^10 .x. "PLONK"];
<main>    ::= [<money> .o. <drink>];
