/* Examples for testing */

true;
if false then true else false; 

x/;
x;

x = true;
x;
if x then false else x; 

lambda x. x;
(lambda x. x) (lambda x. x x); 

{x=lambda x.x, y=(lambda x.x)(lambda x.x)}; 
{x=lambda x.x, y=(lambda x.x)(lambda x.x)}.x;

"hello";

timesfloat (timesfloat 2.0 3.0) (timesfloat 4.0 5.0);

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

let x=true in x;

mult2 = lambda y . timesfloat 2.0 y;

fix = lambda f . (lambda x . f ( lambda y . x x y )) (lambda x . f (lambda y . x x y));


sumaux = lambda f . ( lambda n . ( lambda m . if (iszero n) then m else succ ( f ( pred n ) m )));
sum = fix sumaux;


leqaux = lambda f . ( lambda m . ( lambda n . ( if iszero m then true else ( if iszero n then false else f (pred m) (pred n)))));
leq = fix leqaux;


timesaux = lambda f . ( lambda m . ( lambda n . (if (iszero m) then 0 else sum n (f (pred m) n))));
times = fix timesaux;


factaux = lambda f . ( lambda n . ( if (iszero n) then 1 else times n ( f (pred n))));
fact = fix factaux;

fact 3;
