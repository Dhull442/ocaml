## Asssignment 1

<b>Saksham Dhull : 2017CS10370</b></br>
- <b>Theorem 1</b>: For any `t` belonging to `exptree`, `eval (t) = toInt (stackmc [] (compile t) )` </br> where `toInt` is the inverse function of `mk_big` and `eval`, `stackmc`  & `compile` functions are as specified in `a1.ml`</br>

Let's define a function `size : exptree -> int`</br>
```
let size t = match t with
 N a -> 1
 | fun ( a, b ) -> 1 + (size a) + (size b)
 | fun (a) -> 1 + (size a);;
` vnbh ``
</br>
<b>Proof by Induction</b>
- <b>Base Case: </b> let `size t` = 1 </br> => `t = N a` for some integer a.</br>
Now, </br>
  `eval (N a) = a` </br>
  `compile ( N a ) = [CONST mk_big a]`</br>
  where `mk_big: int -> BigInt` is defined in `a0.ml` </br>
  Now,`stackmc [] [CONST mk_big a] = mk_big a` and `toInt (mk_big a) = a`.

- <b>Induction Hypothesis: </b> Let the above theorem satisfies for all `t` such that `size t =< k` </br>
