## Engineering intuition for Type Inference in Scheme-langserver

Many languages like Java and Typescript, have functioned a type system to prevent the occurrence of execution errors during the running of a program, where variables can be given types by the [Hindley-Milner Type System](https://github.com/webyrd/hindley-milner-type-inferencer) or [System F](https://en.wikipedia.org/wiki/System_F). However, these systems are not omniscient and omnipotent. 

As for scheme, an "untyped language", for which many type annotation information won't be trivially given in static code, an intuitive case is given as following: it doesn't have types or, equivalently, have a single universal type that contains all values and the inference mechanism. 
```scheme
(lambda (a) (+ 1 a))
```

Because, the parameter `a` isn't literally annotated with any type as Typescript.
```typescript
function(a: number){return 1+a}
```

Many counterparts adapt their syntax and demand type information by literally annotating as above Typescript code. But scheme-langserver has found another possible way that consumes [r6rs](http://www.r6rs.org/)-based code and digests their supposed type specification: recalling the above scheme code, apparently, the `+` within r6rs context should have type of `(number? <- (number? ...))`, in which the `<-` indicates this is a type of function, left-hand side is the return's type, and the `(number? ...)` is a type list of parameter(s). The `a` can be easily fixed in this `+`'s parameter with type `number?`, and in this document, scheme-langserver is going to describe a type system using this implicit information from [r6rs](http://www.r6rs.org/).

This document will introduce how scheme-langserver: 
1. represents simple and compound types;
2. attaches type expressions to index;
3. walks the index and finds results;
4. publishes diagnostic information referencing [this page](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics).

### Type Representation
Let's consider the type of literals and here 4 trivial cases:
1. A simple case is like `#t`,`3`, `"string"` and `'a`, in which they all match corresponding basic type predictors `boolean?`, `number?` (also `integer?` and `fixnum?`), `string?` and `symbol?`. So that scheme-langserver will match such literals and using these predictors as their type annotation.
2. A compound case is like list literal `'(1 2 3)` and vector literal `#(1 2 3)`, in which scheme-langserver will match them with `'(number? number? number?)` and `'#(number? number? number?)`. Directly, another much more complicated example `'(1 a "e")` will be assigned with `'(number? symbol? string?)`.
3. An implicit case is given by [r6rs](http://www.r6rs.org/), in which I recognize 1108 forms of function and manually assign them type expression. Recalling the above `+` function case, `(number? <- (number? ...))`, `<-` indicate it's a function or in lisp's tongue, lambda calculus. It receives 0 parameters or something with `number?` type, and returns a `number?`-typed value, using some shortcut markers include `**1` as `+` in regular expression and `...` as `*` in this case.
4. A general case is `something?` for universal type. For example, all predictors' parameter has type of `something?`.

Finally, it's roughly knows that types have its context. This makes differences because in different library may have same-named predicator. So scheme-langserver involves [identifier-references](../analysis/identifier.md) in order to take place of such predictors in above cases. A function `construct-type-expression-with-meta` can be used for this process. And `something?` is an inner predictor won't take part in the transformation.


<!-- 3. Type expressions are scoped in [`document`](../../virtual-file-system/document.sls). This means once time $\Gamma$ machine can only digest one document and type is usually attached to [identifier-reference](../../analysis/identifier/reference.sls)'s `type-exressions`. It much benefits the code indexing. -->

### Attaching Type to Index
Apparently, the hardest core of type inference is consuming codes matching predictors, and after a bunch of cracks and crashes, splitting out codes match other predictors. In scheme-langserver, an index is across all codes, scoped within [document](../../virtual-file-system/document.sls) and denoted with [index-node](../../virtual-file-system/index-node.sls), so an intuitive idea is attaching types to index-node. 

In this section, we introduced:
1. 
3. the Hindley-Milner type system which told you something can respond LSP requests;
4. using Gradual Typing solve the parameter type annotating problem.

#### Lambda Calculus
Lambda Calculus allowing most scheme/lisp code to be normalized as the same form and digested in same way. Unlike many articles, this document won't introduce $\alpha$-conversion, $\beta$-reduction and any other such things. Here are only 2 things to be mentioned in order to know that nearly all codes (except continuation process like `call/cc`) can be digested in same way:

1. All variable assignments can be transformed to lambda calculus' application. Like `(let ((a 1)) (+ 1 a))`, can be simple transformed as `((lambda (a) (+ 1 a)) 1)`. And merely equal case in typescript is
```typescript 
function (){
    var a = 1;
    return a+1;
};
//=================>
//can be transformed to
function (){
    return (function(a){return a+1;})(1);
};
```
This means literal `1` can introduce `number?` type to variable `a` by assignment and specific known information can infer unknown.

2. All lambda calculus, its return type depend on their bottom. As above case `((lambda (a) (+ 1 a)) 1)`, the `(lambda (a) (+ 1 a))`'s return type is specified by the bottom `(+ 1 a)`. This bottom its return type is also specified by the r6rs-defined `+` function's bottom. Following this chain, as this document mentioned above, scheme-langserver has manually assigned 1108 forms with their should-be types, some more type assignment can be derived.

#### For Generality
In practice, not all scheme codes can be trivially transformed into lambda calculus even without `call/cc`, because r6rs standard just says what functions a scheme implementation should have, but doesn't says how to implement. This means a lot of annotation to do in order to tell our type system an expression's type is determined by several parts of code. Actually, scheme-langserver has implemented several rules in [this folder](../../analysis/type/rules/). They do such things:
1. Annotate literals with their types and construct a knowledge list like `(quasiquote (,index-node : ,type))`
   >Conveniently, for unification machine to be introduced in next section, the index-node would be firstly denoted with a variable and then have `(quasiquote (,variable : ,type))`.
2. Annotate expressions with types of their bottoms.
3. Annotate variables index-node, like `a` above, with their assignments.
4. Annotate flows with specific rules. For example, `if` obviously may have two types of returns, and a list of two knowledge would be constructed as `(quasiquote (,index-node : ,type1) (,index-node : ,type2))`.

### Walk and Find
#### Hindley-Milner Type System
Although lambda calculus indicates how information flows from literal and r6rs-base functions to user-selves defined code, it's like a crack and crash engine, in which fuel has been pumped, nothing know we how to ignite it. Luckily, we have Hindley-Milner (HM) type system, a classical type system for the lambda calculus. Among its more notable properties are its completeness and its ability to infer the most general types of a given program without programmer-supplied type annotations or other hints. HM has following detailed rules:

1. Variable Access 
$$ \frac{(x:\sigma) \in \Gamma}{\Gamma \vdash (x:\sigma)} $$ 
Intuitively, this whole rule means if a variable $x$ were given a type above this horizental line, a following-up process under it would be able to apply: spit out the type $\sigma$ of $x$. For detail, $\Gamma$ is a machine that can act all these deductions, $\in$ means extending the machine with assigning type $\sigma$ to variable $x$. $\vdash$ indicats the $\Gamma$ spliting something. 

2. Application
$$\frac{\Gamma \vdash (e_1:(\tau_1 \to \tau_2)) \quad\quad \Gamma \vdash (e_2 : \tau_1) }{\Gamma \vdash ((e_1\ e_2) : \tau_2)}$$
This rule is usually used for applying function `e1` to `e2` like `(e1 e2)` (like above `(+ 1 a)`) and get this expression's type $\tau_2$. You can find its implementation in [application.sls](../../analysis/type/rules/application.sls). Attention, here the $\to$ is opposite to above `<-` direction.

3. Abstract
$$\frac{(\Gamma, \;(x:\tau_1)) \vdash (e:\tau_2)}{\Gamma \vdash ((\lambda\ x\ .\ e) : (\tau_1 \to \tau_2))} $$
This rule is usually used for defining function's type like `(lambda (x) e)`. It means, if a function $e$, when its parameter $x$ had been given type $\tau_1$, its return type were $\tau_2$, then $e$'s whole type would be $\tau_1 \to \tau_2$. In scheme-langserver, this type expression may be `(tau1 <- tau2)`.

4. Let
$$\frac{\Gamma \vdash (e_1:\sigma) \quad\quad (\Gamma,\,(x:\sigma)) \vdash (e_2:\tau)}{\Gamma \vdash ((\mathtt{let}\ (x = e_1)\ \mathtt{in}\ e_2) : \tau)} $$
For variable $x$ has type $\sigma$ and function $e_2$ has $\tau$, if substituted $x$ with $e_1$ in $e_2$, the return type still would be $\tau$.

5. Subtype
$$\frac{\Gamma \vdash (e: \sigma_1) \quad\quad \sigma_1 \sqsubseteq \sigma_2}{\Gamma \vdash (e : \sigma_2)}$$
This rule is saying that if $e$ has type $\sigma_1$ and $\sigma_1$ is subset of $\sigma_2$, or in other words, $\sigma_1$ inherents $\sigma_2$, $e$ will also has type $\sigma_2$.

6. Type Generalize
$$\frac{\Gamma \vdash (e: \sigma) \quad\quad \alpha \notin \mathtt{free}(\Gamma)}{\Gamma \vdash (e:(\forall\ \alpha\ .\ \sigma))}$$
This rule is usually used for type's generation especially for record type inherent in scheme language. It is saying that if there is some variable $\alpha$ which is "not free", then it is safe to say that any expression whose type you know $e : \sigma$ will have that type for any value of $\alpha$. Further, "not free" means $alpha$ has been attached to some variables in above pair list.

#### Implementation Note for $\Gamma$ Machine
1. $:$ means has type, $\sqsubseteq$ means subset to, and $\Gamma$ machine must have a graph with relation between variables and their types, and type themselves. As for the case that graph is sparse and single variable or type won't have many relations, a list like `((variable : type)... (type1 < type2))` can be facilitated in $\Gamma$;
2. $\in$ means extend $\Gamma$ machine's knowledge. And in scheme-langserver, this process is like `(append old-knowledge-list with-new-knowledge-list)`;
3. $\vdash$ indicates a finding process that $\Gamma$ machine can infer something interesting. And a sequential walking and checking process may be usable for the pair list.
4. The horizontal line means unification above and below constructs the two sides of an **equation**. For example, variable access rule is `(variable1 = variable2)` and abstract rule is `(variable3 = (variable4 <- (variable5)))`. And the latter one indicates `variable3` can be substituted with `(variable4 <- (variable5))`.
<!-- 5. R6RS hasn't published their functions' official code implementation, so, many  -->
#### Unification Machine
Directly, $\Gamma$ is a machine who will run a routing process by finding a path from static code, usually represented with [index-node](../../virtual-file-system/index-node.sls) to type expression. This algorithmic process of solving equations between symbolic expressions is named with unification in logic and computer science. And according to the mathematic tradition created by François Viète, by importing [variables](../../analysis/type/variable.sls), the unification algorithm also denote expressions and their parts. 

In scheme-langserver, the [unification algorithm](../../analysis/type/type-inferencer.sls) is in [Robinson's Unification Algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm)'s tongue, and acts $\Gamma$ machine like a spider walking on knowledge graph. After [identifier catching](./identifier.md), construct graph for single document with `construct-substitution-list-for` function and the process path starts from index-node and finally ends with some type-known index-nodes, which are practically specified by r6rs functions or literals. Path-memory and cycle detection mechanism would avoid dead loop. Details are following:
1. Maintain a substitution list as the equation set;
2. As for the type of specific index-node, finding corresponding substitutions by matching the left side of equations;
3. Substitute the left side with right side, and conserve all the transformation;
4. Do 2-3 several loops until there is no new substitutions and output the result;

#### Gradual Typing and Implicit Conversion
Gradual typing wants to make an [implicit conversation](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/): recalling above example `(lambda (a) (+ 1 a))`, the `+` only accept `number?` input, why can't assign the `a` with `number?` type? As the author Jeremy Siek says, this will deconstruct the building of subtype rule. But, as I can see, this is the valuable compromise.

### TODO: Diagnostic based on Type System