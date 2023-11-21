## Type Inference Implemented with DSL
>NOTE:This documentation is temporary and may have many errors. It will be continuously updated. A Chinese book on type inference with DSL is being written. 

Many languages like Java and Typescript, have functioned a type system to prevent the occurrence of execution errors during the running of a program. These systems are usually [Hindley-Milner Type System](https://github.com/webyrd/hindley-milner-type-inferencer) or [System F](https://en.wikipedia.org/wiki/System_F). However, these systems are not omniscient and omnipotent. 

As for scheme, an "untyped language", for which many type annotation information won't be trivially given in static code, an intuitive case is given as following: 
```scheme
(lambda (a) (+ 1 a))
```
It doesn't have types or, equivalently, have a single universal type that contains all values and the inference mechanism. Because the parameter `a` isn't literally annotated with any type as Typescript.
```typescript
function(a: number){return 1+a}
```

Many counterparts like Type-Racket adapt their syntax and demand type information by literally annotating as above Typescript code. But scheme-langserver has found another possible way that consumes [r6rs](http://www.r6rs.org/)-based code and digests their supposed type specification: recalling the above scheme code, apparently, the `+` within r6rs context should have type of `(number? <- (number? ...))`, in which the `<-` indicates this is a type of function, left-hand side is the return's type, and the `(number? ...)` is a type list of parameter(s). **Suppose the code always is executable**, the `a` can be easily fixed in this `+`'s parameter with type `number?`, and in this document, scheme-langserver is going to describe a type system using this implicit information from [r6rs](http://www.r6rs.org/).

This document will introduce how scheme-langserver: 
1. represents simple and compound types;
2. attaches type expressions to index;
3. walks the index and finds results;
4. publishes diagnostic information referencing [this page](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics).

### Type Representation
Let's consider the type of literals and here 4 trivial cases:
1. A simple case is like `#t`,`3`, `"string"` and `'a`, in which they all match corresponding basic type predictors `boolean?`, `number?` (also `integer?` and `fixnum?`), `string?` and `symbol?`. So that scheme-langserver will match such literals and using these predictors as their type annotation.
2. A compound case is like list literal `'(1 2 3)` and vector literal `#(1 2 3)`, in which scheme-langserver will match them with `'(inner:list? number? number? number?)` and `'(inner:vector? number? number? number?)`. Directly, another much more complicated example `'(1 a "e")` will be assigned with `'(inner:list? number? symbol? string?)`. The `inner:list?`, `inner:vector?` and `inner:pair?` are primitives.
3. An implicit case is given by [r6rs](http://www.r6rs.org/), in which I recognize 1108 forms of function and manually assign them type expression. Recalling the above `+` function case, `(number? <- (inner:list? number? ...))`, `<-` indicate it's a function or in lisp's tongue, lambda calculus. It receives 0 parameters or something with `number?` type, and returns a `number?`-typed value, using some shortcut markers include `**1` as `+` in regular expression, that previous pattern occurs more than once time, and `...` as `*` for zero or more times.
4. A general case is `something?` for universal type. For example, all predictors' parameter has type of `something?`.
5. `void?` for `(void)`.

Further, it's roughly knows that types have its context. This makes differences because in different library may have same-named predicator. So scheme-langserver involves [identifier-references](../analysis/identifier.md) in order to take place of such predictors in above cases. A function `construct-type-expression-with-meta` can be used for this process. And `something?`, `void?`, `**1`, `...` and many other primitives won't take part in the transformation.


<!-- 3. Type expressions are scoped in [`document`](../../virtual-file-system/document.sls). This means once time $\Gamma$ machine can only digest one document and type is usually attached to [identifier-reference](../../analysis/identifier/reference.sls)'s `type-exressions`. It much benefits the code indexing. -->

### Attaching Type to Index
#### Lambda Calculus
In scheme-langserver, an index is across all codes, scoped within [document](../../virtual-file-system/document.sls) and denoted with [index-node](../../virtual-file-system/index-node.sls), so an intuitive idea is attaching types to index-node. Apparently, the hardest core of type inference is consuming codes matching predictor expressions, and after a bunch of cracks and crashes, splitting out codes match other simple or complex predictor expressions. 

Previous paper work would hack this problem with lambda calculus' tongue, $\alpha$-conversion, $\beta$-reduction and any other such things. But this section just mentions 2 examples, to show that nearly all codes can be digested in same way:
1. All variable assignments can be transformed to lambda calculus' application. Like `(let ((a 2)) (+ 1 a))`, can be simple transformed as `((lambda (a) (+ 1 a)) 2)`. And merely equal case in typescript is
```typescript 
function (){
    var a = 2;
    return a+1;
};
//=================>
//can be transformed to
function (){
    return (function(a){return a+1;})(2);
};
```
This means literal `2` can introduce `number?` type to variable `a` by assignment and specific known information can infer unknown.

2. All lambda calculus, its return type depend on their bottom. As above case `((lambda (a) (+ 1 a)) 1)`, the `(lambda (a) (+ 1 a))`'s return type is specified by the bottom `(+ 1 a)`. This bottom its return type is also specified by the r6rs-defined `+` function's bottom. Following this chain, as this document mentioned above, scheme-langserver has manually assigned 1108 forms with their should-be types, some more type assignment can be derived.

Further, there's still a long way to all types, because:
1. our annotations on r6rs standard just are not completed;
2. self-defined functions, recursions, variables and flow controls, they won't tell our type system their type annotation by themselves. For above example, `(let ((a 2)) (+ 1 a))`, its type is determined by several parts of code and scheme-langserver has implemented several rules in [this folder](../../analysis/type/substitutions/rules/);
3. some high-level flow control technique like `call/cc`, their types are not able predictable theoretically.

#### Extension of Type Representation
Above we've mentioned "compound", "complex", "several parts" and such on many times. They all lead 3 following tips:
1. Some lambda calculus and their origin codes have simple or compound types;
2. Some types depend on others, such as simple depends on compound and reverse;
3. For these codes don't have types, when we're gathering type information, we need a notation for unknown.

This is to say scheme-langserver has to introduce [variable](../../analysis/type/domain-specific-language/variable.sls) into type representation and take the part of [identifier-reference](../../analysis/identifier/reference.sls). And this would be convenient for successive inference and logic calculation.

### Walk and Find: Prerequisite Knowledge 
#### Hindley-Milner Type System
Although lambda calculus indicates how information flows from literal and r6rs-base functions to user-selves defined code, it's like a crack and crash engine, in which fuel has been pumped, nothing know we how to ignite it. Luckily, we have Hindley-Milner (HM) type system, a classical type system for the lambda calculus. Among its more notable properties are its completeness and its ability to infer the most general types of a given program without programmer-supplied type annotations or other hints. HM has following detailed rules:

1. Variable Access 
$$\frac{(x:\sigma) \in \Gamma}{\Gamma \vdash (x:\sigma)}$$ 
Intuitively, this whole rule means if a variable $x$ were given a type above this horizental line, a following-up process under it would be able to apply: spit out the type $\sigma$ of $x$. For detail, $\Gamma$ is a machine that can act all these deductions, $\in$ means extending the machine with assigning type $\sigma$ to variable $x$. $\vdash$ indicats the $\Gamma$ spliting something. 

2. Application
$$\frac{\Gamma \vdash (e_1:(\tau_1 \to \tau_2)) \quad\quad \Gamma \vdash (e_2 : \tau_1) }{\Gamma \vdash ((e_1\ e_2) : \tau_2)}$$
This rule is usually used for applying function `e1` to `e2` like `(e1 e2)` (like above `(+ 1 a)`) and get this expression's type $\tau_2$. You can find its implementation in [application.sls](../../analysis/type/rules/application.sls). Attention, here the $\to$ is opposite to above `<-` direction.

3. Abstract
$$\frac{(\Gamma, \;(x:\tau_1)) \vdash (e:\tau_2)}{\Gamma \vdash ((\lambda\ x\ .\ e) : (\tau_1 \to \tau_2))} $$
This rule is usually used for defining function's type like `(lambda (x) (+ 1 x))`. It means, if a function $e$, when its parameter $x$ had been given type $\tau_1$, its return type were $\tau_2$, then $e$'s whole type would be $\tau_1 \to \tau_2$. In scheme-langserver, this type expression may be `(tau1 <- tau2)`.

4. Let
$$\frac{\Gamma \vdash (e_1:\sigma) \quad\quad (\Gamma,\,(x:\sigma)) \vdash (e_2:\tau)}{\Gamma \vdash ((\mathtt{let}\ (x = e_1)\ \mathtt{in}\ e_2) : \tau)} $$
Known: the expression $e_1$ has type $sigma$; for variable $x$ has type $\sigma$, function $e_2$ has $\tau$. If substituted $x$ with $e_1$ in $e_2$, the return type still would be $\tau$.

5. Subtype
$$\frac{\Gamma \vdash (e: \sigma_1) \quad\quad \sigma_1 \sqsubseteq \sigma_2}{\Gamma \vdash (e : \sigma_2)}$$
This rule is saying that if $e$ has type $\sigma_1$ and $\sigma_1$ is subset of $\sigma_2$, or in other words, $\sigma_1$ inherents $\sigma_2$, $e$ will also has type $\sigma_2$.

6. Type Generalize
$$\frac{\Gamma \vdash (e: \sigma) \quad\quad \alpha \notin \mathtt{free}(\Gamma)}{\Gamma \vdash (e:(\forall\ \alpha\ .\ \sigma))}$$
This rule is usually used for type's generation especially for record type inherent in scheme language. It is saying that if there is some variable $\alpha$ which is "not free", then it is safe to say that any expression whose type you know $e : \sigma$ will have that type for any value of $\alpha$. Further, "not free" means $alpha$ has been attached to some variables in above pair list.

#### Implementation Note for $\Gamma$ Machine
1. $:$ means has type, $\sqsubseteq$ means subset to, and $\Gamma$ machine must have a graph with relation between variables and their types, and type themselves. As for the case that graph is sparse and single variable or type won't have many relations, a list like `((variable : type)... (type1 < type2))` can be facilitated in $\Gamma$;
2. $\in$ means extend $\Gamma$ machine's knowledge. And in scheme-langserver, this process is like `(append old-knowledge-list with-new-knowledge-list)`;
3. The horizontal line means unification above and below constructs the two sides of an **equation**. These equations are represented as`(vairbale = type-expression-with-variable)`, in which `variable` is referring [variable.sls](../../analysis/type/variable.sls); With these equations, scheme-langserver can do following processes.
4. $\vdash$ indicates a finding process that $\Gamma$ machine can infer something interesting. And a sequential walking and checking process may be usable for the pair list.
<!-- 5. R6RS hasn't published their functions' official code implementation, so, many  -->
#### Unification Machine
Directly, $\Gamma$ is a machine who will run a routing process by finding a path from static code, usually represented with [index-node](../../virtual-file-system/index-node.sls) to type expression. This algorithmic process of solving equations between symbolic expressions is named with unification in logic and computer science. And according to the mathematic tradition created by François Viète, by importing [variables](../../analysis/type/variable.sls), the unification algorithm also denote expressions and their parts. 

In scheme-langserver, the [unification algorithm](../../analysis/type/type-inferencer.sls) is in [Robinson's Unification Algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm)'s tongue, and acts $\Gamma$ machine like a spider walking on knowledge graph. After [identifier catching](./identifier.md), construct graph for single document with `construct-substitution-list-for` function and the process path starts from index-node and finally ends with some type-known index-nodes, which are practically specified by r6rs functions or literals. Path-memory and cycle detection mechanism would avoid dead loop. Details are following:
1. Maintain a substitution list as the equation set;
2. As for the type of specific index-node, finding corresponding substitutions by matching the left side of equations;
3. Substitute the left side with right side, and conserve all the transformation;
4. In the [following section](#walk-and-find-interpreter), this mechanism is also used to remember immediate results, and a `PRIVATE-MAX-DEPTH` is used to limit the depth.


### Walk and Find: Interpreter
>NOTE: The detail code is in [this file](../../analysis/type/domain-specific-language/interpreter.sls). 

The interpreter involves 3 scopes of symbols:
1. Type representations. A procedure [`inner:trivial?`](../../analysis/type/domain-specific-language/inner-type-checker.sls) will verify it. After verifying, the interpreter is mainly responsible for the "Application Rule" in [above section](#hindley-milner-type-system). That means `((boolean? <- (inner:list? something?)) something?)` will be interpreted into `boolean?`.
2. Substitutions. All variables will be deducted with such substitutions and therefore further interpretion will be continued on this base.
3. Macros. For many higher order functions like `car` and `cdr`. Above representations can't undertake the pressure. I just made a very simple macro expander so that users can write macro and make it writing type representations in interpreting process, and here's an example.

#### Example: Gradual Typing and Implicit Conversion
Recalling the example of `(lambda (a) (+ 1 a))`, the parameter `a` is not given type within Hindley-Milner's system. Scheme-langserver solves this problem with [Gradual Typing](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/), it wants to make an [implicit conversation](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/): the `+` only accept `number?` input, why can't assign the `a` with `number?` type? 

The details are listed in the [trivial.sls](../../analysis/type/substitutions/rules/trivial.sls): When the `a` symbol is legally presented in the `+` form, a substitution will be generated like `(variable0 = (with (a b c) ((with (d0 d1 d2) d1) c)))` with `variable0` is attached to `a` and `variable1` is attached to `+`. `d0 d1 d2` is generated according to how many parameters the `+` form has. Whatever, after some interpreting processes, the `a` will be attached with `number?` in `+`'s `(number? <- (inner:list? number? ...))`.

<!-- >NOTE: As the author Jeremy Siek says, this will deconstruct the building of subtype rule, because we **suppose the code always is executable**. But, as I can see, this will be the valuable compromise, if scheme-langserver can give usable information. -->

#### Function Overloading and Recursion
The function overloading and recursion raise the [Halting Problem](https://en.wikipedia.org/wiki/Halting_problem) a practical barrier in my DSL interpreter. To solve this problem, the interpreter has to recall their function name many times and the [loop-avoiding mechanism](#unification-machine) won't allow this. I've just implemented a set of procedure to solve this problem:
1. `type:interpret` is the basic version, and it involves a `max-depth` in order to avoid loop. And here, a much more complicated case is that `((boolean? <- (inner:list? something?)) something?)`, the `boolean?` part usually is taken place with a procedure type representation and like `(((boolean? <- (inner:list? something?))  <- (inner:list? something?)) something?)`. If there were some variables in that part, the interpreter couldn't recognize the newly generated representation is equal to origin one.
2. `type:interpret-result-list` just for taking the result from type:interpret.
3. There may be some errors in macro expanding, and `type:interpret` will fall into an exception and maybe return a list contain macros. Obviously, a macro should be interpreted before interpreting whole representation. This is why `type:depature&interpret->result-list` is implemented.
4. Finally, `type:recursive-interpret-result-list` is for solving the halting problem: it will extend the substitution set and interpret those unsolved representations with these substitutions. With limited extending times, this procedure can solve many representations like [this test](../../tests/analysis/type/substitutions/rules/test-lambda.sps).

#### Weaknesses Now 
The interpreter now heavily depended on Cartesian Product, which is mainly used in generating type representations for procedures. Especially for those procedures have more than 4 parameters which may separately have 4 or more representations, and the results for this parameter list will account for more than 256. In other words, the raising speed of status number $n$ account for $\mathcal{O}(n!)$! 

This is because that the Cartesian Product is a travesal operation. Maybe some features in my DSL could cut edges in this process, but now I don't figure out any solution.

### TODO: Diagnostic based on Type System