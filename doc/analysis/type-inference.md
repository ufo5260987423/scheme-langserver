## Engineering intuition for Type Inference in scheme-langserver

Many languages like Java and Typescript, have functioned a type system to prevent the occurrence of execution errors during the running of a program, where variables can be given types by the [Hindley-Milner Type System](https://github.com/webyrd/hindley-milner-type-inferencer) or [System F](https://en.wikipedia.org/wiki/System_F). However, these systems are not omniscient and omnipotent. 

As for scheme, an "untyped language", for which many type annotation information won't be trivially given in static code, an intuitive case is given as following: it doesn't have types or, equivalently, have a single universal type that contains all values and the inference mechanism. 
```scheme
(lambda (a) (+ 1 a))
```

Because, the parameter `a` isn't literally annotated with any type as Typescript.
```typescript
function(a: number){return 1+a}
```

Many counterparts adapt their syntax and demand type information by literally annotating as above Typescript code. But scheme-langserver has found another possible way that consumes [r6rs](http://www.r6rs.org/)-based code and digests their supposed type specification: recalling the above scheme code, apparently, the `+` within r6rs context should have type of `(number? <- (number? ...))`, in which the `<-` indicates this is a type of procedure, left-hand side is the return's type, and the `(number? ...)` is a type list of parameter(s). The `a` can be easily fixed in this `+`'s parameter with type `number?`, and in this document, scheme-langserver is going to describe a type system using this implicit information from [r6rs](http://www.r6rs.org/).

### Hindley-Milner Type System
A Hindley-Milner (HM) type system is a classical type system for the lambda calculus with parametric polymorphism. Among HM's more notable properties are its completeness and its ability to infer the most general types of a given program without programmer-supplied type annotations or other hints. Algorithm W is an efficient type inference method in practice, and has been successfully applied on large code bases, although it has a high theoretical complexity. The followings are detailed rules:
1. Variable Access 
This rule is usually used for getting type of variable `e1`. You can find its implementation in [let.sls](../../analysis/type/rules/let.sls) and may other files in the future.
$$ \frac{(x:\sigma) \in \Gamma}{\Gamma \vdash (x:\sigma)} $$ 

2. Application
This rule is usually used for applying procedure `e1` to `e2` like `(e1 e2)` and get this expression's type. You can find its implementation in [application.sls](../../analysis/type/rules/application.sls) and may other files in the future.
$$\frac{\Gamma \vdash (e_1:(\tau_1 \to \tau_2)) \quad\quad \Gamma \vdash (e_2 : \tau_1) }{\Gamma \vdash ((e_1\ e_2) : \tau_2)}$$

3. Abstract
This rule is usually used for defining procedure's type like `(lambda (x) e)`.
$$\frac{(\Gamma, \;(x:\tau_1)) \vdash (e:\tau_2)}{\Gamma \vdash ((\lambda\ x\ .\ e) : (\tau_1 \to \tau_2))} $$

4. Variable Declaration
This rule is usually used for refering varable's type with `define`,`let` and many others in [identifier.md](./identifier.md). You can find detailed implementation in [let.sls](../../analysis/type/rules/let.sls) and may other files in the future.
$$\frac{\Gamma \vdash (e_1:\sigma) \quad\quad (\Gamma,\,(x:\sigma)) \vdash (e_2:\tau)}{\Gamma \vdash ((\mathtt{let}\ (x = e_1)\ \mathtt{in}\ e_2) : \tau)} $$

5. Subtype
This rule is usually used for checking types' satisfying of $\sigma_1$ and $\sigma_2$.
$$\frac{\Gamma \vdash (e: \sigma_1) \quad\quad \sigma_1 \sqsubseteq \sigma_2}{\Gamma \vdash (e : \sigma_2)}$$

6. Type Generalize
This rule is usually used for type's gernalization, is especially for record type inherient.
$$\frac{\Gamma \vdash (e: \sigma) \quad\quad \alpha \notin \mathtt{free}(\Gamma)}{\Gamma \vdash (e:(\forall\ \alpha\ .\ \sigma))}$$

#### Understanding 
According to [this page](https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand/12535304#12535304):
1. The forms like $\Gamma \vdash (e: \sigma_1)$ are called predictors. Intuitively, it means if practical code satisfy them, a following-up processors will be able to apply.
> For engineering, match macro in [srfi](https://srfi.schemers.org/srfi-241/) or [ufo-match](https://akkuscm.org/packages/ufo-match/) could match them.
2. The horizontal bar means that "[above] implies [below]".
> For engineering, `if`,`cond` and many control structures can handle this.
3. If there are multiple expressions in [above], then consider them anded together; all of the [above] must be true in order to guarantee the [below].
4. $:$ means has type.
5. $\in$ means is in. (Likewise $\not\in$ means "is not in".)
6. $\Gamma$ is usually used to refer to an environment or context; in this case it can be thought of as a set of type annotations, pairing an identifier with its type. Therefor $x : \sigma \in \Gamma$ means that the environment $\Gamma$ includes the fact that $x$ has type $\sigma$.
> $\Gamma$ should be an engine that spits out predictors. However, it's not intuitively.
7. $\vdash$ can be read as proves or determines. $\Gamma \vdash x:\sigma$ means that the environment $\Gamma$ determines that $x$ has type $\sigma$.
> For engineering, $\vdash$ should be offered from $\Gamma$ and be regarded as methods of $\Gamma$ objects. 
8. $,$ is a way of including specific additional assumptions into an environment $\Gamma$. 
9.  Therefore, $\Gamma, x : \sigma \vdash e : \tau'$ means that environment $\Gamma$, with the additional, overriding assumption that $x$ has type $\tau$, proves that $e$ has type $\tau'$.

#### Engineering Intuition and Type Representation
Apparently, the hardest core of type inference is the $\Gamma$ which represent a machine digesting static code, consuming codes matching predictors, and after a bunch of cracks and crashes, splitting out codes match other predictors. We hope the latest ones code tell us something about types and HM-system indicates how it is possible.

It's roughly knows, types have its context, which means our types' representation should involve [identifier-references](../analysis/identifier.md); types correspond to different data structures, including procedures(lambda expression), lists, and vectors (records pairs, and many others to be considering); and types maybe recursively dependent on themselves. For these, scheme-langserver constructs type expression following these regulations:
1. Each type expression is consisted with [identifier-references](../../analysis/identifier/reference.sls) of type predictors like `number?`, some inner predictors like `something?`, some structure marker, like `<-` for lambdas, `()` for lists and `#()` for vectors and shortcut marker include `**1` as `+` in regular expression and `...` as `*`.
2. They're formed as `(return type <- ((parameter type) ...))` for [Application Rule](#hindleymilner-type-system) or `((parameter type)...)` for [Variable Access Rule](#hindleymilner-type-system). 
3. Type expressions are scoped in [`document`](../../virtual-file-system/document.sls). This means once time $\Gamma$ machine can only digest one document and type is usually attached to [identifier-reference](../../analysis/identifier/reference.sls)'s `type-exressions`. It much benefits the code indexing.

### Unification Machine
In logic and computer science, unification is an algorithmic process of solving equations between symbolic expressions. That means in HM-system, predictors above and below the horizon construct the two sides of an equation into which we hope to translate static codes. And depending on which expressions are allowed to occur in an equation set, and which expressions are considered equal, several frameworks of unification are distinguished. Intuitively, this machine is a complex graph database: each node is a predictor, and directed edges linked them. We classify these edges into different relationships and hope to raise an algorithm which will act like a spider walking on this net.

In other words, unification machine will run a routing process by finding a path from static code, usually represented with [index-node](../../virtual-file-system/index-node.sls) to type expression: as following code shows, this index-node intuitively should return a type `number?` with r6rs context; and further, the `number?` is indicated by the `+` procedure's type `(number?) <- (number? ...)`. 
```scheme
(+ a b)
```
According to the mathematic tradition created by François Viète, here the unification machine must import [variables](../../analysis/type/variable.sls) to denote these expressions and their parts. The path mentioned in last paragraph now is from index-node, via a variable representing its corresponding type expression, via many other variables corresponding to many other index-nodes and finally to some type-known index-nodes, which are practically specified by r6rs identifier-references. Path-memory and cycle detection mechanism would avoid dead loop.

The [detailed process](../../analysis/type/type-inferencer.sls) is in [Robinson's Unifaction Algorith](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm) tongue. After [identifier catching](./identifier.md), construct graph for single document with `construct-substitution-list-for` procedure and the inference process is like following:
1. Maintain a substitution list as the equation set;
2. As for the type of specific index-node, finding corresponding substitutions by matching the left side of equations;
3. Substitute the left side with right side, and conserve all the transformation;
4. Do 2-3 several loops until there're no new substitutions and output the result;

### Gradual Typing and Implicit Conversion
Hindley-Milner Type System is not omniscient and omnipotent. For scheme and many other untyped languages, type inference for following codes acting very differently to Java, C and many other typed languages:
```scheme
(lambda (a) (+ 1 a))
```

Apparently, the `+` with r6rs context indicates this anonymous procedure should have type of `(number? <- (number?))`. However, the parameter `a` isn't given any type annotation like in Typescript:
```typescript
function(a: number){return 1+a}
```

Gradual typing wants to make an [implicit conversation](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/): the `+` only accept `number?` input, why can't assign the `a` with `number?` type? As the author Jeremy Siek says, this will deconstruct the building of subtype rule. But, as I can see, this is the valuable compromise.

### TODO: Diagnostic based on Type System