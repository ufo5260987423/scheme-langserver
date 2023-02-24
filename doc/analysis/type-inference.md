## Type inference in scheme-langserver

A program variable can assume a range of values during the execution of a program. An upper bound of such a range is called a type of the variable. Many languages have functioned a type system to prevent the occurrence of execution errors during the running of a program. However, scheme is not the "typed language" where variables can be given (nontrivial) types: it doesn't have types or, equivalently, have a single universal type that contains all values. 

It is also useful to distinguish between two kinds of execution errors: the ones that cause the computation to stop immediately, and the ones that go unnoticed (for a while) and later cause arbitrary behavior. The former are called trapped errors, whereas the latter are untrapped errors. A program fragment is safe if it does not cause untrapped errors to occur. Languages where all program fragments are safe are called safe languages. Therefore, safe languages rule out the most insidious form of execution errors: the ones that may go unnoticed. Untyped languages may enforce safety by performing run time checks. Typed languages may enforce safety by statically rejecting all programs that are potentially unsafe. Typed languages may also use a mixture of run time and static checks.


|        | Typed | Untyped   |
|--------|-------|-----------|
| Safe   | Java  | Scheme    |
| Unsafe | C     | Assembler |

For scheme, programming would be too frustrating in the absence of both compile time and run time checks to protect against corruption, and scheme-langserver would implement a usable type inferencer instead of a fully decidable one. Unlike [hindley-milner-type-inferencer](https://github.com/webyrd/hindley-milner-type-inferencer) and many other implementations, scheme-langserver's type inferencer have to respond LSP requests which interleave code position information, like in completion, goto definition and many others, and consume [r6rs](http://www.r6rs.org/)-based code without any specific type annotations. Instead, according to [Poly Type Inference in Scheme](https://core.ac.uk/download/pdf/38891838.pdf), the first step is to implement First-order Type System. And, according to [Gradual Typing for Functional Languages](https://www.cs.indiana.edu/~lkuper/talks/gradual/gradual.pdf), it would base on the intuition that the structure of a type may be partially known/unknown at compile-time and the job of the type system is to catch incompatibilities between the known parts of types. 

Further, the known part is following [the summary from csug 9.5](https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0), that we have about 1808 forms to construct a rule-based type inferencer.

### Hindley-Milner Type System
A Hindley-Milner (HM) type system is a classical type system for the lambda calculus with parametric polymorphism. Among HM's more notable properties are its completeness and its ability to infer the most general types of a given program without programmer-supplied type annotations or other hints. Algorithm W is an efficient type inference method in practice, and has been successfully applied on large code bases, although it has a high theoretical complexity. The followings are detailed rules:
1. Variable Access 
This rule is usually used for getting type of variable `e1`.
$$ \frac{(x:\sigma) \in \Gamma}{\Gamma \vdash (x:\sigma)} $$ 

2. Application
This rule is usually used for applying procedure `e1` to `e2` like `(e1 e2)` and get this expression's type.
$$\frac{\Gamma \vdash (e_1:(\tau_1 \to \tau_2)) \quad\quad \Gamma \vdash (e_2 : \tau_1) }{\Gamma \vdash ((e_1\ e_2) : \tau_2)}$$

3. Abstract
This rule is usually used for defining procedure's type like `(lambda (x) e)`.
$$\frac{(\Gamma, \;(x:\tau_1)) \vdash (e:\tau_2)}{\Gamma \vdash ((\lambda\ x\ .\ e) : (\tau_1 \to \tau_2))} $$

4. Variable Declaration
This rule is usually used for refering varable's type with `define`,`let` and many others in [identifier.md](./identifier.md).
$$\frac{\Gamma \vdash (e_1:\sigma) \quad\quad (\Gamma,\,(x:\sigma)) \vdash (e_2:\tau)}{\Gamma \vdash ((\mathtt{let}\ (x = e_1)\ \mathtt{in}\ e_2) : \tau)} $$

5. Subtype
This rule is usually used for checking types' satisfying of $\sigma_1$ and $\sigma_2$.
$$\frac{\Gamma \vdash (e: \sigma_1) \quad\quad \sigma_1 \sqsubseteq \sigma_2}{\Gamma \vdash (e : \sigma_2)}$$

6. Type Generalize
This rule is usually used for type's gernalization, is especially for record type inherient.
$$\frac{\Gamma \vdash (e: \sigma) \quad\quad \alpha \notin \mathtt{free}(\Gamma)}{\Gamma \vdash (e:(\forall\ \alpha\ .\ \sigma))}$$

### How to Understand
According to [this page](https://stackoverflow.com/questions/12532552/what-part-of-hindley-milner-do-you-not-understand/12535304#12535304):
1. The horizontal bar means that "[above] implies [below]".
2. If there are multiple expressions in [above], then consider them anded together; all of the [above] must be true in order to guarantee the [below].
3. $:$ means has type 
4. $\in$ means is in. (Likewise $\not\in$ means "is not in".)
5. $\Gamma$ is usually used to refer to an environment or context; in this case it can be thought of as a set of type annotations, pairing an identifier with its type. Therefore $x : \sigma \in \Gamma$ means that the environment $\Gamma$ includes the fact that $x$ has type $\sigma$.
6. $\vdash$ can be read as proves or determines. $\Gamma \vdash x:\sigma$ means that the environment $\Gamma$ determines that $x$ has type $\sigma$.
7. $,$ is a way of including specific additional assumptions into an environment $\Gamma$. 
8. Therefore, $\Gamma, x : \sigma \vdash e : \tau'$ means that environment $\Gamma$, with the additional, overriding assumption that $x$ has type $\tau$, proves that $e$ has type $\tau'$.

### Scoped Type Expression
Each type expression is consisted with [identifier-references](../../analysis/identifier/reference.sls) of type predictors like `number?`, control operator like `or`, and some inner predictors like `something?`. They're formed as `((return type) ((parameter type)...))` for [Application Rule](#hindleymilner-type-system) or `((parameter type)...)` for [Variable Access Rule](#hindleymilner-type-system). 

1. In scheme-langserver, type is usually attached to [index-node](../../virtual-file-system/index-node.sls)'s `should-have-type` and `actrual-have-type`, and [identifier-reference](../../analysis/identifier/reference.sls)'s `type-exressions`.  
2. Type attachment occurs after [identifier catchment](./identifier.md). And leaves of index should be firstly typed, compound structures including lists, vectors, procedure apply and syntax transform should be deduced from leaves. 
3. For index-node's `actual-have-type`, it's a pure type expression. If it encountered multi-times attachments, they should be compounded as an `or` expression.
4. For index-node's `should-have-type`, it's a list of type expressions. Especially for procedures' arguments, there may be several conflict procedures, and one procedure's different arguments should align type expressions across the whole procedure applying.  
5. For identifier's `type-expressions`, apparently, it's a list of type expressions because of case-lambda and procedure reload.

### Implementation
Intuitively, we can deduce types by tagging r6rs-procedures. However, here a key problem is recursion like 
```scheme
(define (sum-0 i) 
    (if (zero? i)
        i
        (+ i (sum-0 (- i 1)))))
```

In this case, `sum-0`'s return type is the type of `i` or `(+ i (sum-0 (- i 1)))`, its type depend on itself. Taking [hindley-milner-type-inferencer](https://github.com/webyrd/hindley-milner-type-inferencer) as an example, it developed [miniKanren](http://minikanren.org/) a DSL(Domain specific language) to reify type expression. However, it's seemed procedures have more than one argument must be $\beta$-reduced and causes heavily labors on code transformation. 

Our final implementation is roughly rewriting and shrinking [miniKanren](http://minikanren.org/). After [identifier catching](./identifier.md) in a singular [document](../../virtual-file-system/document.sls) code, we transform [index-node](../../virtual-file-system/index-node.sls) tree into triangular substitution list. Then, each [index-node](../../virtual-file-system/index-node.sls)'s `actural-have-type` and correspond [identifier-reference](../../analysis/identifier/reference.sls)'s `type-expressions` can be reified.

Another problem in above case is raised by typing argument `i`, it can not be answered by simple substitution. However, personally I think there are some simplest approaches to "gather" evidence: for `zero?`, type of its single argument has been typed as `(number? x)` in [rnrs-meta-rules.sls](../../analysis/type/rnrs-meta-rules.sls). If there were no other conflicts, the type of `i` would directly be supposed as `(number? x)`, And conflict detection process can be executed with [type-linkage.sls](../../analysis/type/type-linkage.sls). This feature may need more discussed.
