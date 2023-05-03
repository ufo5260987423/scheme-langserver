## 工程直觉：Scheme-langserver如何实现类型推断
包括Java和Typescript等编程语言都有一个类型系统, 能帮程序员尽可能避免代码执行中的错误。这些系统上基于遵循[Hindley-Milner Type System](https://github.com/webyrd/hindley-milner-type-inferencer)或者[System F](https://en.wikipedia.org/wiki/System_F)的一些基本理论构建。然而，艰涩的理论不能保证它们的全知全能。

对scheme语言这样一种“非类型语言(untyped language)”，很多类型系统需要的信息并不能够轻易的从代码中找到：
```scheme
(lambda (a) (+ 1 a))
```
显然，这段代码无法显式给出参数`a`的类型。或者说，`a`的类型是一个包含所有可能类型的全集`something？`（后续会说明这是啥）。这是因为scheme语言不像许多其他语言一样在字面上给出函数的参数的类型。如果是Typescript，为了给`a`标明类型`number`：
```typescript
function(a: number){return 1+a}
```
许多scheme语言的同类（比如Typed-racket）会改变它们的语法并且要求像Typescript这样的语言一样在字面上给出类型信息。但是，scheme-langserver认为可能有另外的手段——当下的大多数scheme代码都在尽量遵循[r6rs](http://www.r6rs.org/)标准，这意味着可以依据标准中给出的函数（或者按照lisp家族的术语来说，过程）所具有的语义来猜测类型信息。回归上面的那段scheme代码，我们有如下推导：
1. 显然`+`是r6rs标准中规定的一个函数。
2. 一般公认这个函数的类型应该是`(number? <- (number? ...))`。其中`<-`说明这是个函数的类型，它的左边是函数的返回值的类型，右边是函数的参数的类型列表。
3. **假定那段scheme代码能够正常运行**，那么`a`的类型就可以确定为`number?`。

这个推导过程将帮助scheme-langserver找到隐含的类型信息并给出类型推导结果。本文将从如下几个反面进行介绍：
1. 类型的表达式；
2. 如何从代码收集类型推断的相关信息；
3. 推断并得到结果；
4. 用类型推断信息对代码进行诊断。

### 类型表达式
让我们首先考虑字面量的类型判断，以下分为四种简单的情况：
1. 诸如`#t`,`3`, `"string"` 和`'a`，我们能够轻易地编写代码把它们的类型用几个判断器（predicator）标注为`boolean?`、 `number?`（`integer?`和`fixnum?`也行）、 `string?`和`symbol?`。
2. 还有一些复合（compound）的情况，如列表`'(1 2 3)`和向量`#(1 2 3)`。这也也没什么稀奇的，用一点递归的手段就能够把它们标注为`'(number? number? number?)`和`'#(number? number? number?)`。这样，一些更复杂的例子`'(1 a "e")`就会有类型标注如`'(number? symbol? string?)`。
3. 对于[r6rs](http://www.r6rs.org/)，我们手动标注了1108个函数和宏。这样scheme-langserver就能处理一些上文那样复杂的案例。当然，在标注过程中，我们对scheme-langserver的类型表达式进行了一些拓展，如：`<-`表示表达式里面有一个函数；`**1`类似正则表达式中的`+`，代表前面的类型或类型表达式出现了一次以上；`...`则类似`*`，表示可以出现0次到无穷次。 
4. 我们虚构了一个判断器`something?`，它对任意待判断的数据都返回“真”。
4. `void?`，用于处理`(void)`。

此外，上文讨论的这些判断器当然是r6rs中的标准判断器，然而我们不能保证在任意代码中它们不会撞上“真假美猴王”。因此，scheme-langserver使用[identifier-references](../analysis/identifier.md)来表示上面这些判断器，带上它们的上下文信息；并使用函数`construct-type-expression-with-meta`构建他们。

### 从代码中收集类型信息
#### Lambda算式
在scheme-langserver的处理过程中，所有的代码通过一整套索引系统管理：代码文件被表示为[document](../../virtual-file-system/document.sls)，其中的代码被解析为[index-node](../../virtual-file-system/index-node.sls)。至此，一种工程上的直觉告诉我们，应当从这一套索引中收集类型相关的信息，为接下来的推导打基础。显然，这一步最困难的事情在于如何从字面量所给出的简单信息，一路火花带闪电，给出那些更为复合的信息。

前人在这个问题上已经水了很多论文，他们大多数用lambda算式的那一套理论摇唇鼓舌：说来说去就一件事，不论怎样形式的代码都可以转化为lambda算式那种形式的代码。本文不会在这里介绍什么$\alpha$转换，$\beta$规约。在这个小节，本文只介绍如下两个案例：
1. 所有的变量赋值过程都可以转换为lambda算式的一种应用。如scheme代码`(let ((a 2)) (+ 1 a))`，可以轻易转换为`((lambda (a) (+ 1 a)) 2)`。用Typescript写个例子的话，就是—— 
```typescript 
function (){
    var a = 2;
    return a+1;
};
//=================>
//可以转换为
function (){
    return (function(a){return a+1;})(2);
};
```
其中`2`是一个字面量，按照上文的讲法，把它的类型信息`number?`传给了变量`a`。

2. 所有的lambda算式，如果把它们看做匿名函数，那么函数的返回值类型取决于lambda算式执行结尾的那个值。在`(lambda (a) (+ 1 a))`中，这决定于`(+ 1 a)`。而后者的返回值类型又决定于`+`。也就是我们在r6rs标准中标注的那些东西。就这样顺藤摸瓜，多少能知道一点其他的。

在此基础上，收集信息的工作仍然有很长的路线要走，因为：
1. 还有很多r6rs的函数我们没有标注；
2. 那些自定义的函数、变量和过程控制语句，它们的类型往往取决于代码的多个不同部分的信息，或者说lambda算式的不同部分的信息。就像上面那个给变量复制的例子`(let ((a 2)) (+ 1 a))`，它的类型信息需要综合处理。为此，我们在[这个文件夹](../../analysis/type/rules/)写了大量的规则来捕捉自定义带来的这些信息；
3. 从理论上讲，诸如`call/cc`这样的高级过程控制无法判断其类型。但是大不了我们就不判断嘛。

#### 扩展类型表达式
上文我们多次提到了这样几个概念“复合”、“复杂”、“多个不同部分”，它们实际上都指向了如下三个事实：
1. 一些lambda算式和它们对应的代码有一些简单或者复合的类型；
2. 这些类型之间通过上面规则定义的方式相互依赖。比如，简单的依赖复杂的，甚至反过来；
3. 有的代码，当我们去收集类型细信息的时候，并不知道它们和其他代码之间的依赖关系，我们需要一种方式来表达这样的未知量。

基于数学的一般原则，scheme-langserver引入了[variable](../../analysis/type/variable.sls)。它会在收集类型信息的过程中占住[identifier-reference](../../analysis/identifier/reference.sls)在类型表达式里面的位置，这样就方便后面进行进一步的逻辑运算和推导。

### 逻辑运算和推导
#### Hindley-Milner类型系统 
虽然lambda算式和我们的那些规则已经指出了类型信息应该如何在代码的结构框架上传递。但是，从字面量到变量，从r6rs标准到自定义的代码，仍然有大量琐碎的逻辑运算和推导工作要做。这里就存在一个问题，即如何利用这些信息做燃料驱动一台引擎，这台引擎将吞食上述信息并突出我们想要的那些未知的类型信息。Hindley-Milner类型系统描述的就是如何在lambda算式基础上做这些工作。它总结了如下几条规则：

1. Variable Access 
$$ \frac{(x:\sigma) \in \Gamma}{\Gamma \vdash (x:\sigma)} $$ 
这一条说的是，如果横线上方的变量$x$已知类型$\sigma$，那么横线下方的逻辑运算成立：即遇到$x$类型系统就能够给出它的类型$\sigma$。这里$\Gamma$可以看做一台无情的机器，虽然上述逻辑运算很愚蠢，但是它仍然会忠诚的执行。$\in$实质上代表了一种扩展机器的知识库的行为——在这条推导规则中，它把$(x:\sigma)$塞进知识库。然后，$\vdash$指向$\Gamma$机械运算的结果。

2. Application
$$\frac{\Gamma \vdash (e_1:(\tau_1 \to \tau_2)) \quad\quad \Gamma \vdash (e_2 : \tau_1) }{\Gamma \vdash ((e_1\ e_2) : \tau_2)}$$
本规则经常用于函数的调用，如将函数`e1`用于参数`e2`——`(e1 e2)` （类似`(+ 1 a)`）——然后获得这个表达式的返回值的类型$\tau_2$。相关实现可以在[application.sls](../../analysis/type/rules/application.sls)中研究。注意，这里的$\to$就是上文中的`<-`，只是方向相反而已。

3. Abstract
$$\frac{(\Gamma, \;(x:\tau_1)) \vdash (e:\tau_2)}{\Gamma \vdash ((\lambda\ x\ .\ e) : (\tau_1 \to \tau_2))} $$
本规则用于获得函数如`(lambda (x) (+ 1 x))`的类型。如果一个函数$e$的参数$x$已知类型$\tau_1$，返回值类型$\tau_2$，则函数的类型为$\tau_1 \to \tau_2$。在scheme-langserver中，这个类型表示为`(tau1 <- tau2)`。

4. Let
$$\frac{\Gamma \vdash (e_1:\sigma) \quad\quad (\Gamma,\,(x:\sigma)) \vdash (e_2:\tau)}{\Gamma \vdash ((\mathtt{let}\ (x = e_1)\ \mathtt{in}\ e_2) : \tau)} $$
已知：表达式$e_1$具有类型$\sigma$；当变量$x$具有类型$\sigma$，函数$e_2$具有类型$\tau$。将$e_1$代入$e_2$表达式中的$x$，返回值类型仍然是$\tau$。

5. Subtype
$$\frac{\Gamma \vdash (e: \sigma_1) \quad\quad \sigma_1 \sqsubseteq \sigma_2}{\Gamma \vdash (e : \sigma_2)}$$
这条规则说的是如果$e$有类型$\sigma_1$，且$\sigma_1$是$\sigma_2$的一个子集（或者说$\sigma_1$继承了$\sigma_2$，$e$也将具有类型$\sigma_2$。

6. Type Generalize
$$\frac{\Gamma \vdash (e: \sigma) \quad\quad \alpha \notin \mathtt{free}(\Gamma)}{\Gamma \vdash (e:(\forall\ \alpha\ .\ \sigma))}$$
这一条规则用来拓展$\Gamma$关于类型的知识（在scheme语言中，大多用于解析record type）。它呈现的逻辑是：若已知某类型变量$\alpha$“不自由”，且某表达式$e$有类型$\sigma$，则$e$同时可能有类型$\alpha$。其中$\alpha$是$\sigma$的任意子集。这一条规则实际上是Subtype的反演——“不自由”说的是在知识库中，存在一些变量具有$\alpha$这个类型，因此我们可以在横线下方一一枚举$\alpha$，看它们是否满足这个判断。有的读者可能对“不自由”和“枚举”没有清晰的认识：它本质上反映的是“排中律”的取消，即“两个互相矛盾的命题必有一真”，这个类似反证法的逻辑不一定成立。因为你必须把矛盾中的一个方面一一列举，才能证明其中一个是真的。对于本规则而言，一一列举“不自由”的$\alpha$在逻辑运算上比较经济实惠。

#### 实现$\Gamma$ 机的工程直觉
1. $\Gamma$机应当拥有一个图知识库，图上的编分两类：$:$ 表示某变量满足相关类型, $\sqsubseteq$描述了集合的包含关系。由于这个图显而易见的稀疏，变量、类型之间没那么多的关系，可以用如下列表来表示这个`((variable : type)... (type1 < type2))`；
2. $\in$表示对$\Gamma$机的知识库拓展操作，也就是往图里面加节点和边。在scheme-langserver中，这个过程类似于`(append old-knowledge-list with-new-knowledge-list)`；
3. 前文提了很多次的那条横线表示的是一种被称作“Unification”的过程。这个过程是说，横线上下两端可以做**等价代换**。这种代换在图中增加了第三种边，即作用于[variable](../../analysis/type/variable.sls)之间的`(vairbale = type-expression-with-variable)`；这就补全了前两种关系之间的空缺；
4. $\vdash$指向一个在图中进行查找的操作，这让$\Gamma$机能够吐出一些我们感兴趣的东西（也就是那些未知的类型啦）。因此，某种适用于上述列表的图计算算法将会在下个小节描述。

### Unification机制 
首先总结上文，$\Gamma$机是一台吞下用[index-node](../../virtual-file-system/index-node.sls)表示的静态代码，然后转化成类型表达式的机器。这里面就有一个算法来推导类型表达式里面啊那些未知量。在scheme-langserver中，[我们的代码采用了](../../analysis/type/type-inferencer.sls) [Robinson's Unification Algorithm](https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm)：$\Gamma$机像蜘蛛一样在图数据构成的网上面爬来爬去，把$:$和$\sqsubseteq$传递的已知信息传递给未知量。在这个过程中，scheme-langserver采用了path-memory和循环检测机制防止死循环。整个机制的细节总结如下:
1. 那个知识图谱实质上是一个“代换”列表，它列出了未知量可以**等价代换**的可选项；
2. 为了方便，我们一般是从“代换”的左边换成右边；
3. 代换不断进行，直到遇到`(variable : type)`给出了已知类型信息。也就是说，当未知量已知，我们就不再进行进一步的“代换”。

#### Gradual Typing和隐式变换
正如本文开头那个例子`(lambda (a) (+ 1 a))`指出的，参数`a`没有被显式标注类型，这就导致Hindley-Milner类型系统中的很多等价代换无法发挥作用。Scheme-langserver使用[Gradual Typing](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/)做了一个[隐式变换](https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/)：`+` 只接受有`number?`类型的参数，那`a`就等于被标注`number?`类型了嘛。 

但是，正如Gradual Typing作者Jeremy Siek所言（作者默默吐槽，其实没看他的论文前，我也发现了这个方法），这种隐式变换将破坏subtype规则。这是因为这种变换的前提是**假设代码总是能够执行**。不过在我看来，总是要有一点妥协的，关键看scheme-langserver能否根据类型推断的结果给出有价值的信息。

### TODO: 用类型推断给出代码的诊断信息