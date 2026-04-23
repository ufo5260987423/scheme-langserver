# 目标
本次工作需要调试scheme-langserver的宏解析功能，以捕获对identifier的声明过程，便于通过LSP（Language Server Protocol）提供goto definition、find references、auto complete功能。你需要完整阅读本文件，然后使用后文所提到的脚本和调试方法，去完成调试。

## 待解析宏的案例
akku包管理器下的(ufo-try)库，try宏的调用可以采用下列方式：
```
(try
    do something here
    (except condition
        [branch to process condition here]
        other branches ...
    )
)
```
其中的c就是try宏声明的一个identifier，它将捕获程序抛出的异常。

## 原理
用户自定义宏经过scheme REPL展开、转换后，将形成完全以scheme primitive宏和 primitive过程（procedure）组织的代码。在这些primitive宏中，let\define等将能够声明identifier。

## scheme-langserver的思路
scheme-langserver希望在branch中提供对c的goto definition、find references、auto complete等服务。为了实现这一点，需要理清用户自定义宏的调用，和宏调用的展开之间的对应关系。
仍然以(ufo-try)，其定义是
```
(define-syntax try
  (lambda (x)
    (syntax-case x (except)
      [(try body0 body1 ... (except condition clause0 clause1 ...))
        #`((call/1cc
        (lambda (escape)
          (with-exception-handler
            (lambda (c)
          (let ([condition c])     ;; clauses may set! this
            #,(let loop ([first #'clause0] [rest #'(clause1 ...)])
                (if (null? rest)
                (syntax-case first (else =>)
                  [(else h0 h1 ...) #'(escape (lambda () h0 h1 ...))]
                  [(tst) #'(let ([t tst]) (if t (escape (lambda () t)) (raise c)))]
                  [(tst => l) #'(let ([t tst]) (if t (escape (lambda () (l t))) (raise c)))]
                  [(tst h0 h1 ...) #'(if tst (escape (lambda () h0 h1 ...)) (raise c))])
                (syntax-case first (=>)
                  [(tst) #`(let ([t tst]) (if t (escape (lambda () t)) #,(loop (car rest) (cdr rest))))]
                  [(tst => l) #`(let ([t tst]) (if t (escape (lambda () (l t))) #,(loop (car rest) (cdr rest))))]
                  [(tst h0 h1 ...) #`(if tst (escape (lambda () h0 h1 ...)) #,(loop (car rest) (cdr rest)))])))))
            (lambda ()
          ;; cater for multiple return values
          (call-with-values
              (lambda () body0 body1 ...)
            (lambda args
              (escape (lambda ()
                    (apply values args))))))))))])))
```
显然，其中的(let ([condition c]) ...)代码指出，对try的宏调用，展开后将通过let宏声明一个identifier名为condition，并且可以被调用。这就形成了展开后的condition和调用中的condition的对应关系，识别出来即可。

# 现状

## 已有功能
通过阅读README.md和doc/目录下的各个文件夹可知目前scheme-langserver已经实现了对primitive宏声明identifier的支持，
1. 核心机制是/analysis/abstract-interpreter.sls
2. 对于primitive宏的处理规则主要在/analysis/identifier/rules下面
3. 处理结果形成了索引，通过/virtual-file-system数据结构保存
4. LSP相关服务支持，在/protocol文件夹下面
5. 主文件是scheme-langserver.sls

## 开发状态
正在开发scheme-langserver的宏解析功能，包括
1.  /analysis/identifier/self-defined-rules/router.sls，负责由abstract-interpreter岔开程序执行通路，将用户自定义宏另例处理；
2.  router.sls将转发akku (ufo-match)中对match宏的调用供调试，也就是说我们调试的时候，目前只看match宏是否被正常处理；
3.  match宏的定义全部由syntax-rules实现，这个和try宏不一样，但是scheme-langserver处理的原理不会改变；
4.  /analysis/identifier/expanders/expansion-wrap.sls的expansion-generator->rule将处理match宏的调用，这边为什么是由这个过程处理，你可以看一下/analysis/identifier/rules/syntax-rules.sls和define-syntax.sls；
5.  /analysis/identifier/expanders包含了宏解析的主要功能。

## 难点
宏定义往往是嵌套的。仍然以ufo-match宏为例，它的定义是大量的syntax-rules宏互相调用和递归调用。scheme-langserver处理的方法是，对每一层嵌套：
1. 识别这层嵌套展开前后的代码对应关系；
2. 调用abstract-interpreter.sls识别展开后代码中的identifier声明；
3. expansion-wrap.sls中有个shallow-copy，基于对应关系将展开后的声明代入到展开前的宏调用代码。

这样，就形成了递归。

## 调试脚本

### 调试环境的准备
bash .akku/env

### 执行脚本
scheme --script ./tests/analysis/identifier/self-defined-rules/test-router.sps

### 脚本输出和问题

当前脚本输出包含下面这段
```
enter-wrap0
(match expression
  [(_ (fuzzy0 **1) fuzzy1 ...)
   (fold-left
     (lambda (exclude-list identifier-parent-index-node)
       (let* ([identifier-index-node (car (index-node-children
                                            identifier-parent-index-node))]
              [target-identifier-reference (let-parameter-process index-node
                                             identifier-index-node
                                             index-node document type)]
              [extended-exclude-list (append
                                       exclude-list
                                       target-identifier-reference)])
         (index-node-excluded-references-set!
           (cadr (index-node-children index-node))
           extended-exclude-list)
         extended-exclude-list))
     '()
     (filter
       (lambda (i) (not (null? (index-node-children i))))
       (index-node-children
         (cadr (index-node-children index-node)))))]
  [else '()])
enter-wrap1
trigger
```
后面按道理应当出现step-into-next-level-step?，但是没有出现。问题是什么？

### 调试方法
在代码中适当通过pretty-print、display等方法打印信息，通过观察程序输出，确定代码分支是否正常执行。

