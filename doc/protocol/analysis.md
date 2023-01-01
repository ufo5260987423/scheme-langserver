## How dose scheme-langserver analyze API requests?
Scheme-langserver equips a [request-queue](../../protocol/analysis/request-queue.sls) and a single-threaded thread-pool to apply [peephole optimization](https://dl.acm.org/doi/10.1145/364995.365000) to asynchronously analyze API requests. More specifically, optimization is now only applied to [notification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage), because notification needn't send a response back.

### Text document synchronizing
[Text document synchronizing APIs]( https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization) that `textDocument/didOpen`, `textDocument/didChange` and `textDocument/didClose`, a server must implement all three of them and receive request for documents, for which the content is managed in the client (e.g. they might have changed). Which means the API requests are sequential instead of parallel.

A key insight is from [LunarVim](https://www.lunarvim.org/), it requests `textDocument/didChange` API to follow closely to the keyboard typing and text changing. As showing in the following [code](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#didChangeTextDocumentParams), request property `contentChanges` array usually has only item. 

```javascript
interface DidChangeTextDocumentParams {
	/**
	 * The document that did change. The version number points
	 * to the version after all provided content changes have
	 * been applied.
	 */
	textDocument: VersionedTextDocumentIdentifier;

	/**
	 * The actual content changes. The content changes describe single state
	 * changes to the document. So if there are two content changes c1 (at
	 * array index 0) and c2 (at array index 1) for a document in state S then
	 * c1 moves the document from S to S' and c2 from S' to S''. So c1 is
	 * computed on the state S and c2 is computed on the state S'.
	 *
	 * To mirror the content of a document using change events use the following
	 * approach:
	 * - start with the same initial content
	 * - apply the 'textDocument/didChange' notifications in the order you
	 *   receive them.
	 * - apply the `TextDocumentContentChangeEvent`s in a single notification
	 *   in the order you receive them.
	 */
	contentChanges: TextDocumentContentChangeEvent[];
}
```

Straightly, if scheme-langserver had a job queue, and it would be efficient that merges several `textDocument/didChange` requests into one. And this may harshly speed up indexing.