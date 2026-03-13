use pr_core::error::ErrorQueue;
use pr_core::parser::parse1_tokenize::tokenize;
use pr_core::parser::parse2_syntactic::parse_statements;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;

mod debug_diagnostics;

pub struct Backend {
    pub client: Client,
    pub files:  Arc<RwLock<HashMap<Url, File>>>,
}

#[derive(Hash)]
pub struct File {
    pub text: String,
    pub version: i32,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            files: Default::default(),
        }
    }
}

impl Backend {
    async fn update_file(&self, uri: Url, version: i32, text: &str) {
        self.client.log_message(MessageType::INFO, format!("update file {uri}")).await;

        let mut files = self.files.write().await;
        if let Some(file) = files.get(&uri) && file.version > version {

        } else {
            files.insert(uri, File { version, text: text.to_string() });
        }
    }

    async fn send_diagnostics(&self, uri: Url, diagnostics: Vec<Diagnostic>) {
        let _ = self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn diagnose_file(&self, uri: Url, text: &str) {
        let mut errors = ErrorQueue::default();

        let tokens = tokenize(&mut errors, text);
        // self.add_diagnostics(uri.clone(), debug_diagnostics::token_diag(&tokens)).await;

        let statements = parse_statements(&mut errors, tokens);
        // self.add_diagnostics(uri.clone(), debug_diagnostics::statement_diag(&statements)).await;

        let diagnostics = errors.to_lsp_diagnostics();
        self.send_diagnostics(uri, diagnostics).await;
    }
}

const LSP_VERSION: &str = env!("CARGO_PKG_VERSION");

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "pr lsp".to_owned(),
                version: Some(LSP_VERSION.to_string())
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "pr LSP initialized").await;
    }

    async fn shutdown(&self) -> Result<()> { Ok(()) }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let text_doc = params.text_document;
        let lang_id = text_doc.language_id;
        let uri = text_doc.uri;

        self.client.log_message(MessageType::INFO, format!("open {lang_id} __ {uri}")).await;

        tokio::join!(
            self.update_file(uri.clone(), text_doc.version, &text_doc.text),
            self.diagnose_file(uri, &text_doc.text),
        );
        // self.update_file(uri.clone(), text_doc.version, text_doc.text).await;
        // self.diagnose_file(uri, text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(change) = params.content_changes.into_iter().next() else {
            return; // cos FULL sync kind
        };
        let uri = params.text_document.uri;
        let text = change.text;

        tokio::join!(
            self.update_file(uri.clone(), params.text_document.version, &text),
            self.diagnose_file(uri, &text),
        );
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: "Simple hover information".to_string(),
        });
        Ok(Some(Hover { contents, range: None }))
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        let item = CompletionItem {
            label: "hello".to_string(),
            detail: Some("example completion".to_string()),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        };
        Ok(Some(CompletionResponse::Array(vec![item])))
    }
}
