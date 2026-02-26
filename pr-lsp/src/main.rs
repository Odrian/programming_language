use tower_lsp::{LspService, Server, Client};

use pr_lsp::Backend;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client: Client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
