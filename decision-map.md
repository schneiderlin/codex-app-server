# Decision Map

Canonical decisions:

- [Codex Agent External Session Semantics](../../docs/decisions/codex-agent-external-session.md)
  Affects: this component owns Codex app-server process lifecycle, stdio JSON-RPC protocol handling, thread/turn calls, event parsing, and neutral callbacks without depending on My Agent or Feishu state.

Relevant implementation plans:

- [Codex Agent App-Server Extraction](../../docs/impl_plan/codex-agent-appserver-extraction.md)
