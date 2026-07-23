# Decision Map

Canonical decisions:

- [Codex Agent External Session Semantics](../../docs/decisions/codex-agent-external-session.md)
  Affects: this component owns Codex app-server process lifecycle, stdio JSON-RPC protocol handling, thread/turn calls, event parsing, and neutral callbacks without depending on My Agent or Feishu state.
- [My-Agent Code Mode Observability](../../docs/decisions/my-agent-code-mode-observability.md)
  Affects: this component contains experimental raw-event handling and translates the narrow Code Mode exec call/output pair into the stable local model.

Relevant implementation plans:

- [Codex Agent App-Server Extraction](../../docs/impl_plan/codex-agent-appserver-extraction.md)
