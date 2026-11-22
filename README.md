# policy-protocol

Policies for cl-stack: permit / forbid / ask with feedback, resource patterns and context conditions, obligations, explicit layers where an inner layer only tightens, and decisions that carry their proof. A thin DSL (`defpolicy`, or the same model as JSON / TOML data) over [logic-protocol](https://github.com/egao1980/logic-protocol)'s analysable Datalog subset. Slice L1 of Track L.

Design: [cl-stack/docs/capabilities/logic.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/logic.md). 0.1.0 lands through team-solve: the contract is `tests/` plus the semantics in the `src/` file headers.

## License

MIT — see [LICENSE](LICENSE).
