# Good and Bad Tests

## Good tests

**Integration-style**: test through real interfaces, not mocks of internal parts.

```rust
// GOOD: tests observable behavior through the workflow's interface
#[test]
fn minting_skips_to_one_past_the_highest_number() {
    let workflow = fixture_workflow(&["0001-first", "0007-later", "0003-middle"]);
    let id = workflow.mint("next").expect("a free number exists");
    assert_eq!(id.as_str(), "0008-next");
}
```

Characteristics:

- Tests behavior callers care about
- Uses the public API only
- Survives internal refactors
- Describes WHAT, not HOW
- One logical assertion per test

## Bad tests

**Implementation-detail tests**: coupled to internal structure.

```rust
// BAD: reaches past the interface into the scan cache
#[test]
fn mint_updates_the_highest_seen() {
    let mut workflow = fixture_workflow(&["0001-first"]);
    workflow.mint("next").unwrap();
    assert_eq!(workflow.highest_seen, 2);
}
```

Red flags:

- Mocking internal collaborators
- Testing private functions
- Asserting on call counts or call order
- Test breaks when refactoring without behavior change
- Test name describes HOW not WHAT
- Verifying through a side channel instead of the interface

```rust
// BAD: bypasses the interface to verify
#[test]
fn resolve_fills_the_candidate_list() {
    let mut r = Resolver::new(&["0025-input-syntax", "0025-input-model"]);
    r.resolve("0025");
    assert_eq!(r.candidates.len(), 2);
}

// GOOD: verifies through the interface, against the documented outcome
#[test]
fn a_fragment_matching_two_live_ids_is_ambiguous() {
    let r = Resolver::new(&["0025-input-syntax", "0025-input-model"]);
    let err = r.resolve("0025").unwrap_err();
    assert!(matches!(err, ResolveError::Ambiguous { candidates } if candidates.len() == 2));
}
```

**Tautological tests**: the expected value restates the implementation, so the test passes by construction.

```rust
// BAD: the expected value is recomputed the way the code computes it
#[test]
fn exit_code_for_findings_is_the_error_class() {
    let report = Report::with(&[finding(Severity::Error)]);
    assert_eq!(report.exit_code(), ExitClass::from(Severity::Error) as i32);
}

// GOOD: the expected value is an independent literal, from the CLI reference
#[test]
fn a_report_with_an_error_finding_exits_1() {
    let report = Report::with(&[finding(Severity::Error)]);
    assert_eq!(report.exit_code(), 1);
}
```
