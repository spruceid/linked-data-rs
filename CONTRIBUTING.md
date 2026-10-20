# Contribution Guidelines

Please read the following guidelines before submitting your changes.

## Code

- Keep functions *small*.
- Avoid repetitions: *factorize* the code.
- Stay organized: use *modules* to categorize the code.
- Keep modules small: use *sub-modules* to break down large categories further.
- Don't re-invent the wheel:
	- Use *standard* library functions.
	- Use *widely adopted* external libraries.
- Use *rich types* to encode invariants statically (e.g. don't overly rely on strings).
- *Justify* code susceptible to cause undefined behavior (e.g. `unsafe` blocks in Rust).
- *Justify* code susceptible to panic (e.g. `unwrap` in Rust).
- Add *doc comments* to all public functions.
	- Add *examples* to non-trivial functions.
	- Document required *invariants* (e.g. `Panics` section in Rust).
- Add *unit tests* for non trivial functions (positive tests, negative tests, edge cases, etc.).

## Commits

- Clean-up the code before committing:
	- Check that the code compiles, with no warnings (e.g. `cargo check` for Rust)
	- Check that all the test pass (e.g. `cargo test` for Rust)
	- Check the formatting (e.g. `cargo fmt` for Rust)
	- Check for advanced warnings (e.g. `cargo clippy` for Rust)
- Use [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/).
- Keep commits small when possible. Split large commits by grouping your changes by theme.

## Pull/Push Requests (PRs)

- Look into the PRs/issues history. Do not try to submit something that has already been rejected before.
- PRs must include a summary of the submitted changes.
- Breaking changes must be documented.
- Breaking changes must be justified.
- Be respectful.