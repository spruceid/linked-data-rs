# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.2] - 2024-03-25

### Removed

- [814006c] Remove unnecessary `V::Literal: ExtractedFromVocabulary` bound on `to_lexical_quads`.

## [0.1.1] - 2024-03-25

### Build

- [4b44dfe] Bump (fix) `rdf-types` version to 0.22.1

## [0.1.0] - 2024-03-22

### Added

- [60b4c5b] Add default RDF quad serializer.
- [f6679d2] Implement all trait for enum types.
- [a82b366] Add support for reverse properties.
- [a82b366] Add support for included nodes.
- [392733f] Impl `LinkedDataSubject` for `Id`.
- [1e813cf] Impl `LinkedData` for the unit type.
- [7e901a0] Add `graph` attribute.
- [0e74f41] Add `Ref` type.
- [0e74f41] Add `reference_interpretation` method to `LinkedDataResource`.
- [77c10a0] Add `serde` support for graph container.
- [0d4079b] Impl `LinkedData` for `Box<T>`.
- [92dd8b8] Implement singleton variant IRI serialization.
- [173c3b0] Add `to_interpreted_graph_quads`.
- [da0dbc0] Add `json_literal` macro.
- [7bf1824] Add parameters to `LiteralTypeMismatch`.
- [3a1d0c1] Add `CowRdfTerm::from_str`.
- [5ae1123] Add `FromLinkedDataError::ExpectedIri`.
- [5ae1123] Add `FromLinkedDataError::UnsupportedIri`.
- [ec8d8cd] Add deserialization context for better errors.
- [1d33fbd] Add `Context::with_anonymous_predicate`
- [e8ffd68] Impl `LinkedDataGraph` for slice types.
- [0b78270] Impl `LinkedDataResource` for `Term`.
- [ece0d9c] Add `to_lexical_subject_quads` function.
- [2afead7] Add CI workflow.

### Build

- [cf7a900] Upgrade `rdf-types` to version 0.17
- [cf7a900] Upgrade `grdf` to version 0.21
- [81278cf] Upgrade `rdf-types` to version 0.18
- [81278cf] Upgrade `grdf` to version 0.22
- [81278cf] Upgrade `json-syntax` to version 0.10
- [a86383a] Upgrade `json-syntax` to 0.11.1
- [c0d45b0] Upgrade `json-syntax` to version 0.12.
- [1ad96c2] Upgrade `xsd-types` to version 0.9.1.
- [2afead7] Bump `rdf-types` to version 0.22.0

### Changed

- [2afead7] Move main package to workspace's toplevel.

### Fixed

- [e47c65e] Fix typo.
- [6ebbda7] Fix warnings.
- [cc1dfe3] Fix some warnings.
- [dad8014] Fix deserialization of ignored fields.
- [9d65503] Fix deserialization lifetime name.
- [ce7db67] Fix `Generator` bound on `to_lexical_quads_with`.
- [58024a9] Fix `json_literal` macro.
- [e774aa9] Fix `json-syntax` dev dependency.

### Removed

- [624e62f] Remove local paths.
- [3e4556b] Remove `LexicalRepr` bound from `SubjectSerializer::graph`.
- [2d69354] Remove json_syntax features.
- [88c1b24] Remove `G: Generator` type parameters.

