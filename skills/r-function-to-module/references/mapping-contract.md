# BioPlotBlocks module-mapping contract

## Evidence priority

Use evidence in this order:

1. the real function object and signature in the locked environment;
2. installed same-version Rd/help;
3. same-version namespace, source, Geom/Stat/Layer objects, or exported objects;
4. same-version official vignettes and examples;
5. actual execution probes;
6. documented human decisions.

Third-party tutorials never establish an argument definition by themselves.

## Argument states

Keep these states distinct: `unset`, `explicit`, `explicit_default`, `explicit_null`, `explicit_na`, `raw_expression`, `missing`, and `inherited`. `unset` is module state; `missing` is a real missing actual argument. All Arguments is an audit view and must not emit unset values.

## Value types

Preserve strings, doubles, integers, logicals, `NULL`, typed `NA`, `Inf`, `-Inf`, `NaN`, symbols, named/unnamed arguments, vectors, lists, formulas, function references, calls, `aes()` mappings, colours, enums, and arbitrary R expressions. Never stringify a symbol, formula, call, or special constant.

## Origins

Use only: `formal`, `dots_documented`, `dots_aesthetic`, `dots_forwarded`, `dots_unknown`, or `nested_expression`. Record the forwarding destination and evidence for `...`. Leave unverified entries `dots_unknown` and expose them only through a named Raw editor.

## Mapping rules

- Map one real R function call to one function module by default.
- Map actual arguments to properties while retaining name, position, state, type, and origin.
- Map nested functions to nested calls or Raw Expression slots.
- Represent ggplot2 composition with the real `+` operator.
- Keep `aes(color = group)` distinct from `color = "red"`.
- Preserve unnamed argument order and input aliases unless an approved canonical policy says otherwise.
- Expand templates to ordinary visible modules.
- Never invoke an undisclosed add-on package.

## Grouping and controls

Put roughly 4-10 frequent, comprehensible native arguments in General; place the rest in Advanced while retaining an All Arguments audit view. Grouping never changes generated code.

Recommended controls are logical state selector, enum selector, numeric input, colour input, symbol/column input, formula editor, vector editor, function selector, mapping editor, or code input. Every nontrivial control retains Raw Expression fallback and warn-only constraints.

## Confidence

Use `verified_runtime`, `verified_documentation`, `inferred`, `manual_override`, or `unknown`. Never expose `unknown` through an ordinary generated control.

## Completion gate

A module is not complete until ModuleSpec validation, argument provenance, grouping rationale, complete formal-argument handling, explicit `...` strategy, Raw fallback, codegen/parser/round-trip tests, real execution, version range, mapping record, and human review all pass.
