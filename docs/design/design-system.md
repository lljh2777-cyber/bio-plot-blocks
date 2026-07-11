# BioPlotBlocks interface design system

The approved visual references are `workspace-concept.png` and `parameter-inspector-concept.png` in this directory.

## Tokens

| Role | Value |
|---|---|
| Canvas | `#ffffff` |
| Subtle surface | `#f7f9fc` |
| Raised surface | `#ffffff` |
| Primary text | `#101b35` |
| Secondary text | `#536078` |
| Muted text | `#7d879b` |
| Border | `#d9e0ea` |
| Strong border | `#b7c2d3` |
| Primary | `#0b5cff` |
| Primary hover | `#0048d8` |
| Verified | `#168a4b` |
| Warning | `#b87500` |
| Danger | `#d9303e` |
| Selection surface | `#edf4ff` |

Spacing uses a 4px base with an 8/12/16/20/24/32px working scale. Controls use 7-10px radii; panels use no decorative outer radius. Shadows are reserved for active overlays and expression editors.

## Typography

- UI: Inter-like system stack (`Inter`, `Segoe UI`, `Helvetica Neue`, sans-serif).
- Code: `IBM Plex Mono`, `Cascadia Code`, `SFMono-Regular`, monospace.
- UI controls: 12-13px, deliberate 500-600 weights.
- Panel titles: 15-16px at 650-700 weight.
- Code: 13px at 1.65 line height.

## Component families

- quiet top command bar with one compact primary action;
- compact module-library rows, not cards;
- layer rows connected by a semantic `+` spine and category-color rail;
- parameter audit table with origin, state, value, and expression affordance;
- open plot and code canvases separated by a single border;
- thin status rail for diagnostics and version provenance;
- outline icons with 1.75px strokes, rounded caps, and 16-18px optical size.

## Container and interaction rules

The application uses rails, rows, tables, and canvases. Selected state combines border, background, and a leading marker so it is not conveyed by color alone. Keyboard focus uses a 2px cobalt outline. Motion is limited to 120-180ms state transitions and is disabled under `prefers-reduced-motion`.

At narrow widths, the three workspace rails stack in workflow order and the preview/code region becomes a single column. Functional controls stay code-native and remain keyboard accessible.
