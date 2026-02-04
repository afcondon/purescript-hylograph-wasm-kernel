# purescript-hylograph-wasm-kernel

Rust/WASM force simulation kernel for Hylograph.

## Overview

High-performance force simulation using Rust compiled to WebAssembly. Provides 3-4x speedup over the D3.js kernel for large graphs while maintaining API compatibility with `hylograph-simulation`.

## Installation

```bash
spago install hylograph-wasm-kernel
```

## Requirements

The WASM module must be loaded before use. See `showcases/wasm-force-demo` for a complete example.

## Modules

- `Hylograph.Kernel.WASM.Engine` - WASM engine implementation
- `Hylograph.Kernel.WASM.Simulation` - Simulation control
- `Hylograph.Kernel.WASM.Events` - Event handling
- `Hylograph.Kernel.WASM.FFI` - WASM FFI bindings
- `Hylograph.Kernel.WASM.Halogen` - Halogen integration

## Usage

```purescript
import Hylograph.Simulation (runSimulation, Engine(..))

-- Use WASM engine instead of D3
{ handle, events } <- runSimulation config { engine = WASM }
```

## Part of Hylograph

- **hylograph-wasm-kernel** - WASM physics kernel (this package)
- **hylograph-d3-kernel** - D3 physics kernel (fallback)
- **hylograph-simulation** - High-level simulation API
- **hylograph-simulation-core** - Kernel-agnostic types

## License

MIT
