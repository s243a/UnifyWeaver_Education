# Chapter 3: JavaScript Integration

Use WASM-compiled Prolog from Node.js and the browser.

## Node.js

```javascript
const fs = require('fs');

async function loadPrologMath(wasmPath) {
    const bytes = fs.readFileSync(wasmPath);
    const { instance } = await WebAssembly.instantiate(bytes);
    return instance.exports;
}

const math = await loadPrologMath('./prolog_wasm.wasm');
console.log('sum(10) =', math.sum(10));       // 55
console.log('factorial(5) =', math.factorial(5)); // 120
```

## Browser

```html
<!DOCTYPE html>
<html>
<head>
    <title>Prolog WASM Demo</title>
</head>
<body>
    <h1>Prolog WebAssembly</h1>
    <p>sum(100) = <span id="result"></span></p>
    
    <script type="module">
    async function loadPrologMath() {
        const response = await fetch('prolog_wasm.wasm');
        const bytes = await response.arrayBuffer();
        const { instance } = await WebAssembly.instantiate(bytes);
        return instance.exports;
    }
    
    const math = await loadPrologMath();
    document.getElementById('result').textContent = math.sum(100);
    </script>
</body>
</html>
```

## Generated Bindings

### JavaScript

```prolog
?- generate_js_bindings([func(sum, 2, tail_recursion)], JSCode),
   write(JSCode).
```

Output:
```javascript
async function loadPrologMath(wasmPath = 'prolog_wasm.wasm') {
  const response = await fetch(wasmPath);
  const bytes = await response.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes);
  
  return {
    sum: (n) => instance.exports.sum(n)
  };
}
```

### TypeScript

```prolog
?- generate_ts_bindings([func(sum, 2, tail_recursion)], TSCode),
   write(TSCode).
```

Output:
```typescript
export interface PrologMathExports {
  sum(n: number): number;
}

export async function loadPrologMath(wasmPath = 'prolog_wasm.wasm'): Promise<PrologMathExports> {
  // ...
}
```

## React Example

```jsx
import { useState, useEffect } from 'react';

function Calculator() {
    const [math, setMath] = useState(null);
    const [input, setInput] = useState(10);
    
    useEffect(() => {
        loadPrologMath().then(setMath);
    }, []);
    
    if (!math) return <div>Loading WASM...</div>;
    
    return (
        <div>
            <input 
                type="number" 
                value={input} 
                onChange={e => setInput(Number(e.target.value))}
            />
            <p>sum({input}) = {math.sum(input)}</p>
            <p>factorial({input}) = {math.factorial(input)}</p>
        </div>
    );
}
```

## Edge Computing

### Cloudflare Worker

```javascript
export default {
    async fetch(request) {
        const math = await loadPrologMath();
        const n = new URL(request.url).searchParams.get('n') || 10;
        return new Response(`sum(${n}) = ${math.sum(Number(n))}`);
    }
};
```

---

**←** [Previous: Compilation](02_compilation.md) | [📖 Book: WASM Target](./)
