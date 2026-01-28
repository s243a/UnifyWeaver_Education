<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for Jython pipeline code generation.

## Question Index

1. [What is the API for compiling predicates to Jython?](#bjy02-q-api)
2. [Why does Jython use Java I/O classes?](#bjy02-q-java-io)
3. [How does BufferedReader work?](#bjy02-q-bufferedreader)
4. [How are Prolog predicates translated to Jython?](#bjy02-q-translation)
5. [Why is the future import needed?](#bjy02-q-future)
6. [How do I safely access dictionary keys?](#bjy02-q-get)
7. [What is the difference between Jython and CPython?](#bjy02-q-cpython)
8. [How do I run the generated Jython code?](#bjy02-q-running)
9. [How does the process function signal filtering?](#bjy02-q-filtering)
10. [How can I improve Jython performance?](#bjy02-q-performance)

---

<a id="bjy02-q-api"></a>
## Q1: What is the API for compiling predicates to Jython?

**Question:** What is the main predicate for compiling Prolog to Jython pipeline code?

**Answer:** Use `compile_predicate_to_jython/3`:

```prolog
?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code).
```

Parameters:
- `filter/2`: Predicate to compile
- `[pipeline_input(true)]`: Enable stdin JSONL processing
- `Code`: Output variable for generated Jython

**See:** [02_pipeline_mode_impl.md#compile_predicate_to_jython3](02_pipeline_mode_impl.md#compile_predicate_to_jython3)

---

<a id="bjy02-q-java-io"></a>
## Q2: Why does Jython use Java I/O classes?

**Question:** Why use Java I/O instead of Python's sys.stdin?

**Answer:** Java I/O provides:

- Consistent encoding handling across platforms
- Better performance on the JVM
- Direct access to Java streams
- Fewer encoding issues than Jython's Python stdin

```python
from java.io import BufferedReader, InputStreamReader
from java.lang import System as JavaSystem

reader = BufferedReader(InputStreamReader(JavaSystem.in))
```

**See:** [02_pipeline_mode_impl.md#why-java-io](02_pipeline_mode_impl.md#why-java-io)

---

<a id="bjy02-q-bufferedreader"></a>
## Q3: How does BufferedReader work?

**Question:** How do I read lines using Java's BufferedReader in Jython?

**Answer:** The BufferedReader pattern:

```python
reader = BufferedReader(InputStreamReader(JavaSystem.in))
line = reader.readLine()
while line is not None:
    # Process line
    line = reader.readLine()
```

`readLine()` returns `None` (Java null) at end of input.

**See:** [02_pipeline_mode_impl.md#bufferedreader-pattern](02_pipeline_mode_impl.md#bufferedreader-pattern)

---

<a id="bjy02-q-translation"></a>
## Q4: How are Prolog predicates translated to Jython?

**Question:** How does the compiler translate Prolog patterns to Jython?

**Answer:** Key translation rules:

| Prolog Pattern | Jython Output |
|----------------|---------------|
| `get_field(R, "value", V)` | `V = R.get('value')` |
| `V > 50` | `V is not None and V > 50` |
| `Output = Input` | `return record` |
| Failure | `return None` |

The `None` check prevents errors on missing fields.

**See:** [02_pipeline_mode_impl.md#filter-compilation](02_pipeline_mode_impl.md#filter-compilation)

---

<a id="bjy02-q-future"></a>
## Q5: Why is the future import needed?

**Question:** Why does the generated code include `from __future__ import print_function`?

**Answer:** Jython 2.7 uses Python 2.x syntax by default:

```python
from __future__ import print_function
```

This enables:
- `print()` as function instead of statement
- Consistency with Python 3.x code
- Forward compatibility

**See:** [02_pipeline_mode_impl.md#print_function](02_pipeline_mode_impl.md#print_function)

---

<a id="bjy02-q-get"></a>
## Q6: How do I safely access dictionary keys?

**Question:** How does the process function handle missing keys?

**Answer:** Use the `.get()` method:

```python
value = record.get('value')      # Returns None if missing
value = record.get('value', 0)   # Returns 0 if missing
```

Combined with `None` check:
```python
if value is not None and value > 50:
    return record
```

**See:** [02_pipeline_mode_impl.md#safe-field-access](02_pipeline_mode_impl.md#safe-field-access)

---

<a id="bjy02-q-cpython"></a>
## Q7: What is the difference between Jython and CPython?

**Question:** How does Jython differ from standard Python?

**Answer:** Key differences:

| Feature | Jython | CPython |
|---------|--------|---------|
| Runtime | JVM | Native |
| I/O | Java classes | Python built-ins |
| Libraries | Java + limited Python | Full Python ecosystem |
| stdin | `JavaSystem.in` | `sys.stdin` |

Jython can use Java libraries directly but has limited Python 3 support.

**See:** [02_pipeline_mode_impl.md#jython-vs-cpython](02_pipeline_mode_impl.md#jython-vs-cpython)

---

<a id="bjy02-q-running"></a>
## Q8: How do I run the generated Jython code?

**Question:** How do I execute the generated Jython pipeline?

**Answer:** Several options:

```bash
# Direct execution
echo '{"value": 75}' | jython filter_pipeline.py

# With standalone JAR
java -jar jython-standalone-2.7.3.jar filter_pipeline.py

# With classpath
CLASSPATH=deps/* jython filter_pipeline.py < input.jsonl
```

**See:** [02_pipeline_mode_impl.md#running-generated-code](02_pipeline_mode_impl.md#running-generated-code)

---

<a id="bjy02-q-filtering"></a>
## Q9: How does the process function signal filtering?

**Question:** How does the process function indicate whether to include or exclude a record?

**Answer:** Return the record to include, `None` to exclude:

```python
def process(record):
    value = record.get('value')
    if value is not None and value > 50:
        return record  # Include
    return None        # Exclude
```

The pipeline checks `if result is not None` before printing.

**See:** [02_pipeline_mode_impl.md#filter-compilation](02_pipeline_mode_impl.md#filter-compilation)

---

<a id="bjy02-q-performance"></a>
## Q10: How can I improve Jython performance?

**Question:** How can I optimize Jython pipeline performance?

**Answer:** Several approaches:

1. **JVM warm-up**: Subsequent runs are faster due to JIT
2. **Heap size**: Increase for large files
   ```bash
   java -Xmx2g -jar jython-standalone.jar filter_pipeline.py
   ```
3. **Standalone JAR**: Better startup time than installed Jython
4. **Batch processing**: Process multiple files in one JVM session

**See:** [02_pipeline_mode_impl.md#performance-considerations](02_pipeline_mode_impl.md#performance-considerations)
