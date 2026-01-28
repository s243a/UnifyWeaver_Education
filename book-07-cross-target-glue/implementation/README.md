<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 7: Cross-Target Glue - Implementation Documentation

Technical deep-dive documentation for the Cross-Target Glue layer.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 4: Pipe Protocols | [04_pipe_protocols_impl.md](./04_pipe_protocols_impl.md) | [04_pipe_protocols_questions.md](./04_pipe_protocols_questions.md) |

## Question Count

- Chapter 4: 11 questions

## Topics Covered

### Chapter 4: Pipe Protocols
- TSV protocol specification and escape sequences
- JSON Lines protocol
- `generate_tsv_writer/3` and `generate_tsv_reader/3`
- `generate_json_writer/3` and `generate_json_reader/3`
- Header negotiation with type specifications
- Field mapping with `declare_connection/3`
- `generate_pipeline_script/3`
- Format conversion between TSV and JSON
- Performance characteristics

## Source Files

- `src/unifyweaver/glue/pipe_glue.pl`

## Related Books

- [Book 2: Bash Target](../book-02-bash-target/) - Primary target for glue layer
- [Book 5: Python Target](../book-05-python-target/) - Common pipeline participant
- [Book 6: Go Target](../book-06-go-target/) - High-performance pipeline stage
