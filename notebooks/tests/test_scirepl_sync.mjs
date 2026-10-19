// Offline regressions for the introductory notebooks. Syntax checks never execute cells.
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';

const directory = fileURLToPath(new URL('../', import.meta.url));
const names = ['01_family_tree_tutorial.ipynb', '02_recursion_patterns.ipynb',
    '03_call_graph_analysis.ipynb'];
const appIndex = process.argv.indexOf('--app-workbooks');
if (appIndex >= 0 && !process.argv[appIndex + 1]) {
    throw new Error('--app-workbooks requires the path to SciREPL www/workbooks');
}
const appDirectory = appIndex >= 0 ? resolve(process.argv[appIndex + 1]) : null;
let passed = 0;
let failed = 0;
function check(name, callback) {
    try {
        callback();
        passed++;
        console.log(`PASS ${name}`);
    } catch (error) {
        failed++;
        console.error(`FAIL ${name}: ${error.message}`);
    }
}
const notebooks = names.map(name => JSON.parse(readFileSync(join(directory, name), 'utf8')));
const source = cell => Array.isArray(cell.source) ? cell.source.join('') : cell.source;
const code = (notebook, index) => source(notebook.cells[index]);
const ordered = (text, parts) => {
    let cursor = 0;
    for (const part of parts) {
        const found = text.indexOf(part, cursor);
        assert.ok(found >= 0, `missing or out of order: ${part}`);
        cursor = found + part.length;
    }
};

const prologProbe = spawnSync('swipl', ['--version'], { encoding: 'utf8' });
const hasProlog = prologProbe.status === 0;
if (!hasProlog) console.log('SKIP Prolog syntax checks: install SWI-Prolog to enable them.');
const readTerms = 'repeat, read_term(user_input,T,[syntax_errors(error)]), '
    + '(T == end_of_file -> ! ; fail), halt.';
for (const [index, notebook] of notebooks.entries()) {
    const name = names[index];
    check(`${name}: notebook structure`, () => {
        assert.equal(notebook.nbformat, 4);
        assert.equal(notebook.metadata.kernelspec.language, 'prolog');
        assert.equal(notebook.cells.length, [28, 46, 38][index]);
        for (const cell of notebook.cells) {
            assert.ok(['code', 'markdown'].includes(cell.cell_type));
            assert.ok(Array.isArray(cell.source));
            assert.ok(cell.source.every(line => typeof line === 'string'));
        }
    });
    for (const [cellIndex, cell] of notebook.cells.entries()) {
        if (cell.cell_type !== 'code') continue;
        const text = source(cell);
        const bash = text.startsWith('%%bash\n');
        if (!bash && !hasProlog) continue;
        check(`${name}: cell ${cellIndex} ${bash ? 'Bash' : 'Prolog'} syntax`, () => {
            const result = bash
                ? spawnSync('bash', ['-n'], { input: text.slice('%%bash\n'.length), encoding: 'utf8' })
                : spawnSync('swipl', ['-q', '-f', 'none', '-g', readTerms],
                    { input: text, encoding: 'utf8' });
            assert.ifError(result.error);
            assert.equal(result.status, 0, result.stderr);
        });
    }
    if (appDirectory) {
        check(`${name}: app source parity (native DOT path excepted)`, () => {
            const app = JSON.parse(readFileSync(join(appDirectory, name), 'utf8'));
            assert.equal(notebook.cells.length, app.cells.length);
            for (const [cellIndex, cell] of notebook.cells.entries()) {
                let expected = source(app.cells[cellIndex]);
                if (index === 2 && cellIndex === 34) {
                    expected = expected.replaceAll('/shared/data/even_odd_graph.dot',
                        '../output/even_odd_graph.dot');
                }
                assert.equal(cell.cell_type, app.cells[cellIndex].cell_type);
                assert.equal(source(cell), expected, `cell ${cellIndex} differs`);
            }
        });
    }
}

const [family, recursion, graph] = notebooks;
check('Family tree uses the current classifier API', () => {
    assert.ok(code(family, 26).includes('recursive_compiler:classify_predicate(ancestor/2,'));
    assert.ok(!code(family, 26).includes('classify_recursion('));
});
check('Family tree prints the yes/no query result explicitly', () => {
    assert.ok(code(family, 11).includes("writeln('Yes: Abraham is an ancestor of Jacob')"));
});
check('SCC display rebuilds its graph', () => {
    ordered(code(graph, 14), ['build_call_graph(', 'find_sccs(', 'forall(member(']);
});
check('SCC classification rebuilds its graph and components', () => {
    ordered(code(graph, 16), ['build_call_graph(', 'find_sccs(', 'forall(member(']);
});
check('DOT export rebuilds its source and closes the stream safely', () => {
    ordered(code(graph, 34), ['build_call_graph(', 'generate_dot(', 'setup_call_cleanup(',
        "open('../output/even_odd_graph.dot'", 'write(', 'close(']);
    assert.ok(!code(graph, 34).includes('/shared/'));
});
check('Predicate group means mutual recursion, not transitive reachability', () => {
    assert.ok(source(graph.cells[21]).includes('mutually recursive predicate group'));
    assert.ok(!code(graph, 22).includes('All predicates reachable'));
});
check('Fibonacci call detection does not duplicate results', () => {
    assert.ok(code(graph, 30).includes('once(contains_call_to(_FibBody, fib))'));
});
check('Generated factorial and mutual-recursion scripts can be sourced as libraries', () => {
    for (const index of [20, 38]) {
        ordered(code(recursion, index), ['split_string(', 'append(_LibraryLines,',
            'atomics_to_string(', 'setup_call_cleanup(']);
    }
});
check('All generated-file writes close streams with setup_call_cleanup', () => {
    for (const notebook of notebooks) {
        for (const cell of notebook.cells) {
            const text = source(cell);
            if (cell.cell_type === 'code' && /open\([^\n]+, write,/.test(text)) {
                assert.ok(text.includes('setup_call_cleanup('));
                assert.ok(text.includes('close(_Stream)'));
            }
        }
    }
});
check('Tree-sum Bash argument has valid binary nodes and sums to 11', () => {
    const bash = code(recursion, 30);
    const match = bash.match(/^tree_sum "([^"]+)"$/m);
    assert.ok(match, 'missing quoted tree_sum argument');
    const sum = node => {
        assert.ok(Array.isArray(node));
        if (node.length === 0) return 0;
        assert.equal(node.length, 3, 'each binary node needs value, left and right');
        assert.equal(typeof node[0], 'number');
        return node[0] + sum(node[1]) + sum(node[2]);
    };
    assert.equal(sum(JSON.parse(match[1])), 11);
    assert.ok(bash.includes(`echo "Tree sum of ${match[1]}:"`));
});
console.log(`\n${passed} passed, ${failed} failed`);
process.exitCode = failed ? 1 : 0;
