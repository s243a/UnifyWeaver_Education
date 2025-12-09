import sys
import json
import re
from typing import Iterator, Dict, Any, Set
from dataclasses import dataclass

# FrozenDict - hashable dictionary for use in sets
@dataclass(frozen=True)
class FrozenDict:
    '''Immutable dictionary that can be used in sets.'''
    items: tuple
    
    @staticmethod
    def from_dict(d: Dict) -> 'FrozenDict':
        return FrozenDict(tuple(sorted(d.items())))
    
    def to_dict(self) -> Dict:
        return dict(self.items)
    
    def get(self, key, default=None):
        for k, v in self.items:
            if k == key:
                return v
        return default
    
    def __contains__(self, key):
        return any(k == key for k, _ in self.items)
    
    def __repr__(self):
        return f'FrozenDict({dict(self.items)})'


def read_jsonl(stream: Any) -> Iterator[Dict]:
    '''Read JSONL records.'''
    for line in stream:
        line = line.strip()
        if line:
            yield json.loads(line)

def write_jsonl(records: Iterator[Dict], stream: Any):
    '''Write JSONL records.'''
    for record in records:
        stream.write(json.dumps(record) + '\n')
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/embedding.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/importer.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/onnx_embedding.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/searcher.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/crawler.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/llm.py not found\n
# ERROR: Runtime file src/unifyweaver/targets/python_runtime/chunker.py not found\n
class SemanticRuntime:
    def __init__(self, db_path='data.db', model_path='models/model.onnx', vocab_path='models/vocab.txt'):
        self.importer = PtImporter(db_path)
        if os.path.exists(model_path):
            self.embedder = OnnxEmbeddingProvider(model_path, vocab_path)
        else:
            sys.stderr.write(f'Warning: Model {model_path} not found, embeddings disabled\\n')
            self.embedder = None
            
        self.crawler = PtCrawler(self.importer, self.embedder)
        self.searcher = PtSearcher(db_path, self.embedder)
        self.llm = LLMProvider()
        self.chunker = HierarchicalChunker()

_runtime_instance = None
def _get_runtime():
    global _runtime_instance
    if _runtime_instance is None:
        _runtime_instance = SemanticRuntime()
    return _runtime_instance

def fetch_xml_func(url):
    if os.path.exists(url):
        return open(url, 'rb')
    return None


def _apply_rule_1(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    '''Copy rule: ancestor(_22748,_22750)'''
    if fact.get('relation') == 'parent' and 'arg0' in fact and 'arg1' in fact:
        yield FrozenDict.from_dict({'relation': 'ancestor', 'arg0': fact.get('arg0'), 'arg1': fact.get('arg1')})


def _apply_rule_2(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    '''Join rule: ancestor(_22712,_22714) :- parent(_22712,_22726), ancestor(_22726,_22714)'''
    # Case 1: Fact matches first goal
    if fact.get('relation') == 'parent' and 'arg0' in fact and 'arg1' in fact:
        for other in total:
            if other.get('arg0') == fact.get('arg1') and other.get('relation') == 'ancestor':
                yield FrozenDict.from_dict({'relation': 'ancestor', 'arg0': fact.get('arg0'), 'arg1': other.get('arg1')})
    # Case 2: Fact matches second goal
    if fact.get('relation') == 'ancestor' and 'arg0' in fact and 'arg1' in fact:
        for other in total:
            if other.get('arg1') == fact.get('arg0') and other.get('relation') == 'parent':
                yield FrozenDict.from_dict({'relation': 'ancestor', 'arg0': other.get('arg0'), 'arg1': fact.get('arg1')})




def process_stream_generator(records: Iterator[Dict]) -> Iterator[Dict]:


    '''Semi-naive fixpoint evaluation.'''


    total: Set[FrozenDict] = set()


    delta: Set[FrozenDict] = set()


    


    # Initialize delta with input records


    for record in records:


        frozen = FrozenDict.from_dict(record)


        delta.add(frozen)


        total.add(frozen)


        yield record  # Yield initial facts


    


    # Initialize with static facts





    


    # Fixpoint iteration (semi-naive evaluation)


    while delta:


        new_delta: Set[FrozenDict] = set()


        


        # Apply rules to facts in delta


        for fact in delta:


            for new_fact in _apply_rule_1(fact, total):


                if new_fact not in total and new_fact not in new_delta:


                    new_delta.add(new_fact)


                    yield new_fact.to_dict()
            for new_fact in _apply_rule_2(fact, total):


                if new_fact not in total and new_fact not in new_delta:


                    new_delta.add(new_fact)


                    yield new_fact.to_dict()


        


        total.update(new_delta)


        delta = new_delta




def main():
    records = read_jsonl(sys.stdin)
    results = process_stream_generator(records)
    write_jsonl(results, sys.stdout)

if __name__ == '__main__':
    main()
