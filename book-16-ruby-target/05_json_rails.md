<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 5: JSON and Rails Integration

The Ruby target includes options for generating executable scripts and integrating with Rails applications.

## JSON Output Mode

The `json_output` option generates a wrapper method that collects results and outputs JSON:

```prolog
?- compile_predicate_to_ruby(parent/2, [json_output], Code).
```

### Generated Code Structure

```ruby
#!/usr/bin/env ruby
require 'set'
require 'json'

def parent
  facts = [
    ["alice", "bob"],
    ["bob", "charlie"]
  ]
  facts.each { |fact| yield(*fact) }
end

# JSON output wrapper
def parent_json
  results = []
  parent { |*args| results << [args[1], args[2]] }
  puts results.to_json
end

# Run if executed directly
parent_json if __FILE__ == $0
```

### Key Features

1. **`require 'json'`**: Imports JSON standard library
2. **`_json` method**: Collects results into array
3. **Auto-execution**: `if __FILE__ == $0` runs when executed directly
4. **Library mode**: Can be required without auto-execution

### Usage

```bash
# Direct execution
ruby parent.rb
# Output: [["alice","bob"],["bob","charlie"]]

# Pretty print with jq
ruby parent.rb | jq .

# Use as library
ruby -e 'require "./parent.rb"; parent { |x, y| puts "#{x} -> #{y}" }'
```

## Pipeline Mode

The `pipeline` option generates code optimized for stdin/stdout pipelines:

```prolog
?- compile_predicate_to_ruby(transform/2, [pipeline], Code).
```

### Generated Code

```ruby
#!/usr/bin/env ruby
require 'set'
require 'json'

def transform
  # ... compiled predicate ...
end

# Pipeline mode - outputs JSON to stdout
def run_pipeline
  results = []
  transform { |*args| results << [args[1], args[2]] }
  puts results.to_json
end

run_pipeline if __FILE__ == $0
```

### Difference from JSON Output

| Feature | `json_output` | `pipeline` |
|---------|---------------|------------|
| Method name | `predicate_json` | `run_pipeline` |
| Intent | API/direct use | Shell pipelines |
| Output | `puts` (adds newline) | `puts` (adds newline) |

## Rails Integration

### Service Object Pattern

Create a service class that wraps generated predicates:

```ruby
# app/services/ancestor_service.rb
require_relative 'generated/predicates'

class AncestorService
  def self.find_all
    results = []
    ancestor { |x, y| results << { ancestor: x, descendant: y } }
    results
  end

  def self.find_for(person)
    results = []
    ancestor do |x, y|
      results << { ancestor: x, descendant: y } if x == person
    end
    results
  end

  def self.count
    count = 0
    ancestor { |_, _| count += 1 }
    count
  end
end
```

### Controller Usage

```ruby
# app/controllers/ancestors_controller.rb
class AncestorsController < ApplicationController
  def index
    @ancestors = AncestorService.find_all
    render json: @ancestors
  end

  def show
    @ancestors = AncestorService.find_for(params[:person])
    render json: @ancestors
  end
end
```

### Model Integration

```ruby
# app/models/family_tree.rb
class FamilyTree
  include ActiveModel::Model

  def self.grandparents
    results = []
    grandparent { |x, z| results << { grandparent: x, grandchild: z } }
    results
  end
end
```

### Background Jobs

```ruby
# app/jobs/compute_ancestors_job.rb
class ComputeAncestorsJob < ApplicationJob
  queue_as :default

  def perform
    results = []
    ancestor { |x, y| results << [x, y] }

    # Store results
    Rails.cache.write('ancestors', results, expires_in: 1.hour)
  end
end
```

## API Endpoint Example

### Routes

```ruby
# config/routes.rb
Rails.application.routes.draw do
  namespace :api do
    namespace :v1 do
      resources :ancestors, only: [:index]
      resources :parents, only: [:index]
    end
  end
end
```

### Controller

```ruby
# app/controllers/api/v1/ancestors_controller.rb
module Api
  module V1
    class AncestorsController < ApplicationController
      def index
        results = []
        ancestor { |x, y| results << { from: x, to: y } }

        render json: {
          data: results,
          meta: { count: results.size }
        }
      end
    end
  end
end
```

### Response

```json
{
  "data": [
    { "from": "alice", "to": "bob" },
    { "from": "alice", "to": "charlie" },
    { "from": "bob", "to": "charlie" }
  ],
  "meta": { "count": 3 }
}
```

## Testing

### RSpec Examples

```ruby
# spec/services/ancestor_service_spec.rb
RSpec.describe AncestorService do
  describe '.find_all' do
    it 'returns all ancestor relationships' do
      results = AncestorService.find_all
      expect(results).to include(
        { ancestor: 'alice', descendant: 'charlie' }
      )
    end
  end

  describe '.count' do
    it 'counts relationships' do
      expect(AncestorService.count).to be > 0
    end
  end
end
```

### Minitest Examples

```ruby
# test/services/ancestor_service_test.rb
class AncestorServiceTest < ActiveSupport::TestCase
  test "find_all returns relationships" do
    results = AncestorService.find_all
    assert_includes results.map { |r| r[:ancestor] }, 'alice'
  end
end
```

## Performance Considerations

### Caching Results

```ruby
class CachedAncestorService
  def self.all
    Rails.cache.fetch('ancestors', expires_in: 10.minutes) do
      results = []
      ancestor { |x, y| results << [x, y] }
      results
    end
  end
end
```

### Streaming Large Results

For large result sets, stream instead of collecting:

```ruby
# In controller
def index
  response.headers['Content-Type'] = 'application/json'

  self.response_body = Enumerator.new do |yielder|
    yielder << '['
    first = true
    ancestor do |x, y|
      yielder << ',' unless first
      first = false
      yielder << { from: x, to: y }.to_json
    end
    yielder << ']'
  end
end
```

## Best Practices

1. **Wrap in services**: Don't call generated methods directly from controllers
2. **Cache expensive computations**: Use Rails.cache for recursive predicates
3. **Type conversion**: Convert to appropriate Ruby types early
4. **Error handling**: Wrap in begin/rescue for robustness

```ruby
class SafeAncestorService
  def self.find_all
    results = []
    ancestor { |x, y| results << { ancestor: x.to_s, descendant: y.to_s } }
    results
  rescue => e
    Rails.logger.error("Ancestor computation failed: #{e.message}")
    []
  end
end
```

## Summary

| Integration | Pattern | Use Case |
|-------------|---------|----------|
| CLI | `_json` wrapper | Scripts, pipelines |
| Rails API | Service object | JSON endpoints |
| Background | Jobs | Heavy computation |
| Caching | Rails.cache | Repeated queries |

## Related Topics

- [Book 15: Perl Target](../book-15-perl-target/README.md) - Callback-based pipelines
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Multi-target orchestration
