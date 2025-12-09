# AWK Target Example: Sales Report

This example demonstrates how to compile Prolog predicates to AWK scripts for efficient text processing pipelines.

## Files

- `sales_filter.pl`: Prolog source defining filtering logic (`Category = 'Electronics', Amount > 500`).
- `compile.pl`: Prolog script to compile the filter and aggregation logic.
- `filter_sales.awk`: Generated AWK script to filter rows.
- `sum_sales.awk`: Generated AWK script to sum values.

## How to Run

1. **Compile the Prolog code:**
   ```bash
   swipl -s ../../../init.pl -s compile.pl -g halt
   ```

2. **Run the pipeline:**
   ```bash
   # Create dummy data
   echo -e "Laptop\tElectronics\t1200" > sales.txt
   echo -e "Chair\tFurniture\t150" >> sales.txt
   
   # Run filter -> cut -> sum
   ./filter_sales.awk sales.txt | cut -f3 | ./sum_sales.awk
   ```

## Expected Output

For the standard test case, the total should be **2000**.

```
