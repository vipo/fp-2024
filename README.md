# fp-2024

## Animal shelter

#### Syntax
- A batch begins with the keyword `BEGIN` and ends with `END`.
- Each command within the batch is separated by a semicolon (`;`).

### Entering Multi-Line Commands
To enter batch commands:
1. Run 'stack run fp2024-three'
2. Use the `:paste` command in the REPL:

#### Examples

Single query:
1)
```plaintext
ADD cat Tom 6
```

Batch queries:
```plaintext
:paste
BEGIN
ADD cat Tom 2;
ADD dog Max 5;
DELETE dog Max 5;
ADD fish Jem 1;
END
```
2)
```plaintext
:paste
BEGIN
ADD dog Max 5;
END
```
3)
```plaintext
:paste
BEGIN
ADD dog Maxi 5;
LIST
END
```
4)
To test LOAD - will show current animals that were added from 'state.txt' file
```plaintext
:paste
BEGIN
LIST
END
```

5)
```plaintext
:paste
BEGIN
ADD snake Long 10;
ADD bug Sim 1;
ADD cat Murr 4;
ADD dog Au 8;
DELETE cat Murr 4;
LIST
END
```
- After pressing <Ctrl-D> we see queries that were completed.
- After writting 'SAVE', file 'state.txt' is created. There is minimal batch of queries.
- After writting 'LOAD' programs state will be loaded from a file 'state.txt'. To see result, we should execute commands from example 3)


Notes:
Added 'Eq' to State in Lib2 - comparison needed for tests.
Removed old tests from Lib2 to have less code.