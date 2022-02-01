# PF_Proiect
## Binary decision diagram reduction

Input = boolean formula, with:
- ```xy``` meaning x and y
- ```x+y``` meaning x or y
- ```!x``` meaning not x

Example: ```xy+x!yz```

Possible commands:
- ```redundant n``` -> remove an element which is redundant
- ```sameTree x y``` -> removed two elements which describe the same computation tree
- ```done``` -> exit the console where there are no more operations left
- ```donef``` -> exit the console even if there are still operations left

Output: reduced decision tree

### bdd.exe
The executable file has been built using ghc for Windows.
If you don\'t trust it and/or your OS isn\'t compatible with Windows, you can compile the ```bdd.hs``` file yourself using the original [Glasgow Haskell Compiler](https://www.haskell.org/ghc/), specifying your OS.

Command: ```ghc bdd.hs```

Otherwise, just run the executable from the command line.

### Example execution

Enter formula:
-> ```xy+x!yz```

Current BDD:
```
1. if "x" then 2 else 3
2. if "y" then 4 else 5
3. if "y" then 6 else 7
4. if "z" then 9 else 9
5. if "z" then 9 else 8
6. if "z" then 8 else 8
7. if "z" then 8 else 8
8. "0"
9. "1"
```

Which transformation to apply?
-> ```redundant 4```
...

Which transformation to apply?
-> ```done```
Reduced BDD has 5 nodes.