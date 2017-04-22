# Code Translator
A Code Translator for C8, CS146.

This translate converts loops or ifs into labels, goto and simple ifs. All code and examples are in `translate.hs`.

`ghci translate.hs` to run it.

## Updated

Rewritten using Monad, the old version is `trans-old.hs`.

## Examples:

### Simple While Loop with Break

```c
int i = 0;
while (i < 100) {
  printf("%d\n", i);
  if (i == 66) break;
  ++i;
}
```

Write the code above into Haskell:

```haskell
s2 = [Exp "int i = 0", 
    While "i < 100" 
       [Exp "printf(\"%d\\n\", i)",
        If "i == 66" [Break] [], 
        Exp "++i"]]
```

After translation:

```c
*Main> ts s2
int i = 0;
L0:;
if (!(i < 100)) goto L2;
print i;
if (i == 66) goto L1;
++i;
goto L0;
L2:;
L1:;
```

### Perfect Numbers from 1 to 10000

```c
int i = 1;
while (i <= 10000) {
  int acc = 0;
  for (int j = 1; j < i; ++j) {
    if (i % j == 0) acc += j;
  }
  if (i == acc) printf("%d\n", i);
  ++i;
}
```

 Write the code above into Haskell:

```haskell
s5 = [Exp "int i = 1", 
    While "i <= 10000" 
       [Exp "int acc = 0", 
        (For "int j = 1" "j < i" "++j" 
           [If "i % j == 0" [Exp "acc += j"] []]), 
        (If "i == acc" [Exp "printf(\"%d\\n\", i)"] []), 
        (Exp "++i")]]
```

After translation:

```c
*Main> ts s5
int i = 1;
L0:;
if (!(i <= 10000)) goto L2;
int acc = 0;
int j = 1;
L3:;
if (!(j < i)) goto L6;
if (i % j == 0) acc += j;
L4:;
++j;
goto L3;
L6:;
L5:;
if (i == acc) printf("%d\n", i);
++i;
goto L0;
L2:;
L1:;
```

### Switch

``` haskell
s4 = [Exp "int i = 0", 
    Switch "i" 
       [("0", [Exp "printf(\"%d = 0\\n\", i)"]), 
        ("1", [Exp "printf(\"%d = 1\\n\", i)"]), 
        ("2", [Exp "printf(\"%d = 2\\n\", i)"])] 
       [Exp "printf(\"other\\n\")"]]
```

into:

```c
*Main> ts s4
int i = 0;
if (i == 0) goto L0;
if (i == 1) goto L2;
if (i == 2) goto L4;
printf("other\n");
goto L5;
L4:;
printf("%d = 2\n", i);
L5:;
goto L3;
L2:;
printf("%d = 1\n", i);
L3:;
goto L1;
L0:;
printf("%d = 0\n", i);
L1:;
```

## Report A Problem

Feel free to report mistakes I made or give suggestions at  [GitHub Issue](https://github.com/noti0na1/CodeTranslator/issues).
