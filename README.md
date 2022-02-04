# Sardinas-Patterson Algorithm
Sardinas-Patterson algorithm implemented in Haskell. Checks if set generated by given regular expression is code.

## Usage
```
ghc Main.hs 
./Main
```

## Regular expressions syntax
- ```[a-zA-Z0-9]``` - Usable operands
- ```.``` - Epsilon
- ```a+b``` - Alternative
- ```(a)* ``` - Kleene Star (Takes only first 100 words)
- ```(ab)``` - Sequence

Project made for Functional Programming course at Jagiellonian University in winter semester 2021/2022.