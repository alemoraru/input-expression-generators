# Research Project - CSE3000

## **Introduction** 📖
This repository contains the code that was written for the _**Automated Validation of Definitional Interpreters**_ research project as part of the bachelor-end thesis work at TU Delft (CSE3000). The main research question that is tackled within this project is "__*How effective are property-based testing frameworks, such as QuickCheck or SmallCheck, for automatically generating input expressions to definitional interpreters?*__".

Before diving into the repository, consider reading the following papers from which the code was heavily inspired/used:
- [QuickCheck: a lightweight tool for random testing of Haskell programs](https://dl.acm.org/doi/10.1145/351240.351266) - by Koen Claessen & John Hughes
- [Smallcheck and lazy smallcheck: automatic exhaustive testing for small values](https://dl.acm.org/doi/10.1145/1411286.1411292) - by Colin Runciman, Matthew Naylor & Fredrik Lindblad
- [Generating Constrained Random Data with Uniform Distribution](https://link.springer.com/chapter/10.1007/978-3-319-07151-0_2) - by Koen Claessen, Jonas Duregård & Michał H. Pałka
- [Testing an optimising compiler by generating random lambda terms](https://dl.acm.org/doi/10.1145/1982595.1982615) - by Michał H. Pałka, Koen Claessen, Alejandro Russo & John Hughes

## **Repository structure** 💻

All logic code is within the files located under the `src` package. The packages `Arithmetic`, `Booleans`, `Conditional`, `RefLC` & `STLC` have roughtly the following structure:
* A `Grammar.hs` file which contains the corresponding ADT definition and the needed instances to generate data with SmallCheck
* A `Generator.hs` file which contains the generators for QuickCheck, the uniform generation approach and a function that one can modify to adjust the relative weights of the generation method that will be used to create test.
* A `TypeChecker.hs` file which can type-check the ADT defined in the same folder.
* A `Suite` folder which contains the following:
  * Some `InterpX.hs` files which contain correct interpreters for the ADT defined in the grammar file.
  * Some `InterpFaultyX.hs` files which contain interpreters with some mistakes.

### **Spaces**

The code needed to generate well-typed terms that follow a uniform distribution and that satisfy a predicate is located within the `Spaces` folder together with additional grammars (such as the simple-typed lambda calculus with De Bruin indices) that were used for testing. 

## **Running the tests** 🏃‍

Before running the tests, check out the `MainSpec` file within the `test` folder. To that end, the default test suite that will be run 
will be `SmallCheck/SpecArith.hs`. To change the test suite that will be run or the number of times it will be executed, check out the `loop` function.

To actually run the tests, enter the terminal of your choice and run the `stack test` command. This will run all tests 
within the specified test suite. 

**Notes**: 
* Alternatively, you can individiually load each test file to run specific properties with custom parameters.
* The uniform generator and the QuickCheck generator are defined in the same file, therefore running a combination of the two requires modification in the respective `Generator.hs` file where relative frequencies are specified. 
* When modifying the relative frequencies in the `Generator` files, run `stack build` before `stack ghci`-ing the required files.

```Haskell
-- Generates tests only with the QuickCheck generator
instance Arbitrary Expr where
    arbitrary = frequency [(1, sized arbNaiveExpr), (0, arbUniformExpr)]
```

```Haskell
-- Generates tests only with the uniform distribution generator
instance Arbitrary Expr where
    arbitrary = frequency [(0, sized arbNaiveExpr), (1, arbUniformExpr)]
```

## **Evaluation** 📊

All the evaluations that were done within this project and documented in the accompanying research paper are shown in the `logs` folder.
To this end, see a more detailed explanation [here](logs/log.md) to fully what each log file entails.

## **Frameworks used** 🔨

- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) - Property-based testing library that generates random test data
- [SmallCheck](https://hackage.haskell.org/package/smallcheck) - Property-based testing library that allows to verify properties for all test cases up to some depth
- [hspec](https://hackage.haskell.org/package/hspec) - Testing framework for Haskell


