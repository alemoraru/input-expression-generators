# LOGS, LOGS, LOGS

## **Folder Description** 📖
This folder contains the logs that were obtained and used for the evaluation section (and some Appendices) of the report. <br>
This folder is further split into 3 sub-folders:
* `Arithmetic`
* `Booleans`
* `Conditional`

Each of the above folders contains the logs for the respective ADT. <br>
Following this, each sub-folder contains at least the following sub-folders:
* `QuickCheck`
* `SmallCheck`
* `UniformGen`

## **Naming Conventions** 📑

* `TimeSpace` folders include logs for the time and memory consumption for generating terms of different sizes with the uniform generator.
* `FaultsFound` folders include logs for the execution of 10 test runs of 100 tests of different generation methods, thus include information regarding reported counter-examples and the number of tests needed to report them.
* `MixedGen` folder includes the results for the mixed generator's performance for the execution of 10 test runs of 100 tests.

## **Notes** 💡

* `SmallCheck` test runs may include more than 100 tests (sometimes even more than 1000). This is because of the nature of how SmallCheck works internally. Controlling the number of tests is not possible and also not useful since it poses threats to the actual validity of the exhaustive generation method.
* `MixedGen` test runs may sometimes include less than 100 tests. This is because a lot of tests cases are discared from the QuickCheck generator that are not well-typed.