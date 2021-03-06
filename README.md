_My solutions to the 2019 edition of [Advent Of Code](https://adventofcode.com/2019)._

## Previous participations

* [2017](https://github.com/FlorianCassayre/AdventOfCode-2017)
* [2018](https://github.com/FlorianCassayre/AdventOfCode-2018)

## Problems & Solutions

* [Day 1](https://adventofcode.com/2019/day/1): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day01.scala)
* [Day 2](https://adventofcode.com/2019/day/2): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day02.scala)
* [Day 3](https://adventofcode.com/2019/day/3): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day03.scala)
* [Day 4](https://adventofcode.com/2019/day/4): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day04.scala)
* [Day 5](https://adventofcode.com/2019/day/5): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day05.scala)
* [Day 6](https://adventofcode.com/2019/day/6): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day06.scala)
* [Day 7](https://adventofcode.com/2019/day/7): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day07.scala)
* [Day 8](https://adventofcode.com/2019/day/8): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day08.scala)
* [Day 9](https://adventofcode.com/2019/day/9): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day09.scala)
* [Day 10](https://adventofcode.com/2019/day/10): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day10.scala)
* [Day 11](https://adventofcode.com/2019/day/11): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day11.scala)
* [Day 12](https://adventofcode.com/2019/day/12): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day12.scala)
* [Day 13](https://adventofcode.com/2019/day/13): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day13.scala)
* [Day 14](https://adventofcode.com/2019/day/14): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day14.scala)
* [Day 15](https://adventofcode.com/2019/day/15): [solution](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day15.scala)
* [Day 16](https://adventofcode.com/2019/day/16): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day16.scala)
* [Day 17](https://adventofcode.com/2019/day/17): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day17.scala)
* [Day 18](https://adventofcode.com/2019/day/18): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day18.scala)
* [Day 19](https://adventofcode.com/2019/day/19): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day19.scala)
* [Day 20](https://adventofcode.com/2019/day/20): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day20.scala)
* [Day 21](https://adventofcode.com/2019/day/21): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day21.scala)
* [Day 22](https://adventofcode.com/2019/day/22): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day22.scala)
* [Day 23](https://adventofcode.com/2019/day/23): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day23.scala)
* [Day 24](https://adventofcode.com/2019/day/24): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day24.scala)
* [Day 25](https://adventofcode.com/2019/day/25): –[](https://github.com/FlorianCassayre/AdventOfCode-2019/blob/master/src/main/scala/adventofcode/solutions/Day25.scala)


## Usage

This project runs on Scala `2.13.1` and sbt `1.3.3`.

Use the following template to write a solution:

```Scala
package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  override def solutionA = ???

  override def solutionB = ???

}
```
(change `1` to the current problem day and fill in the `???`)

Then, to run your code start by entering the sbt shell:
```
$ sbt
```

And type the following command:
```
> day 1
```

The output will be printed to the console and stored to the corresponding files in `output/`.

Alternatively, since `Day` extends `App`, all singleton children can be run as regular applications.

## License

This repository is licensed under the MIT License, please refer to the `LICENSE` file.