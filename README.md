# hCM

*Conceptual modelling support library for Haskell.*

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.org/MarekSuchanek/hCM.svg?branch=master)](https://travis-ci.org/MarekSuchanek/hCM)
[![Hackage](https://img.shields.io/hackage/v/hCM.svg)](https://hackage.haskell.org/package/hCM-0.1.0.0)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hCM.svg)](http://packdeps.haskellers.com/feed?needle=hCM)

## Introduction

This library is the result of finding a suitable way how to support conceptual modelling within Haskell programming language in the most simple but smart manner. hCM should allow you to build conceptual model with Haskell freely without any restrictions about selected representation of entities, relationships and model itself.

Advantages gained by applying *hCM* are:

- **Compiler-driven modelling** = [GHC](https://www.haskell.org/ghc/) guides you through implementing conceptual model via writing instances and mandatory functions.
- **Visualization** = you can generate visualization of model and its instances (DOT format, see [Graphviz](http://www.graphviz.org))
- **Verification** = correctness of model-implementation consistency is guaranteed by using conceptual model as part of implementation (model-is-a-code)
- **Validation** = you can validate the conceptual model with domain expert thanks to visualization, instance generation (use [QuickCheck](https://hackage.haskell.org/package/QuickCheck)'s [Arbitrary](https://hackage.haskell.org/package/QuickCheck-2.9.2/docs/Test-QuickCheck-Arbitrary.html)) and easy constraint construction as pure functions

## Installation

-  Use standard Haskell way with [stack](https://www.haskellstack.org/).
-  Optionally you can use prepared [Makefile](Makefile).

## Usage

There is very simple example of trivial model within [app/Example.hs](app/Example.hs) file. For more complex example, try to check [hCM-CaseStudy](https://github.com/MarekSuchanek/hCM-CaseStudy).

If you have used this library and want to share your project, feel free to let me know via [issues](https://github.com/MarekSuchanek/hCM/issue).

Sharing **ideas** and reporting **bugs** is more than welcome as well via [issues](https://github.com/MarekSuchanek/hCM/issue)!

## License

This project is licensed under the MIT license - see the [LICENSE](LICENSE) file for more details.
