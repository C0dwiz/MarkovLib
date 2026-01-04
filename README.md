# MarkovLib

MarkovLib is a Haskell library implementing probabilistic models and basic machine learning algorithms:

- **Markov Chains**
- **Hidden Markov Models (HMM)** with Forward and Viterbi
- **Simple Perceptron neural learner**
- **Utility functions for probability vectors**

This project includes a library component (`MarkovLib`), an example executable, and tests.

---

## 📦 Features

### Markov Chain

Discrete Markov chain simulator with transition matrices.

### Hidden Markov Model (HMM)

- **Forward** algorithm — compute the probability of an observation sequence
- **Viterbi** algorithm — find the most probable hidden state path

### Perceptron (Neural)

Simple linear classifier (perceptron).

---

## 🚀 Installation

### As a Library in Your Own Project

If you're building your own Haskell project and want to **use MarkovLib as a dependency**, follow these steps:

### 🧩 Local Dependency

1. **Copy the `MarkovLib/` folder into your project root**  
   (or use Git submodule).

```

your-project/
├── .git/
├── app/
├── src/
├── MarkovLib/    ← copy/clone this folder here
├── your-project.cabal

````

2. **Edit your `.cabal` file** and add it as a local package:

```cabal
packages: .
          MarkovLib
````

Or if using `stack.yaml`:

```yaml
packages:
- .
- MarkovLib
```

3. **Add dependency** to your library/executable:

```cabal
build-depends:
    base
  , MarkovLib
  , ...
```

Then run:

```sh
cabal update
cabal build
```

---

## 📄 Example Usage

### Markov Chain

```haskell
import Markov.Chain
import System.Random (getStdGen)

main = do
  gen <- getStdGen
  let states = ["Sunny","Rainy"]
      m      = [[0.8,0.2],[0.5,0.5]]
  print $ simulate 10 (fromMatrix states m) "Sunny" gen
```

---

### Hidden Markov Model

```haskell
import Markov.Hidden

let hmm = HMM
      ["Hot","Cold"]
      ["1","2"]
      [0.8,0.2]
      [[0.6,0.4],[0.5,0.5]]
      [[0.7,0.3],[0.4,0.6]]

let (bestPath, bestProb) = viterbi hmm ["1","2","1"]
```

---

### Perceptron

```haskell
import Markov.Neural

let p0 = Perceptron [0,0] 0
let trained = foldl (train 0.2) p0 [ ([0,0],0), ([1,0],1) ]
```

---

## 🧪 Run Tests

```sh
cabal test
```

---

## 📚 Documentation

Generate HTML docs:

```sh
cabal haddock
```

---

## 📜 License

This project is licensed under the **MIT License**. See [LICENSE](LICENSE) file for details.