# Lisa

> A Lisp dialect designed for codingworkshops

## Example

```lisp
(def fib (n)
    (if (<= n 2) 1
        (-
            (fib (- n 1))
            (fib (- n 2)))))
(var fib10 (fib 10))
```

## Running examples

```sh
cd examples
elm reactor
```

Navigate to http://localhost:8000/Main.elm

## Usage

```elm
import Lisa

doParse source =
    case Lisa.processString source of
        Ok program ->
            -- do something with program

        Err err ->
          Err err
```

## License

This project is licensed under the GPL v3. See the [LICENSE](LICENSE) file for
more details.
