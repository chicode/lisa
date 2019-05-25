# Lisa

> A Lisp dialect designed for codingworkshops

## Example

```lisp
(defunc fib-recurse (n)
    (var a 0) (var b 1) (var f 1)
    (defunc calc-fib (i)
        (if (<= i n)
            (do
                (set f (+ a b))
                (set a b)
                (set b f)
                (calc-fib (+ i 1)))
            f))
    (calc-fib 2))
(defunc fib-while (n)
    (var a 0) (var b 1) (var f 1)
    (var i 2)
    (while (<= i n)
        (set f (+ a b))
        (set a b)
        (set b f)
        (set i (+ i 1))
        f))
(= (fib-recurse 10) (fib-while 10) 55)
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
