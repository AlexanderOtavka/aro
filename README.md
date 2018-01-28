# aro

The aro compiler.

## Building and Running

Running `make` will create a docker image named `aro` with the compiled
binary. `make docker-run args="foo bar"` will build and run the docker image,
passing `foo` and `bar` as arguments to the aro executable. `make docker-test`
will run all unit tests in the docker container.
