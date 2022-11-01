#!/bin/bash

cd `dirname $0`
eval $(opam env)
exec ocamlearlybird $@
