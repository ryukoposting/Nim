import strutils

template pkg(name: string; cmd = "nimble test"; version = ""; hasDeps = false): untyped =
  packages.add((name, cmd, version, hasDeps))

var packages*: seq[tuple[name, cmd, version: string; hasDeps: bool]] = @[]


pkg "arraymancer", "nim c -r src/arraymancer.nim", "", true
pkg "ast_pattern_matching", "nim c -r tests/test1.nim"
pkg "blscurve", "", "", true
pkg "bncurve", "", "", true
pkg "c2nim", "nim c -r testsuite/tester.nim"
pkg "chronicles", "nim c -o:chr -r chronicles.nim", "", true
pkg "chronos"
pkg "cligen", "nim c -o:cligenn -r cligen.nim"
pkg "compactdict"
pkg "criterion"
pkg "docopt"
pkg "gara"
pkg "glob"
pkg "gnuplot"
pkg "hts", "nim c -o:htss -r src/hts.nim"
pkg "inim"
pkg "itertools", "nim doc src/itertools.nim"
pkg "iterutils"
pkg "karax", "nim c -r tests/tester.nim"
pkg "loopfusion"
pkg "nake", "nim c nakefile.nim"
pkg "neo", "nim c -d:blas=openblas tests/all.nim", "", true
pkg "nicy", "nim c src/nicy.nim"
pkg "nigui", "nim c -o:niguii -r src/nigui.nim"
pkg "nimcrypto", "nim c -r tests/testapi.nim"
pkg "NimData", "", "", true
pkg "nimes", "nim c src/nimes.nim", "", true
pkg "nimgame2", "nim c nimgame2/nimgame.nim", "", true
pkg "nimongo", "nimble test_ci", "", true
pkg "nimpy", "nim c -r tests/nimfrompy.nim"
pkg "nimsl"
pkg "nimsvg"
pkg "nimx", "nim c --threads:on test/main.nim", "", true
pkg "norm", "nim c -o:normm src/norm.nim"
pkg "ormin", "nim c -o:orminn ormin.nim", "", true
pkg "parsetoml"
pkg "patty"
pkg "plotly", "nim c examples/all.nim", "", true
pkg "protobuf", "nim c -o:protobuff -r src/protobuf.nim", "", true
pkg "regex", "", "", true
pkg "rosencrantz", "nim c -o:rsncntz -r rosencrantz.nim"
pkg "sdl1", "nim c -r src/sdl.nim"
pkg "sdl2_nim", "nim c -r sdl2/sdl.nim"
pkg "stint", "nim c -o:stintt -r stint.nim"
pkg "zero_functional", "nim c -r test.nim"
