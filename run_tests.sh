#!/bin/bash
# Cz compiler test runner

CZC="./target/debug/czc"
PASS=0
FAIL=0
TOTAL=0

run_test() {
    local file="$1"
    local test_name=$(grep '// TEST:' "$file" | head -1 | sed 's/.*TEST: //')
    local expected=$(grep '// EXPECT:' "$file" | head -1 | sed 's/.*EXPECT: //')
    TOTAL=$((TOTAL + 1))

    local tmpdir=$(mktemp -d)
    local output_bin="$tmpdir/a.out"

    # Try to compile
    compile_output=$($CZC "$file" -o "$output_bin" 2>&1)
    compile_exit=$?

    case "$expected" in
        success)
            if [ $compile_exit -ne 0 ]; then
                echo "FAIL: $file ($test_name)"
                echo "  Expected: success, Got: compile error (exit $compile_exit)"
                echo "  Output: $compile_output"
                FAIL=$((FAIL + 1))
            else
                # Try to run - main's return value is the exit code, so any
                # exit code is valid. Only signal-killed (exit >= 128) is a failure.
                run_output=$("$output_bin" 2>&1)
                run_exit=$?
                if [ $run_exit -ge 128 ]; then
                    echo "FAIL: $file ($test_name)"
                    echo "  Expected: success, Got: killed by signal (exit $run_exit)"
                    echo "  Output: $run_output"
                    FAIL=$((FAIL + 1))
                else
                    echo "PASS: $file ($test_name)"
                    PASS=$((PASS + 1))
                fi
            fi
            ;;
        compile-error)
            if [ $compile_exit -eq 0 ]; then
                echo "FAIL: $file ($test_name)"
                echo "  Expected: compile-error, Got: success"
                FAIL=$((FAIL + 1))
            else
                echo "PASS: $file ($test_name)"
                PASS=$((PASS + 1))
            fi
            ;;
        link-error)
            # Should compile (generate C) but fail at link stage
            # We check that czc exits non-zero due to linker error
            if [ $compile_exit -eq 0 ]; then
                echo "FAIL: $file ($test_name)"
                echo "  Expected: link-error, Got: success"
                FAIL=$((FAIL + 1))
            else
                # Check that it's a linker error, not a compile error
                if echo "$compile_output" | grep -qi "リンクエラー"; then
                    echo "PASS: $file ($test_name)"
                    PASS=$((PASS + 1))
                else
                    echo "FAIL: $file ($test_name)"
                    echo "  Expected: link-error, Got: other error"
                    echo "  Output: $compile_output"
                    FAIL=$((FAIL + 1))
                fi
            fi
            ;;
        runtime-error)
            if [ $compile_exit -ne 0 ]; then
                echo "FAIL: $file ($test_name)"
                echo "  Expected: runtime-error, Got: compile error"
                echo "  Output: $compile_output"
                FAIL=$((FAIL + 1))
            else
                # Run and expect non-zero exit
                run_output=$("$output_bin" 2>&1)
                run_exit=$?
                if [ $run_exit -eq 0 ]; then
                    echo "FAIL: $file ($test_name)"
                    echo "  Expected: runtime-error, Got: success (exit 0)"
                    FAIL=$((FAIL + 1))
                else
                    echo "PASS: $file ($test_name)"
                    PASS=$((PASS + 1))
                fi
            fi
            ;;
        *)
            echo "SKIP: $file ($test_name) - Unknown EXPECT: $expected"
            ;;
    esac

    rm -rf "$tmpdir"
}

# Build the compiler first
echo "Building czc..."
cargo build 2>&1 | tail -3
if [ $? -ne 0 ]; then
    echo "Build failed!"
    exit 1
fi
echo ""

# Run all tests
for f in $(find tests -name '*.cz' | sort); do
    run_test "$f"
done

echo ""
echo "===================="
echo "Results: $PASS/$TOTAL passed, $FAIL failed"
