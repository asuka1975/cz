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
                # Run the compiled binary
                run_output=$("$output_bin" 2>&1)
                run_exit=$?

                if [ $run_exit -ge 128 ]; then
                    echo "FAIL: $file ($test_name)"
                    echo "  Expected: success, Got: killed by signal (exit $run_exit)"
                    echo "  Output: $run_output"
                    FAIL=$((FAIL + 1))
                else
                    # Verify exit code and stdout
                    local expected_exit=$(grep '// EXPECTED_EXIT_CODE:' "$file" | head -1 | sed 's/.*EXPECTED_EXIT_CODE: *//')
                    local expected_stdout_raw=$(grep '// EXPECTED_STDOUT:' "$file" | head -1 | sed 's/.*EXPECTED_STDOUT: *//')
                    # Convert \n escapes to actual newlines
                    local expected_stdout
                    expected_stdout=$(printf '%b' "$expected_stdout_raw")

                    local failed=0
                    local fail_details=""

                    if [ -n "$expected_exit" ] && [ "$run_exit" != "$expected_exit" ]; then
                        failed=1
                        fail_details="$fail_details\n  Exit code: expected $expected_exit, got $run_exit"
                    fi

                    if [ -n "$expected_stdout_raw" ] && [ "$run_output" != "$expected_stdout" ]; then
                        failed=1
                        fail_details="$fail_details\n  Stdout: expected $(echo "$expected_stdout" | head -c 80), got $(echo "$run_output" | head -c 80)"
                    fi

                    if [ $failed -eq 1 ]; then
                        echo "FAIL: $file ($test_name)"
                        printf "%b\n" "$fail_details"
                        FAIL=$((FAIL + 1))
                    else
                        echo "PASS: $file ($test_name)"
                        PASS=$((PASS + 1))
                    fi
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
