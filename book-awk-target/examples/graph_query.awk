# SPDX-License-Identifier: MIT OR Apache-2.0
# Copyright (c) 2026 UnifyWeaver contributors
# Caller for the generated embedded path.awk library. Load it after path.awk.
END {
    if (start == "") {
        print "Set -v start=NODE and optionally -v target=NODE" > "/dev/stderr"
        exit 2
    }
    if (target != "") {
        if (check(start, target)) print start ":" target
        else exit 1
    } else {
        count = find_all(start, results)
        for (i = 1; i <= count; i++) print start ":" results[i]
    }
}
