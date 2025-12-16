# GlobalMentor Management Library

JVM management and profiling utilities using `java.lang.management`.

## Quick Reference

**Stack profiling:** `com.globalmentor.management.profile.Profiler` — `startStackProbe()`/`stopStackProbe()` for periodic stack sampling; `StackProbeOperation` for configurable profiling with package filtering.

## Overview

### `com.globalmentor.management.profile`

Profiling utilities for JVM thread analysis.

#### Profiler

`Profiler` provides brute-force stack sampling profiling:

```java
Profiler.startStackProbe();
try {
    // code to profile
} finally {
    Profiler.stopStackProbe();
}
```

Calls to `startStackProbe()` and `stopStackProbe()` are reference-counted, allowing nested profiling regions.

#### Stack Probe Operation

`StackProbeOperation` provides configurable stack sampling:

```java
StackProbeOperation probe = new StackProbeOperation();

// Ignore framework packages (includes child packages)
probe.addIgnoreParentPackage("org.springframework");
probe.addIgnoreParentPackage(Package.getPackage("java.util.concurrent"));

Profiler.setStackProbeOperation(probe);
Profiler.startStackProbe();
```

The operation tracks method invocation counts and line numbers for analysis.

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).
