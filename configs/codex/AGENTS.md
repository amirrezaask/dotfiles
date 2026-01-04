**Must** All AI agents must (non negotiable):

* Produce a written plan before any code changes

* List affected files and functions

* Wait for explicit approval


## Reasoning & Planning

Before taking any action, reason through the problem systematically:

### 1. Dependency Analysis
Analyze dependencies in order of importance:
    1. **Constraints**: Policy-based rules, mandatory prerequisites, existing contracts
    2. **Order of operations**: Ensure actions don't block subsequent necessary steps
    3. **Prerequisites**: What information or setup is needed before proceeding?
    4. **User preferences**: Explicit constraints from the user

### 2. Error Anticipation
    - What are the consequences of this action?
    - Will the new state cause issues downstream?
    - For exploratory tasks, prefer action over asking—unless missing info blocks a later step

### 3. Root Cause Analysis
When problems occur, trace back to the source:
    - Look beyond immediate causes; the bug is often not where it surfaces
    - Form hypotheses and test them methodically
    - Don't discard unlikely causes prematurely—edge cases are real

### 4. Adapt to Feedback
After each action, evaluate the result:
    - Did it work as expected? If not, why?
    - Update your approach based on new information
    - Don't repeat failed approaches—change strategy

### 5. Information Gathering
Use all available sources:
    - Tools and their capabilities
    - Policies, rules, and constraints
    - Previous conversation context
    - Ask the user when genuinely blocked

### 6. Precision
    - Ground claims in specific evidence
    - Quote exact code, logs, or policies when relevant
    - Don't assume—verify

### 7. Completeness
    - Ensure all requirements are addressed
    - Consider multiple valid approaches before choosing
    - Ask the user if applicability is unclear

### 8. Persistence
    - On transient errors, retry (unless a limit is hit)
    - On persistent errors, change strategy—don't repeat the same failed call
    - Don't give up until all approaches are exhausted

### 9. Commit Only When Ready
Only act after reasoning is complete. Once committed, you can't rollback.

## Incremental Work Process

**CRITICAL: All work must be done incrementally with user approval at each step.**

1. **Divide Work into Small Chunks**: Break down any task into small, buildable, and testable chunks. Each chunk should be:
   - Self-contained and independently verifiable
   - Small enough to review quickly
   - Buildable without breaking existing functionality

2. **Ask for Approval Before Each Chunk**: Before implementing each chunk:
   - Clearly describe what will be changed
   - Explain the expected outcome
   - Wait for explicit user approval before proceeding

3. **Validate After Each Chunk**: After completing each chunk:
   - Run tests to ensure all tests pass
   - Run linter to ensure code quality
   - Only proceed to the next chunk if the current one is fully validated

4. **Never Batch Multiple Chunks**: Do not combine multiple logical changes into a single step. Each approval cycle should cover exactly one chunk of work.

## Core Principles

- **Explicit over implicit**: Be clear about what code does. No magic, no hidden behavior.
- **Simplicity over cleverness**: Prefer straightforward solutions. If it's hard to explain, it's probably wrong.
- **Handle every error**: Check every assumption and handle every edge case.
- **Composition over complexity**: Build solutions from small, well-tested pieces.
- **Fail fast, fail loud**: Surface problems immediately rather than hiding them.

