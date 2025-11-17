# Contributing to datadriftR

We welcome contributions from the community! Here's how you can help:

## Reporting Issues

- Check existing [issues](https://github.com/yourusername/datadriftR/issues) first
- Provide a minimal reproducible example
- Include session info: `sessionInfo()`

## Submitting Pull Requests

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make your changes:
   - Add tests for new functionality
   - Update documentation (roxygen2 comments)
   - Follow existing code style
4. Run tests: `devtools::test()`
5. Run R CMD check: `devtools::check()`
6. Commit with clear messages
7. Push and open a pull request

## Code Style

- Use R6 classes for new detectors
- Follow existing naming conventions (`add_element()`, `change_detected`)
- Document all exported functions with roxygen2
- Include `@examples` in documentation

## Testing

- All new detectors must include unit tests
- Test coverage should include:
  - Constructor behavior
  - Basic streaming updates
  - Drift detection on synthetic data
  - Edge cases (empty streams, single values, etc.)

## Questions?

Open a [discussion](https://github.com/yourusername/datadriftR/discussions) or contact the maintainers.
