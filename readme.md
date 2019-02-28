# GlobalMentor Parent POM

Parent POM for GlobalMentor projects. Provides dependency management for project and testing.

Note that by default the `release` profile signs the artifact with the GlobalMentor software signing key during the `verify` phase. If verifying, installing, or deploying with the `release` profile in a non-GlobalMentor project, the key needs to be changed by overriding the `gpg.keyname` user property, or setting the `keyname` configuration of the [Apache Maven GPG Plugin](https://maven.apache.org/plugins/maven-gpg-plugin/) [`gpg:sign`](https://maven.apache.org/plugins/maven-gpg-plugin/sign-mojo.html) goal.

## Issues

Issues tracked by [JIRA](https://globalmentor.atlassian.net/projects/JAVA).

## Changelog

- v8.0.0: (2018-10-11) First release; supports Java 1.8.
