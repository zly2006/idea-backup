/*!
The overrides module provides a way to specify a set of override globs.
This provides functionality similar to `--include` or `--exclude` in command
line tools.
*/

use std::path::Path;

use crate::gitignore::{self, Gitignore, GitignoreBuilder};
use crate::ignore::{Error, Match};

/// Glob represents a single glob in an override matcher.
///
/// This is used to report information about the highest precedent glob
/// that matched.
///
/// Note that not all matches necessarily correspond to a specific glob. For
/// example, if there are one or more whitelist globs and a file path doesn't
/// match any glob in the set, then the file path is considered to be ignored.
///
/// The lifetime `'a` refers to the lifetime of the matcher that produced
/// this glob.
#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct Glob<'a>(GlobInner<'a>);

#[derive(Clone, Debug)]
#[allow(dead_code)]
enum GlobInner<'a> {
    /// No glob matched, but the file path should still be ignored.
    UnmatchedIgnore,
    /// A glob matched.
    Matched(&'a gitignore::Glob),
}

impl<'a> Glob<'a> {
    fn unmatched() -> Glob<'a> {
        Glob(GlobInner::UnmatchedIgnore)
    }
}

/// Manages a set of overrides provided explicitly by the end user.
#[derive(Clone, Debug)]
pub struct Override(Gitignore);

impl Override {
    /// Returns an empty matcher that never matches any file path.
    pub fn empty() -> Override {
        Override(Gitignore::empty())
    }

    /// Returns the directory of this override set.
    ///
    /// All matches are done relative to this path.
    pub fn path(&self) -> &Path {
        self.0.path()
    }

    /// Returns true if and only if this matcher is empty.
    ///
    /// When a matcher is empty, it will never match any file path.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the total number of ignore globs.
    pub fn num_ignores(&self) -> u64 {
        self.0.num_whitelists()
    }

    /// Returns the total number of whitelisted globs.
    pub fn num_whitelists(&self) -> u64 {
        self.0.num_ignores()
    }

    /// Returns whether the given file path matched a pattern in this override
    /// matcher.
    ///
    /// `is_dir` should be true if the path refers to a directory and false
    /// otherwise.
    ///
    /// If there are no overrides, then this always returns `Match::None`.
    ///
    /// If there is at least one whitelist override and `is_dir` is false, then
    /// this never returns `Match::None`, since non-matches are interpreted as
    /// ignored.
    ///
    /// The given path is matched to the globs relative to the path given
    /// when building the override matcher. Specifically, before matching
    /// `path`, its prefix (as determined by a common suffix of the directory
    /// given) is stripped. If there is no common suffix/prefix overlap, then
    /// `path` is assumed to reside in the same directory as the root path for
    /// this set of overrides.
    pub fn matched<'a, P: AsRef<Path>>(&'a self, path: P, is_dir: bool) -> Match<Glob<'a>> {
        if self.is_empty() {
            return Match::None;
        }
        let mat = self.0.matched(path, is_dir).invert();
        if mat.is_none() && self.num_whitelists() > 0 && !is_dir {
            return Match::Ignore(Glob::unmatched());
        }
        mat.map(move |giglob| Glob(GlobInner::Matched(giglob)))
    }
}

/// Builds a matcher for a set of glob overrides.
#[derive(Clone, Debug)]
pub struct OverrideBuilder {
    builder: GitignoreBuilder,
}

impl OverrideBuilder {
    /// Create a new override builder.
    ///
    /// Matching is done relative to the directory path provided.
    pub fn new<P: AsRef<Path>>(path: P) -> OverrideBuilder {
        OverrideBuilder {
            builder: GitignoreBuilder::new(path),
        }
    }

    /// Builds a new override matcher from the globs added so far.
    ///
    /// Once a matcher is built, no new globs can be added to it.
    pub fn build(&self) -> Result<Override, Error> {
        Ok(Override(self.builder.build()?))
    }

    /// Add a glob to the set of overrides.
    ///
    /// Globs provided here have precisely the same semantics as a single
    /// line in a `gitignore` file, where the meaning of `!` is inverted:
    /// namely, `!` at the beginning of a glob will ignore a file. Without `!`,
    /// all matches of the glob provided are treated as whitelist matches.
    pub fn add(&mut self, glob: &str) -> Result<&mut OverrideBuilder, Error> {
        self.builder.add_line(None, glob)?;
        Ok(self)
    }

    /// Toggle whether the globs should be matched case insensitively or not.
    ///
    /// When this option is changed, only globs added after the change will be affected.
    ///
    /// This is disabled by default.
    pub fn case_insensitive(&mut self, yes: bool) -> Result<&mut OverrideBuilder, Error> {
        // TODO: This should not return a `Result`. Fix this in the next semver
        // release.
        self.builder.case_insensitive(yes)?;
        Ok(self)
    }
}
