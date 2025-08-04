use std::fs;
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

/// 自定义文件遍历器
pub struct FileWalker {
    ignore_patterns: Vec<GitIgnoreRule>,
    include_hidden: bool,
    include_git: bool,
}

/// Git 忽略规则
#[derive(Debug, Clone)]
struct GitIgnoreRule {
    pattern: String,
    is_negation: bool,       // 是否是否定规则（!开头）
    is_directory_only: bool, // 是否只匹配目录（/结尾）
    is_absolute: bool,       // 是否是绝对路径（/开头）
    source_dir: PathBuf,     // 规则来源目录
}

/// 遍历结果
#[derive(Debug)]
pub struct WalkEntry {
    path: PathBuf,
    file_type: FileType,
}

#[derive(Debug)]
pub enum FileType {
    File,
    Directory,
}

impl FileWalker {
    /// 创建新的文件遍历器
    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        let mut walker = Self {
            ignore_patterns: Vec::new(),
            include_hidden: true,
            include_git: true,
        };

        // 收集所有.gitignore文件的规则
        walker.collect_gitignore_rules(root.as_ref());

        walker
    }

    /// 设置是否包含隐藏文件
    pub fn include_hidden(mut self, include: bool) -> Self {
        self.include_hidden = include;
        self
    }

    /// 设置是否包含 .git 目录
    pub fn include_git(mut self, include: bool) -> Self {
        self.include_git = include;
        self
    }

    /// 递归收集所有.gitignore文件的规则
    fn collect_gitignore_rules(&mut self, dir: &Path) {
        let gitignore_path = dir.join(".gitignore");
        if gitignore_path.exists() {
            if let Ok(content) = fs::read_to_string(&gitignore_path) {
                self.load_gitignore_patterns(content, dir.to_path_buf());
            }
        }

        // 递归处理子目录
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    let filename = path
                        .file_name()
                        .and_then(|name| name.to_str())
                        .unwrap_or("");

                    // 跳过.git目录和其他隐藏目录以避免无限递归
                    if !filename.starts_with('.') {
                        self.collect_gitignore_rules(&path);
                    }
                }
            }
        }
    }

    /// 从 .gitignore 内容加载忽略模式
    fn load_gitignore_patterns(&mut self, content: String, source_dir: PathBuf) {
        for line in content.lines() {
            let line = line.trim();
            // 跳过空行和注释
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            let mut pattern = line.to_string();
            let mut is_negation = false;
            let mut is_directory_only = false;
            let mut is_absolute = false;

            // 处理否定规则
            if pattern.starts_with('!') {
                is_negation = true;
                pattern = pattern[1..].to_string();
            }

            // 处理目录规则
            if pattern.ends_with('/') {
                is_directory_only = true;
                pattern = pattern[..pattern.len() - 1].to_string();
            }

            // 处理绝对路径
            if pattern.starts_with('/') {
                is_absolute = true;
                pattern = pattern[1..].to_string();
            }

            self.ignore_patterns.push(GitIgnoreRule {
                pattern,
                is_negation,
                is_directory_only,
                is_absolute,
                source_dir: source_dir.clone(),
            });
        }
    }

    /// 检查文件路径是否应该被忽略
    fn should_ignore(&self, path: &Path, root_path: &Path) -> bool {
        let filename = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");

        // 检查隐藏文件
        if !self.include_hidden && filename.starts_with('.') {
            // 但如果设置了包含 .git，则不忽略 .git 目录
            if !self.include_git || filename != ".git" {
                return true;
            }
        }

        // 如果不包含 .git 目录，则忽略它
        if !self.include_git && filename == ".git" {
            return true;
        }

        // 获取相对于根目录的路径
        let relative_path = match path.strip_prefix(root_path) {
            Ok(rel) => rel,
            Err(_) => return false,
        };

        let relative_str = relative_path.to_string_lossy();
        let is_directory = path.is_dir();

        let mut should_ignore = false;

        // 检查所有gitignore规则
        for rule in &self.ignore_patterns {
            if self.matches_rule(rule, &relative_str, filename, is_directory, path, root_path) {
                if rule.is_negation {
                    return false; // 否定规则，不忽略
                } else {
                    should_ignore = true; // 正常规则，忽略
                }
            }
        }

        should_ignore
    }

    /// 检查路径是否匹配特定规则
    fn matches_rule(
        &self,
        rule: &GitIgnoreRule,
        relative_path: &str,
        filename: &str,
        is_directory: bool,
        full_path: &Path,
        root_path: &Path,
    ) -> bool {
        // 如果规则只匹配目录，但当前是文件，则不匹配
        if rule.is_directory_only && !is_directory {
            return false;
        }

        // 计算规则应用的相对路径
        let rule_relative_path = if rule.source_dir == root_path {
            relative_path
        } else {
            // 规则来自子目录，需要计算相对于该子目录的路径
            &*match full_path.strip_prefix(&rule.source_dir) {
                Ok(rel) => rel.to_string_lossy(),
                Err(_) => {
                    // 如果路径不在规则源目录下，则不匹配
                    return false;
                }
            }
        };

        let rule_relative_str = rule_relative_path;

        if rule.is_absolute {
            // 绝对路径匹配
            self.pattern_matches(&rule.pattern, rule_relative_str)
        } else {
            // 相对路径匹配，可以匹配任何层级
            self.pattern_matches(&rule.pattern, filename)
                || self.pattern_matches(&rule.pattern, rule_relative_str)
                || rule_relative_str
                    .split('/')
                    .any(|part| self.pattern_matches(&rule.pattern, part))
        }
    }

    /// 模式匹配实现，支持 * 和 ** 通配符
    fn pattern_matches(&self, pattern: &str, text: &str) -> bool {
        if pattern == text {
            return true;
        }

        // 处理 ** 通配符
        if pattern.contains("**") {
            return self.globstar_match(pattern, text);
        }

        // 处理普通 * 通配符
        if pattern.contains('*') {
            return self.wildcard_match(pattern, text);
        }

        // 检查是否是路径的一部分
        text.split('/').any(|part| part == pattern)
    }

    /// 处理 ** 通配符匹配
    fn globstar_match(&self, pattern: &str, text: &str) -> bool {
        let parts: Vec<&str> = pattern.split("**").collect();

        if parts.len() == 1 {
            return self.wildcard_match(pattern, text);
        }

        let prefix = parts[0];
        let suffix = parts[parts.len() - 1];

        // 简化处理：前缀匹配和后缀匹配
        if prefix.is_empty() && suffix.is_empty() {
            return true; // ** 匹配任何内容
        }

        if prefix.is_empty() {
            return text.ends_with(suffix.trim_start_matches('/'));
        }

        if suffix.is_empty() {
            return text.starts_with(prefix.trim_end_matches('/'));
        }

        text.starts_with(prefix.trim_end_matches('/'))
            && text.ends_with(suffix.trim_start_matches('/'))
    }

    /// 简单的通配符匹配实现
    fn wildcard_match(&self, pattern: &str, text: &str) -> bool {
        let pattern_parts: Vec<&str> = pattern.split('*').collect();

        if pattern_parts.len() == 1 {
            return text == pattern;
        }

        let mut text_pos = 0;

        for (i, part) in pattern_parts.iter().enumerate() {
            if part.is_empty() {
                continue;
            }

            if i == 0 {
                // 第一部分必须在开头匹配
                if !text[text_pos..].starts_with(part) {
                    return false;
                }
                text_pos += part.len();
            } else if i == pattern_parts.len() - 1 {
                // 最后一部分必须在结尾匹配
                return text[text_pos..].ends_with(part);
            } else {
                // 中间部分需要找到匹配位置
                if let Some(pos) = text[text_pos..].find(part) {
                    text_pos += pos + part.len();
                } else {
                    return false;
                }
            }
        }

        true
    }

    /// 遍历目录并返回所有文件
    pub fn walk<P: AsRef<Path>>(&self, root: P) -> Result<Vec<WalkEntry>, std::io::Error> {
        let mut results = Vec::new();
        let root_path = root.as_ref();

        self.walk_recursive(root_path, root_path, &mut results)?;

        Ok(results)
    }

    /// 递归遍历目录
    fn walk_recursive(
        &self,
        current_path: &Path,
        root_path: &Path,
        results: &mut Vec<WalkEntry>,
    ) -> Result<(), std::io::Error> {
        let entries = fs::read_dir(current_path)?;

        for entry in entries {
            let entry = entry?;
            let path = entry.path();
            let metadata = entry.metadata()?;

            // 检查是否应该忽略
            if self.should_ignore(&path, root_path) {
                continue;
            }

            if metadata.is_file() {
                results.push(WalkEntry {
                    path: path.clone(),
                    file_type: FileType::File,
                });
            } else if metadata.is_dir() {
                results.push(WalkEntry {
                    path: path.clone(),
                    file_type: FileType::Directory,
                });

                // 递归遍历子目录
                self.walk_recursive(&path, root_path, results)?;
            }
        }

        Ok(())
    }

    /// 获取目录中最新的修改时间
    pub fn get_latest_modification_time<P: AsRef<Path>>(
        &self,
        root: P,
    ) -> Result<u64, Box<dyn std::error::Error>> {
        let entries = self.walk(root)?;
        let mut latest_time = 0u64;

        for entry in entries {
            if matches!(entry.file_type, FileType::File) {
                if let Ok(metadata) = fs::metadata(&entry.path) {
                    if let Ok(modified) = metadata.modified() {
                        if let Ok(duration) = modified.duration_since(UNIX_EPOCH) {
                            let timestamp = duration.as_secs();
                            if timestamp > latest_time {
                                latest_time = timestamp;
                            }
                        }
                    }
                }
            }
        }

        Ok(latest_time)
    }

    /// 检查目录是否有文件在指定时间之后被修改
    pub fn has_modifications_after<P: AsRef<Path>>(
        &self,
        root: P,
        after_timestamp: u64,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let entries = self.walk(root)?;

        for entry in entries {
            if matches!(entry.file_type, FileType::File) {
                if let Ok(metadata) = fs::metadata(&entry.path) {
                    if let Ok(modified) = metadata.modified() {
                        if let Ok(duration) = modified.duration_since(UNIX_EPOCH) {
                            let timestamp = duration.as_secs();
                            if timestamp > after_timestamp {
                                return Ok(true); // 找到了更新的文件，立即返回
                            }
                        }
                    }
                }
            }
        }

        Ok(false) // 没有找到更新的文件
    }

    /// 复制文件并保持目录结构
    pub fn copy_files_to<P: AsRef<Path>, Q: AsRef<Path>>(
        &self,
        source_root: P,
        dest_root: Q,
    ) -> Result<usize, Box<dyn std::error::Error>> {
        let entries = self.walk(&source_root)?;
        let mut file_count = 0;
        let source_root = source_root.as_ref();
        let dest_root = dest_root.as_ref();

        for entry in entries {
            if matches!(entry.file_type, FileType::File) {
                // 计算相对路径
                let relative_path = entry.path.strip_prefix(source_root)?;
                let dest_path = dest_root.join(relative_path);

                // 确保目标目录存在
                if let Some(parent) = dest_path.parent() {
                    fs::create_dir_all(parent)?;
                }

                // 复制文件
                fs::copy(&entry.path, &dest_path)?;
                file_count += 1;
            }
        }

        Ok(file_count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    fn setup_test_dir() -> PathBuf {
        let temp_dir = std::env::temp_dir().join("idea_backup_test");

        // 清理可能存在的测试目录
        if temp_dir.exists() {
            fs::remove_dir_all(&temp_dir).expect("无法清理测试目录");
        }

        // 创建测试目录结构
        fs::create_dir_all(&temp_dir).expect("无法创建测试目录");
        fs::create_dir_all(temp_dir.join("src")).expect("无法创建src目录");
        fs::create_dir_all(temp_dir.join("target")).expect("无法创建target目录");
        fs::create_dir_all(temp_dir.join(".git")).expect("无法创建.git目录");

        // 创建测试文件
        fs::write(temp_dir.join("Cargo.toml"), "[package]\nname = \"test\"\n").expect("无法创建Cargo.toml");
        fs::write(temp_dir.join("src/main.rs"), "fn main() {}\n").expect("无法创建main.rs");
        fs::write(temp_dir.join("target/test.bin"), "binary\n").expect("无法创建binary文件");
        fs::write(temp_dir.join(".git/config"), "[core]\n").expect("无法创建git配置");
        fs::write(temp_dir.join("README.md"), "# Test Project\n").expect("无法创建README");

        // 创建 .gitignore 文件
        fs::write(temp_dir.join(".gitignore"), "target/\n*.log\n").expect("无法创建.gitignore");

        temp_dir
    }

    #[test]
    fn test_file_walker_basic() {
        let test_dir = setup_test_dir();
        let walker = FileWalker::new(&test_dir);

        let entries = walker.walk(&test_dir).expect("遍历失败");

        // 应该包含一些文件，但排除 target/ 目录下的内容
        assert!(!entries.is_empty());

        // 验证包含了预期的文件
        let paths: Vec<String> = entries.iter()
            .map(|e| e.path.strip_prefix(&test_dir).unwrap().to_string_lossy().to_string())
            .collect();

        assert!(paths.iter().any(|p| p == "Cargo.toml"));
        assert!(paths.iter().any(|p| p == "src/main.rs"));
        assert!(paths.iter().any(|p| p == "README.md"));

        // 清理
        fs::remove_dir_all(&test_dir).ok();
    }

    #[test]
    fn test_gitignore_filtering() {
        let test_dir = setup_test_dir();
        let walker = FileWalker::new(&test_dir);

        let entries = walker.walk(&test_dir).expect("遍历失败");
        let paths: Vec<String> = entries.iter()
            .map(|e| e.path.strip_prefix(&test_dir).unwrap().to_string_lossy().to_string())
            .collect();

        // target/ 目录下的文件应该被忽略
        assert!(!paths.iter().any(|p| p.starts_with("target/")));

        // 清理
        fs::remove_dir_all(&test_dir).ok();
    }

    #[test]
    fn test_include_git_directory() {
        let test_dir = setup_test_dir();
        let walker = FileWalker::new(&test_dir).include_git(true);

        let entries = walker.walk(&test_dir).expect("遍历失败");
        let paths: Vec<String> = entries.iter()
            .map(|e| e.path.strip_prefix(&test_dir).unwrap().to_string_lossy().to_string())
            .collect();

        // 应该包含 .git 目录下的文件
        assert!(paths.iter().any(|p| p.starts_with(".git/")));

        // 清理
        fs::remove_dir_all(&test_dir).ok();
    }

    #[test]
    fn test_wildcard_matching() {
        let test_dir = setup_test_dir();

        // 创建一些日志文件
        fs::write(test_dir.join("app.log"), "log content").expect("无法创建日志文件");
        fs::write(test_dir.join("debug.log"), "debug content").expect("无法创建调试日志");

        let walker = FileWalker::new(&test_dir);
        let entries = walker.walk(&test_dir).expect("遍历失败");
        let paths: Vec<String> = entries.iter()
            .map(|e| e.path.strip_prefix(&test_dir).unwrap().to_string_lossy().to_string())
            .collect();

        // *.log 文件应该被忽略
        assert!(!paths.iter().any(|p| p.ends_with(".log")));

        // 清理
        fs::remove_dir_all(&test_dir).ok();
    }

    #[test]
    fn test_copy_files() {
        let test_dir = setup_test_dir();
        let dest_dir = std::env::temp_dir().join("idea_backup_test_dest");

        if dest_dir.exists() {
            fs::remove_dir_all(&dest_dir).ok();
        }

        let walker = FileWalker::new(&test_dir);
        let file_count = walker.copy_files_to(&test_dir, &dest_dir).expect("复制失败");

        assert!(file_count > 0);
        assert!(dest_dir.join("Cargo.toml").exists());
        assert!(dest_dir.join("src/main.rs").exists());

        // 清理
        fs::remove_dir_all(&test_dir).ok();
        fs::remove_dir_all(&dest_dir).ok();
    }
}
