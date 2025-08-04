#![allow(dead_code)]

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

use clap::Parser;
use dotenv::dotenv;
use serde::{Deserialize, Serialize};

pub mod ensure_7z;
pub mod ignore;
pub mod pattern;
use ensure_7z::ensure_7z_installed;
pub mod default_types;
pub mod dir;
pub mod gitignore;
pub mod overrides;
pub mod pathutil;
pub mod types;
pub mod walk;
use ignore::WalkBuilder;

#[derive(Parser)]
#[command(name = "idea-backup")]
#[command(about = "A CLI tool to backup IdeaProjects with 7z compression")]
struct Args {
    /// Password for 7z compression (overrides .env file)
    #[arg(short, long)]
    password: Option<String>,

    /// Output directory for backup files
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Input directory to scan for projects
    #[arg(short, long, default_value = "~/IdeaProjects")]
    input: String,

    /// Force backup even if no changes detected
    #[arg(short, long)]
    force: bool,
}

/// 项目快照信息
#[derive(Serialize, Deserialize, Debug)]
struct ProjectSnapshot {
    /// 项目路径
    path: String,
    /// 最后修改时间（Unix 时间戳）
    last_modified: u64,
    /// 最后备份时间
    last_backup: u64,
}

/// 快照管理器
#[derive(Serialize, Deserialize, Debug)]
struct SnapshotManager {
    projects: HashMap<String, ProjectSnapshot>,
}

/// 扩展路径，处理 ~ 和 . 字符
fn expand_path(path: &str) -> PathBuf {
    let path = if let Some(path) = path.strip_prefix("~/") {
        dirs::home_dir().expect("无法获取home目录").join(path)
    } else if path == "~" {
        dirs::home_dir().expect("无法获取home目录")
    } else if let Some(path) = path.strip_prefix("./") {
        env::current_dir().expect("无法获取当前目录").join(path)
    } else if path == "." {
        env::current_dir().expect("无法获取当前目录")
    } else {
        PathBuf::from(path)
    };

    // 规范化路径
    path.canonicalize().unwrap_or(path)
}

impl SnapshotManager {
    /// 从文件加载快照信息
    fn load(snapshot_file: &PathBuf) -> Self {
        if snapshot_file.exists() {
            match fs::read_to_string(snapshot_file) {
                Ok(content) => match serde_json::from_str(&content) {
                    Ok(manager) => manager,
                    Err(e) => {
                        eprintln!("解析快照文件失败: {e}");
                        Self::new()
                    }
                },
                Err(e) => {
                    eprintln!("读取快照文件失败: {e}");
                    Self::new()
                }
            }
        } else {
            Self::new()
        }
    }

    /// 创建新的快照管理器
    fn new() -> Self {
        Self {
            projects: HashMap::new(),
        }
    }

    /// 保存快照信息到文件
    fn save(&self, snapshot_file: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
        let content = serde_json::to_string_pretty(self)?;
        fs::write(snapshot_file, content)?;
        Ok(())
    }

    /// 更新项目快照信息
    fn update_project(&mut self, project_name: String, path: String, last_modified: u64) {
        let current_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("时间错误")
            .as_secs();

        self.projects.insert(
            project_name,
            ProjectSnapshot {
                path,
                last_modified,
                last_backup: current_time,
            },
        );
    }
}

/// 获取目录中最新的修改时间
fn get_latest_modification_time(path: &PathBuf) -> Result<u64, Box<dyn std::error::Error>> {
    let mut latest_time = 0;

    for entry in WalkBuilder::new(path).build() {
        if let Ok(entry) = entry {
            if let Ok(metadata) = entry.metadata() {
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
fn has_modifications_after(
    path: &PathBuf,
    after_timestamp: u64,
) -> Result<bool, Box<dyn std::error::Error>> {
    for entry in WalkBuilder::new(path).build() {
        if let Ok(entry) = entry {
            if let Ok(metadata) = entry.metadata() {
                if let Ok(modified) = metadata.modified() {
                    if let Ok(duration) = modified.duration_since(UNIX_EPOCH) {
                        let timestamp = duration.as_secs();
                        if timestamp > after_timestamp {
                            return Ok(true);
                        }
                    }
                }
            }
        }
    }
    Ok(false)
}

fn main() {
    let args = Args::parse();

    dotenv().ok();
    ensure_7z_installed();

    // 获取密码：优先使用命令行参数，其次使用环境变量
    let password = args
        .password
        .or_else(|| env::var("ZIP_PASSWORD").ok())
        .expect("请通过 -p 参数或在.env中设置ZIP_PASSWORD");

    // 扩展输入目录路径
    let input_dir = expand_path(&args.input);
    println!("输入目录: {}", input_dir.display());

    // 检查输入目录是否存在
    if !input_dir.exists() {
        eprintln!("错误: 输入目录不存在: {}", input_dir.display());
        std::process::exit(1);
    }

    // 获取输出目录：优先使用命令行参数，其次使用默认的 ~/IdeaProjects
    let output_dir = args.output.unwrap_or_else(|| {
        dirs::home_dir()
            .expect("无法获取home目录")
            .join("IdeaProjects")
    });

    // 创建输出目录（如果不存在）
    if !output_dir.exists() {
        fs::create_dir_all(&output_dir).expect("无法创建输出目录");
        println!("已创建输出目录: {}", output_dir.display());
    }

    // 快照文件路径
    let snapshot_file = output_dir.join(".backup_snapshots.json");

    // 加载快照管理器
    let mut snapshot_manager = SnapshotManager::load(&snapshot_file);
    println!(
        "已加载快照信息，当前跟踪 {} 个项目",
        snapshot_manager.projects.len()
    );

    let mut processed_count = 0;
    let mut backed_up_count = 0;

    for entry in fs::read_dir(&input_dir).unwrap_or_else(|e| {
        eprintln!("无法读取输入目录 {}: {}", input_dir.display(), e);
        std::process::exit(1);
    }) {
        let entry = entry.expect("读取目录项失败");
        let path = entry.path();
        if path.is_dir() {
            let project_name = path.file_name().unwrap().to_string_lossy().to_string();
            processed_count += 1;

            println!("正在检查项目: {project_name}");

            // 先检查快照，如果强制备份则跳过检查
            if !args.force {
                if let Some(snapshot) = snapshot_manager.projects.get(&project_name) {
                    // 快速检查是否有文件在上次备份后被修改
                    match has_modifications_after(&path, snapshot.last_modified) {
                        Ok(false) => {
                            println!("  跳过 {project_name} - 无更改");
                            continue;
                        }
                        Ok(true) => {
                            println!("  检测到 {project_name} 有更改，准备备份");
                        }
                        Err(e) => {
                            eprintln!("检查项目 {project_name} 修改状态失败: {e}");
                            continue;
                        }
                    }
                } else {
                    println!("  {project_name} 是新项目，准备首次备份");
                }
            } else {
                println!("  强制备份 {project_name}");
            }

            // 只有确定需要备份时才获取精确的最新修改时间
            let latest_modified = match get_latest_modification_time(&path) {
                Ok(time) => time,
                Err(e) => {
                    eprintln!("获取项目 {project_name} 修改时间失败: {e}");
                    continue;
                }
            };

            backed_up_count += 1;

            let backup_name = format!("{project_name}_backup.7z");
            let backup_path = output_dir.join(&backup_name);

            // 创建临时目录
            let temp_dir = std::env::temp_dir().join(format!("backup_{project_name}"));
            if temp_dir.exists() {
                let _ = fs::remove_dir_all(&temp_dir);
            }
            fs::create_dir_all(&temp_dir).expect("无法创建临时目录");

            // 使用FileWalker复制文件到临时目录
            let walker = WalkBuilder::new(&path); // 包含 .git 目录

            let mut file_count = 0;

            for entry in walker.build() {
                if let Ok(entry) = entry {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let relative_path = entry_path.strip_prefix(&path).unwrap();
                        let dest_path = temp_dir.join(relative_path);

                        // 确保目标目录存在
                        if let Some(parent) = dest_path.parent() {
                            fs::create_dir_all(parent).expect("无法创建目标目录");
                        }

                        // 复制文件到临时目录
                        fs::copy(entry_path, &dest_path).expect("无法复制文件到临时目录");
                        file_count += 1;
                    }
                } else {
                    eprintln!("读取目录项失败: {entry:?}");
                }
            }

            println!("  已复制 {file_count} 个文件到临时目录");

            // 使用7z压缩临时目录
            let status = Command::new("7z")
                .arg("a")
                .arg(&backup_path)
                .arg(format!("-p{password}"))
                .arg("-mhe=on")
                .arg("-r")
                .arg(".")
                .current_dir(&temp_dir)
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("7z压缩失败");

            // 清理临时目录
            let _ = fs::remove_dir_all(&temp_dir);

            if status.success() {
                // 更新快照信息
                snapshot_manager.update_project(
                    project_name.clone(),
                    path.to_string_lossy().to_string(),
                    latest_modified,
                );
                if let Err(e) = snapshot_manager.save(&snapshot_file) {
                    eprintln!("保存快照信息失败: {e}");
                }
                println!("  {} 备份完成: {}", project_name, backup_path.display());
            } else {
                eprintln!("  {project_name} 备份失败");
            }
        }
    }

    if let Err(e) = snapshot_manager.save(&snapshot_file) {
        eprintln!("保存快照信息失败: {e}");
    } else {
        println!("快照信息已保存到: {}", snapshot_file.display());
    }

    println!("\n备份统计:");
    println!("  检查的项目: {processed_count}");
    println!("  执行备份的项目: {backed_up_count}");
    println!("  跳过的项目: {}", processed_count - backed_up_count);
}
