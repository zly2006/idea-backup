use clap::Parser;
use dotenv::dotenv;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

mod ensure_7z;
use ensure_7z::ensure_7z_installed;

#[derive(Parser)]
#[command(name = "7z-extractor")]
#[command(about = "A CLI tool to extract 7z files with password support")]
struct Args {
    /// Input path: single 7z file or directory containing 7z files
    #[arg(value_name = "INPUT")]
    input: PathBuf,

    /// Password for 7z extraction (overrides .env file)
    #[arg(short, long)]
    password: Option<String>,

    /// Output directory for extracted files
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn extract_7z_file(
    file_path: &PathBuf,
    output_dir: &Path,
    password: &str,
) -> Result<(), String> {
    if file_path.extension().is_none_or(|ext| ext != "7z") {
        return Err(format!("{} 不是7z文件", file_path.display()));
    }

    // 为每个7z文件创建单独的目录
    let file_stem = file_path.file_stem().unwrap().to_string_lossy();
    let dir_name = if file_stem.ends_with("_backup") {
        file_stem.trim_end_matches("_backup").to_string()
    } else {
        file_stem.to_string()
    };
    let final_output_dir = output_dir.join(dir_name);

    // 创建输出目录
    if !final_output_dir.exists() {
        fs::create_dir_all(&final_output_dir)
            .map_err(|e| format!("创建目录失败 {}: {}", final_output_dir.display(), e))?;
    }

    println!(
        "正在解压: {} -> {}",
        file_path.display(),
        final_output_dir.display()
    );

    // 执行7z解压命令
    let status = Command::new("7z")
        .arg("x") // 解压保持目录结构
        .arg(file_path)
        .arg(format!("-p{password}"))
        .arg(format!("-o{}", final_output_dir.display()))
        .arg("-y") // 覆盖所有文件
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .status()
        .map_err(|e| format!("执行7z命令失败: {e}"))?;

    if status.success() {
        println!(
            "✅ 解压成功: {}",
            file_path.file_name().unwrap().to_string_lossy()
        );
        Ok(())
    } else {
        Err(format!(
            "❌ 解压失败: {}",
            file_path.file_name().unwrap().to_string_lossy()
        ))
    }
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

    // 获取输出目录：优先使用命令行参数，其次使用当前目录
    let output_dir = args
        .output
        .unwrap_or_else(|| std::env::current_dir().expect("无法获取当前目录"));

    // 创建输出目录（如果不存在）
    if !output_dir.exists() {
        fs::create_dir_all(&output_dir).expect("无法创建输出目录");
        println!("已创建输出目录: {}", output_dir.display());
    }

    if !args.input.exists() {
        eprintln!("❌ 输入路径不存在: {}", args.input.display());
        std::process::exit(1);
    }

    let mut success_count = 0;
    let mut error_count = 0;

    if args.input.is_file() {
        // 解压单个文件
        match extract_7z_file(&args.input, &output_dir, &password) {
            Ok(()) => success_count += 1,
            Err(e) => {
                eprintln!("{e}");
                error_count += 1;
            }
        }
    } else if args.input.is_dir() {
        // 批量解压目录中的所有7z文件
        let entries = fs::read_dir(&args.input).expect("无法读取目录");
        let mut found_7z = false;

        for entry in entries {
            let entry = entry.expect("读取目录项失败");
            let path = entry.path();

            if path.is_file() && path.extension().is_some_and(|ext| ext == "7z") {
                found_7z = true;
                match extract_7z_file(&path, &output_dir, &password) {
                    Ok(()) => success_count += 1,
                    Err(e) => {
                        eprintln!("{e}");
                        error_count += 1;
                    }
                }
            }
        }

        if !found_7z {
            eprintln!("❌ 在目录 {} 中未找到任何7z文件", args.input.display());
            std::process::exit(1);
        }
    } else {
        eprintln!("❌ 输入路径既不是文件也不是目录: {}", args.input.display());
        std::process::exit(1);
    }

    println!("\n📊 解压完成:");
    println!("   成功: {success_count} 个文件");
    if error_count > 0 {
        println!("   失败: {error_count} 个文件");
    }
}
