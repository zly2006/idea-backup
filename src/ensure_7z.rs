use std::process::Command;

pub fn ensure_7z_installed() {
    let output = Command::new("which")
        .arg("7z")
        .output()
        .expect("failed to execute process");
    if output.status.success() {
        println!("7z 已安装");
    } else {
        println!("正在通过 brew 安装 7z...");
        let status = Command::new("brew")
            .arg("install")
            .arg("p7zip")
            .status()
            .expect("failed to install p7zip");
        if !status.success() {
            panic!("brew 安装 7z 失败");
        }
    }
}
