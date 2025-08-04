# Idea Backup

一个用于自动备份项目的 Rust 命令行工具，支持智能快照管理和增量备份。

## 特性

- 🚀 **智能快照管理** - 只备份有变化的项目，避免重复备份
- 📁 **自定义输入目录** - 支持指定任意项目目录，默认为 `~/IdeaProjects`
- 🔒 **7z 加密压缩** - 使用密码保护的 7z 格式压缩备份文件
- 🙈 **完整 .gitignore 支持** - 自动解析多层级 .gitignore 文件，支持 `**` 通配符
- 📦 **包含 Git 历史** - 可选择备份完整的 .git 目录和历史记录
- 🔧 **路径扩展** - 支持 `~` 和 `.` 字符的路径解析
- 📊 **详细统计** - 显示备份过程和结果统计

## 安装

### 从源码构建

```bash
git clone https://github.com/your-username/idea-backup.git
cd idea-backup
cargo build --release
```

编译后的可执行文件位于 `target/release/idea-backup`

### 系统要求

- Rust 1.70+
- 7z 工具（macOS 通过 Homebrew 自动安装）

## 使用方法

### 基本用法

```bash
# 使用默认设置备份 ~/IdeaProjects 目录
./idea-backup -p your_password

# 指定输入和输出目录
./idea-backup -p your_password -i ~/MyProjects -o ~/Backups

# 强制备份所有项目（忽略快照检查）
./idea-backup -p your_password --force
```

### 命令行参数

```
USAGE:
    idea-backup [OPTIONS]

OPTIONS:
    -p, --password <PASSWORD>    压缩文件的密码（优先于 .env 文件）
    -i, --input <INPUT>          要扫描的项目目录 [default: ~/IdeaProjects]
    -o, --output <OUTPUT>        备份文件的输出目录
    -f, --force                  强制备份所有项目，忽略变更检测
    -h, --help                   显示帮助信息
```

### 环境变量配置

你可以创建 `.env` 文件来配置默认密码：

```bash
# .env 文件
ZIP_PASSWORD=your_secure_password
```

## 工作原理

### 快照管理

工具会在输出目录创建 `.backup_snapshots.json` 文件来跟踪每个项目的状态：

```json
{
  "projects": {
    "my-project": {
      "path": "/Users/username/IdeaProjects/my-project",
      "last_modified": 1672531200,
      "last_backup": 1672531200
    }
  }
}
```

### 智能备份逻辑

1. **首次运行** - 备份所有发现的项目
2. **后续运行** - 只备份有文件变更的项目
3. **变更检测** - 遍历项目文件，比较最新修改时间
4. **增量优化** - 快速检查避免不必要的完整遍历

### .gitignore 支持

- ✅ 支持多层级 .gitignore 文件
- ✅ 支持标准 gitignore 语法（`*`, `**`, `!`, `/`）
- ✅ 支持否定规则 (`!important.file`)
- ✅ 支持目录专用规则 (`build/`)
- ✅ 支持绝对路径规则 (`/root-only`)

## 输出文件

每个项目会生成一个加密的 7z 文件：

```
~/IdeaProjects/
├── my-project/
└── backups/
    ├── my-project_backup.7z
    ├── another-project_backup.7z
    └── .backup_snapshots.json
```

## 解压工具

项目包含一个配套的解压工具：

```bash
# 解压备份文件
./7z-extractor path/to/backup.7z output_directory
```

## 开发

### 运行测试

```bash
cargo test
```

### 代码检查

```bash
cargo clippy --fix --allow-dirty
```

### 格式化代码

```bash
cargo fmt
```

## 贡献

1. Fork 这个仓库
2. 创建你的特性分支 (`git checkout -b feature/amazing-feature`)
3. 提交你的更改 (`git commit -m 'Add some amazing feature'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 打开一个 Pull Request

## 许可证

MIT License - 查看 [LICENSE](LICENSE) 文件了解详情

## 更新日志

### v0.1.0
- 初始版本
- 智能快照管理
- 完整 .gitignore 支持
- 7z 加密压缩
- 路径扩展支持

## 技术栈

- **Rust** - 系统编程语言
- **clap** - 命令行参数解析
- **serde** - 序列化/反序列化
- **7z** - 压缩工具
