use core::str::FromStr as _;
use strum_macros::EnumString;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TargetOs {
    Linux,
    Macos,
    Windows,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TargetArch {
    X86_64,
    Aarch64,
}

pub struct Target {
    pub llvm_target: &'static str,

    pub pointer_width: u16,
    pub target_os: TargetOs,
    pub target_arch: TargetArch,
}

impl Target {
    pub fn get_current() -> Self {
        let target = env!("BUILD_TARGET");
        let Ok(target) = AvailableTargets::from_str(target) else {
            panic!("unknown target {}", target);
        };
        target.to_target()
    }
    pub fn check_cfg(&self, name: &str, value: &str) -> Option<bool> {
        let expect = match name {
            "pointer_width" => &self.pointer_width.to_string(),
            "target_os" => self.target_os.as_str(),
            "target_arch" => self.target_arch.as_str(),
            _ => return None,
        };
        Some(value == expect)
    }
    pub fn check_cfg_name(&self, name: &str) -> Option<bool> {
        let result = match name {
            "linux" => self.target_os == TargetOs::Linux,
            "macos" => self.target_os == TargetOs::Macos,
            "unix" => matches!(self.target_os, TargetOs::Linux | TargetOs::Macos),
            "windows" => self.target_os == TargetOs::Windows,
            _ => return None,
        };
        Some(result)
    }
}

#[expect(non_camel_case_types)]
#[derive(EnumString, Copy, Clone)]
enum AvailableTargets {
    #[strum(serialize = "x86_64-unknown-linux-gnu")]
    x86_64_unknown_linux_gnu,
    #[strum(serialize = "aarch64-apple-darwin")]
    aarch64_apple_darwin,
    #[strum(serialize = "x86_64-pc-windows-msvc")]
    x86_64_pc_windows_msvc,
}

impl AvailableTargets {
    fn to_target(self) -> Target {
        match self {
            Self::x86_64_unknown_linux_gnu => Target {
                llvm_target: "x86_64-unknown-linux-gnu",
                pointer_width: 64,
                target_os: TargetOs::Linux,
                target_arch: TargetArch::X86_64,
            },
            Self::aarch64_apple_darwin => Target {
                llvm_target: "aarch64-apple-darwin",
                pointer_width: 64,
                target_os: TargetOs::Macos,
                target_arch: TargetArch::Aarch64,
            },
            Self::x86_64_pc_windows_msvc => Target {
                llvm_target: "x86_64-pc-windows-msvc",
                pointer_width: 64,
                target_os: TargetOs::Windows,
                target_arch: TargetArch::X86_64,
            },
        }
    }
}

impl TargetOs {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Linux => "linux",
            Self::Macos => "macos",
            Self::Windows => "windows",
        }
    }
}

impl TargetArch {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64",
            Self::Aarch64 => "aarch64",
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Target;

    #[test]
    fn test_get_current_target() {
        let _target = Target::get_current();
    }
}
