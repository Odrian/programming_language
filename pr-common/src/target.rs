use std::str::FromStr;
use strum_macros::EnumString;

pub struct Target {
    pub llvm_target: &'static str,

    pub pointer_width: u16,
    pub target_os: &'static str,
    pub target_arch: &'static str,
}

impl Target {
    pub fn get_current() -> Target {
        let target = env!("BUILD_TARGET");
        let Ok(target) = AvailableTargets::from_str(target) else {
            panic!("unknown target {}", target);
        };
        target.to_target()
    }
    pub fn check_cfg(&self, name: &str, value: &str) -> Option<bool> {
        let expect = match name {
            "pointer_width" => &self.pointer_width.to_string(),

            "target_os" => self.target_os,

            "target_arch" => self.target_arch,
            _ => return None
        };
        Some(value == expect)
    }
    pub fn check_cfg_name(&self, name: &str) -> Option<bool> {
        let result = match name {
            "linux" => self.target_os == "linux",
            "macos" => self.target_os == "macos",
            "windows" => self.target_os == "windows",
            _ => return None
        };
        Some(result)
    }

}

#[allow(non_camel_case_types)]
#[derive(EnumString)]
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
            AvailableTargets::x86_64_unknown_linux_gnu => Target {
                llvm_target: "x86_64-unknown-linux-gnu",
                pointer_width: 64,
                target_os: "linux",
                target_arch: "x86_64",
            },
            AvailableTargets::aarch64_apple_darwin => Target {
                llvm_target: "aarch64-apple-darwin",
                pointer_width: 64,
                target_os: "macos",
                target_arch: "aarch64",
            },
            AvailableTargets::x86_64_pc_windows_msvc => Target {
                llvm_target: "x86_64-pc-windows-msvc",
                pointer_width: 64,
                target_os: "windows",
                target_arch: "x86_64",
            },
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
