"""Exercise the Arch installer without host package or login-shell changes."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


REPO = Path(__file__).resolve().parents[2]
SHELL_PACKAGES = ["fish", "zsh", "starship"]
CORE_PACKAGES = SHELL_PACKAGES + [
    "git", "base-devel", "clang", "cmake", "curl", "unzip", "ca-certificates",
    "openssl", "pkgconf", "gnupg", "procps-ng", "mosh", "tmux", "screen",
    "htop", "btop", "colordiff", "emacs", "ripgrep", "fd", "tokei", "just",
    "rink", "difftastic", "mergiraf", "rust", "fontconfig", "ttf-roboto",
    "ttf-anonymous-pro", "ttf-firacode-nerd",
]

MOCK = r'''#!/usr/bin/python3
import json, os, pathlib, subprocess, sys
name = pathlib.Path(sys.argv[0]).name
args = sys.argv[1:]
state_path = pathlib.Path(os.environ["MOCK_STATE"])
state = json.loads(state_path.read_text())
with open(os.environ["MOCK_LOG"], "a") as log:
    log.write(json.dumps([name, *args]) + "\n")
if name == "pacman":
    if args[0] == "-T":
        sys.exit(0 if args[1] in state["installed"] + state.get("providers", []) else 127)
    if args[0] == "-Si":
        sys.exit(0 if args[1] in state["available"] else 1)
    if args[0] != "-S" or args[1:3] != ["--needed", "--"]:
        sys.exit("Unexpected pacman mutation: " + repr(args))
    if state.get("fail_install"):
        sys.exit(1)
    state["installed"] += args[3:]
elif name == "sudo":
    if not args or args[0] != "pacman":
        sys.exit("Unexpected sudo command: " + repr(args))
    sys.exit(subprocess.call(args))
elif name in ("yay", "paru"):
    if args[:3] != ["-S", "--needed", "--"]:
        sys.exit("Unexpected AUR mutation: " + repr(args))
    state["installed"] += args[3:]
elif name == "getent":
    print("test:x:1000:1000:Test:" + os.environ["HOME"] + ":" + state["shell"])
elif name == "chsh":
    state["shell"] = args[1]
elif name == "starship":
    if args != ["init", "fish"] and args != ["init", "zsh"]:
        sys.exit("Unexpected Starship invocation")
    if args[1] == "fish":
        print("function fish_prompt; printf 'starship-test>'; end")
    else:
        print("PROMPT='starship-test>'")
else:
    sys.exit("Legacy installer command was invoked: " + name)
state_path.write_text(json.dumps(state))
'''


class ArchInstallerTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="dotfiles-test-")
        self.root = Path(self.temp.name)
        self.home = self.root / "home with spaces"
        self.home.mkdir()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.state_path = self.root / "state.json"
        self.log_path = self.root / "calls.jsonl"
        self.release = self.root / "os-release"
        self.release.write_text('ID=cachyos\nID_LIKE="arch linux"\n')
        self.state = {"installed": CORE_PACKAGES.copy(), "available": CORE_PACKAGES.copy(),
                      "shell": shutil.which("fish") or "/bin/fish"}
        self.save_state()
        for command in ["pacman", "sudo", "yay", "chsh", "getent", "apt", "apt-get", "curl", "wget", "cargo"]:
            self.mock(command)
        # Keep package detection independent of software installed on the host.
        # In particular these tests must still work AFTER Starship is installed.
        for command in ["bash", "cat", "dirname", "id", "mkdir", "ln", "readlink", "grep", "cut", "fish", "zsh"]:
            executable = shutil.which(command)
            if executable:
                (self.bin / command).symlink_to(executable)
        self.env = os.environ.copy()
        for key in ["XDG_CONFIG_HOME", "ZDOTDIR", "STARSHIP_CONFIG", "EDITOR", "GIT_EDITOR", "CARGO_HOME"]:
            self.env.pop(key, None)
        self.env.update(HOME=str(self.home), PATH=str(self.bin),
                        MOCK_STATE=str(self.state_path), MOCK_LOG=str(self.log_path),
                        DOTFILES_OS_RELEASE=str(self.release), TERM="xterm-ghostty")

    def tearDown(self):
        self.temp.cleanup()

    def mock(self, command):
        path = self.bin / command
        path.write_text(MOCK)
        path.chmod(0o755)

    def save_state(self):
        self.state_path.write_text(json.dumps(self.state))

    def calls(self):
        if not self.log_path.exists():
            return []
        return [json.loads(line) for line in self.log_path.read_text().splitlines()]

    def mutations(self):
        return [call for call in self.calls()
                if call[0] in ["sudo", "yay", "paru", "chsh"] or call[:2] == ["pacman", "-S"]]

    def run_install(self, *args, script="install.sh", ok=True):
        result = subprocess.run(["bash", str(REPO / ".install" / script), *args],
                                env=self.env, cwd=self.root, text=True, capture_output=True)
        if ok:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def write_home(self, name, text):
        path = self.home / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def test_repeat_run_preserves_apps_and_adds_hooks_once_after_distro_config(self):
        self.state["installed"] += ["ghostty", "google-chrome", "chatgpt-desktop", "claude-desktop"]
        self.save_state()
        fish = self.write_home(".config/fish/config.fish", "# distro\nif true\n    true\nend\n")
        zsh = self.write_home(".zshrc", "# distro zsh\n")
        protected = [self.write_home(name, "user-config\n") for name in [
            ".config/ghostty/config", ".config/niri/config.kdl", ".config/starship.toml",
            ".local/share/applications/google-chrome.desktop", ".secrets", ".gitconfig"]]
        self.run_install()
        first_fish, first_zsh = fish.read_text(), zsh.read_text()
        self.run_install()
        self.assertEqual(fish.read_text(), first_fish)
        self.assertEqual(zsh.read_text(), first_zsh)
        self.assertEqual(first_fish.count('source "$__fish_config_dir/../dotfiles/shell.fish"'), 1)
        self.assertTrue(first_fish.startswith("# distro\nif true\n    true\nend\n"))
        self.assertTrue(first_zsh.startswith("# distro zsh\n"))
        for path in protected:
            self.assertEqual(path.read_text(), "user-config\n")
        self.assertEqual(self.mutations(), [])
        self.assertTrue((self.home / ".config/dotfiles/shell.fish").is_symlink())

    def test_missing_packages_installed_once_without_upgrading_existing_targets(self):
        self.state["installed"].remove("starship")
        self.save_state()
        self.run_install("--component", "shells")
        self.run_install("--component", "shells")
        transactions = [call for call in self.calls() if call[:2] == ["pacman", "-S"]]
        self.assertEqual(transactions, [["pacman", "-S", "--needed", "--", "starship"]])

    def test_provider_satisfies_rust_without_replacing_rustup(self):
        self.state["installed"].remove("rust")
        self.state["installed"].append("rustup")
        self.state["providers"] = ["rust"]
        self.save_state()
        self.run_install(script="rust.sh")
        self.assertEqual(self.mutations(), [])

    def test_dry_run_creates_nothing_and_does_not_change_shell(self):
        self.state["installed"].remove("starship")
        self.state["shell"] = shutil.which("zsh") or "/bin/zsh"
        self.save_state()
        result = self.run_install("--dry-run", "--shell", "fish")
        self.assertIn("Dry run complete; no changes made.", result.stdout)
        self.assertEqual(list(self.home.iterdir()), [])
        self.assertEqual(self.mutations(), [])

    def test_custom_config_and_zdotdir(self):
        self.env["XDG_CONFIG_HOME"] = str(self.home / "config with spaces")
        self.env["ZDOTDIR"] = str(self.home / "zsh config")
        self.run_install("--component", "shells")
        self.assertTrue((Path(self.env["XDG_CONFIG_HOME"]) / "starship.toml").is_symlink())
        self.assertTrue((Path(self.env["ZDOTDIR"]) / ".zshrc").exists())
        self.assertFalse((self.home / ".zshrc").exists())
        self.mock("starship")
        for shell in ["fish", "zsh"]:
            executable = shutil.which(shell)
            if not executable:
                continue
            result = subprocess.run([executable, "-i", "-c", 'printf "%s\\n" "$STARSHIP_CONFIG"'],
                                    env=self.env, cwd=self.root, text=True, capture_output=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn(str(Path(self.env["XDG_CONFIG_HOME"]) / "starship.toml"), result.stdout)

    def test_existing_startup_symlinks_are_not_edited(self):
        target = self.root / "managed-zshrc"
        target.write_text("# externally managed\n")
        (self.home / ".zshrc").symlink_to(target)
        result = self.run_install("--component", "shells")
        self.assertEqual(target.read_text(), "# externally managed\n")
        self.assertIn("Keeping symlink:", result.stdout)

    def test_dangling_config_symlink_is_preserved(self):
        target = self.home / ".config/starship.toml"
        target.parent.mkdir()
        target.symlink_to(self.root / "not-created")
        self.run_install("--component", "shells")
        self.assertEqual(target.readlink(), self.root / "not-created")

    def test_package_failure_stops_before_shell_configuration(self):
        self.state["installed"].remove("starship")
        self.state["fail_install"] = True
        self.save_state()
        self.run_install("--component", "shells", ok=False)
        self.assertEqual(list(self.home.iterdir()), [])

    def test_missing_aur_package_requires_opt_in_before_any_transaction(self):
        self.run_install("--component", "shells", "--component", "google-chrome", ok=False)
        self.assertEqual(self.mutations(), [])
        self.assertEqual(list(self.home.iterdir()), [])
        self.run_install("--component", "google-chrome", "--aur")
        self.run_install("--component", "google-chrome", "--aur")
        self.assertEqual([call for call in self.calls() if call[0] == "yay"],
                         [["yay", "-S", "--needed", "--", "google-chrome"]])

    def test_existing_application_executable_preserves_alternate_package(self):
        chrome = self.home / ".local/bin/google-chrome"
        chrome.parent.mkdir(parents=True)
        chrome.write_text("#!/bin/sh\nexit 0\n")
        chrome.chmod(0o755)
        self.run_install(script="google-chrome.sh")
        self.assertEqual(self.mutations(), [])

    def test_all_linux_entry_points_dispatch_without_legacy_downloads(self):
        packages = ["docker", "docker-buildx", "docker-compose", "ghostty", "google-chrome",
                    "spotify", "telegram-desktop", "signal-desktop", "cursor-bin", "claude-code",
                    "openai-codex-bin", "github-cli", "nodejs", "npm", "opencode", "niri", "fuzzel", "swaylock",
                    "visual-studio-code-bin", "vscodium-bin", "slack-desktop", "cuda", "cudnn",
                    "nccl", "xsecurelock"]
        self.state["installed"] += packages
        self.save_state()
        for script in ["apt-packages", "rust", "fonts", "docker", "ghostty", "google-chrome",
                       "spotify", "telegram", "signal", "cursor", "claude", "codex", "github",
                       "node", "opencode", "install-niri", "vscode", "vscodium", "slack", "cuda",
                       "xsecurelock"]:
            with self.subTest(script=script):
                self.run_install(script=f"{script}.sh")
        self.run_install("--configure-only", script="spotify.sh")
        self.run_install(script="selenium.sh", ok=False)
        self.assertEqual(self.mutations(), [])
        self.assertFalse(any(call[0] in ["apt", "apt-get", "curl", "wget", "cargo"] for call in self.calls()))

    def test_invalid_options_fail_before_changes(self):
        for args in [("--shell", "bash"), ("--shell",), ("--component", "unknown"), ("--unknown",)]:
            with self.subTest(args=args):
                self.run_install(*args, ok=False)
        self.assertEqual(self.mutations(), [])
        self.assertIn("Usage:", self.run_install("--help").stdout)

    def test_arch_detection_rejects_unrelated_distribution(self):
        self.release.write_text('ID=ubuntu\nID_LIKE=debian\n')
        self.run_install(script="arch.sh", ok=False)
        self.assertEqual(self.calls(), [])

    @unittest.skipUnless(shutil.which("fish") and shutil.which("zsh"), "fish and zsh required")
    def test_login_shell_change_is_idempotent_and_uses_registered_shell(self):
        self.state["shell"] = shutil.which("zsh")
        self.save_state()
        self.run_install("--component", "shells", "--shell", "fish")
        self.run_install("--component", "shells", "--shell", "fish")
        self.assertEqual(len([call for call in self.calls() if call[0] == "chsh"]), 1)

    @unittest.skipUnless(shutil.which("fish") and shutil.which("zsh"), "fish and zsh required")
    def test_shell_startup_with_starship_and_repeated_sources(self):
        self.mock("starship")
        self.run_install("--component", "shells")
        (self.home / ".cargo/bin").mkdir(parents=True)
        for shell, command in [
            ("fish", 'source "$__fish_config_dir/../dotfiles/shell.fish"; fish_prompt; printf "\\n%s\\n" $TERM; printf "%s\\n" $PATH'),
            ("zsh", 'source "$HOME/.config/dotfiles/shell.zsh"; print -r -- "$PROMPT" "$TERM"; print -rl -- $path'),
        ]:
            with self.subTest(shell=shell):
                result = subprocess.run([shell, "-i", "-c", command], env=self.env, cwd=self.root,
                                        text=True, capture_output=True)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("starship-test>", result.stdout)
                self.assertIn("xterm-ghostty", result.stdout)
                self.assertEqual(result.stdout.count(str(self.home / ".cargo/bin")), 1)
                self.assertEqual([call for call in self.calls() if call == ["starship", "init", shell]],
                                 [["starship", "init", shell]])

    @unittest.skipUnless(shutil.which("fish") and shutil.which("zsh"), "fish and zsh required")
    def test_starship_takes_over_from_powerlevel10k(self):
        self.mock("starship")
        self.write_home(".zshrc", '''
precmd_functions=(_p9k_precmd keep_user_hook)
prompt_powerlevel9k_teardown() {
    precmd_functions=(${precmd_functions:#_p9k_precmd})
    PROMPT='default>'
}
''')
        self.run_install("--component", "shells")
        result = subprocess.run(["zsh", "-i", "-c", 'print -rl -- "$PROMPT" $precmd_functions'],
                                env=self.env, cwd=self.root, text=True, capture_output=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("starship-test>", result.stdout)
        self.assertIn("keep_user_hook", result.stdout)
        self.assertNotIn("_p9k_precmd", result.stdout)

    @unittest.skipUnless(shutil.which("fish") and shutil.which("zsh"), "fish and zsh required")
    def test_shell_snippets_work_without_starship_or_cargo_env(self):
        # An isolated PATH prevents this test from depending on installed tools.
        self.run_install("--component", "shells")
        empty_bin = self.root / "empty-bin"
        empty_bin.mkdir()
        shell_env = self.env | {"PATH": str(empty_bin)}
        for shell in ["fish", "zsh"]:
            command = f'source "{REPO}/.config/dotfiles/shell.{shell}"'
            command += '; printf "startup-ok\\n"' if shell == "fish" else '; print startup-ok'
            result = subprocess.run([shutil.which(shell), "--no-config" if shell == "fish" else "-f", "-i", "-c", command],
                                    env=shell_env, cwd=self.root, text=True, capture_output=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("startup-ok", result.stdout)
            self.assertNotIn("command not found", result.stderr)


if __name__ == "__main__":
    unittest.main()
