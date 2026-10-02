#!/usr/bin/env python3
"""Restore chosen desktop settings; never export a user's configuration."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import re
import shlex
import shutil
import subprocess
import tempfile
from urllib.parse import quote
from urllib.request import Request, urlopen


ASSETS = Path(__file__).resolve().parent / "desktop"
CLIPBOARD = {"IgnoreImages": "false", "IgnoreSelection": "true",
             "KeepClipboardContents": "true", "MaxClipItems": "100"}
APPS = ["systemsettings.desktop", "google-chrome.desktop",
        "org.telegram.desktop.desktop", "dev.zed.Zed.desktop", "chatgpt.desktop",
        "com.anthropic.Claude.desktop", "com.mitchellh.ghostty.desktop"]


def xdg(kind, default):
    return Path(os.environ.get("XDG_" + kind + "_HOME") or Path.home() / default)


def regular(path):
    if path.is_symlink() or (path.exists() and not path.is_file()):
        raise RuntimeError(f"Preserving symlink or non-file: {path}")


def backup(path):
    regular(path)
    if not path.exists():
        return
    data = path.read_bytes()
    root = xdg("STATE", ".local/state") / "dotfiles/backups"
    root.mkdir(parents=True, exist_ok=True, mode=0o700)
    name = path.name + "." + hashlib.sha256(str(path).encode() + data).hexdigest() + ".bak"
    dest = root / name
    if not dest.exists():
        with dest.open("xb") as stream:
            os.chmod(dest, 0o600)
            stream.write(data)
    print(f"Local backup: {dest}")


def write_settings(path, text, dry_run):
    regular(path)
    if path.exists() and path.read_text() == text:
        print(f"Already configured: {path}")
        return
    print(f"{'Would update' if dry_run else 'Update'}: {path}")
    if dry_run:
        return
    backup(path)
    mode = path.stat().st_mode & 0o777 if path.exists() else 0o600
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(dir=path.parent, prefix=".dotfiles-")
    try:
        with os.fdopen(fd, "w") as stream:
            os.chmod(temporary, mode)
            stream.write(text)
        os.replace(temporary, path)
    finally:
        Path(temporary).unlink(missing_ok=True)


def merge_group(text, group, values):
    """Change only these keys, retaining comments and other KConfig groups."""
    lines = text.splitlines()
    header = f"[{group}]"
    starts = [i for i, line in enumerate(lines) if line == header]
    if len(starts) > 1:
        raise RuntimeError(f"Duplicate {header} groups; preserving file")
    if not starts:
        return text.rstrip() + ("\n\n" if text.strip() else "") + header + "\n" + "".join(
            f"{key}={value}\n" for key, value in values.items())
    start = starts[0] + 1
    end = next((i for i in range(start, len(lines)) if lines[i].startswith("[")), len(lines))
    remaining = dict(values)
    result = []
    for line in lines[start:end]:
        key = line.split("=", 1)[0]
        if key in values:
            if key in remaining:
                result.append(f"{key}={remaining.pop(key)}")
        else:
            result.append(line)
    result += [f"{key}={value}" for key, value in remaining.items()]
    return "\n".join(lines[:start] + result + lines[end:]) + "\n"


def restore_ssh(dry_run):
    root = Path.home() / ".ssh"
    config = root / "config"
    regular(config)
    text = config.read_text() if config.exists() else ""
    fragment = (ASSETS / "gitlab-ssh.conf").read_text()
    # The original installation already has this complete, specific stanza.
    if fragment.strip() in text:
        print("GitLab alias already configured")
        return
    for line in text.splitlines():
        words = shlex.split(line, comments=True)
        if words and words[0].lower() == "host" and "gitlab.com-bitemyapp" in words[1:]:
            raise RuntimeError("Existing GitLab alias differs; preserving it. Compare desktop/gitlab-ssh.conf.")
    include = "Include config.d/dotfiles-gitlab.conf"
    target = root / "config.d/dotfiles-gitlab.conf"
    regular(target)
    if target.exists() and target.read_text() != fragment:
        raise RuntimeError(f"Preserving conflicting SSH fragment: {target}")
    if not dry_run:
        root.mkdir(parents=True, exist_ok=True, mode=0o700)
    write_settings(target, fragment, dry_run)
    if include not in text.splitlines():
        write_settings(config, "# dotfiles: GitLab SSH alias\n" + include + "\n\n" + text, dry_run)


def panel_script(panel_id):
    data = xdg("DATA", ".local/share")
    dirs = [data] + [Path(p) for p in os.environ.get("XDG_DATA_DIRS", "/usr/local/share:/usr/share").split(":") if p]
    launchers = ["applications:" + app for app in APPS if any((d / "applications" / app).is_file() for d in dirs)]
    launchers.insert(1 if launchers else 0, "preferred://filemanager")
    options = {"panelId": panel_id, "launchers": launchers}
    return "var dotfilesOptions = " + json.dumps(options) + ";\n" + (ASSETS / "panel.js").read_text()


def restore_plasma(dry_run, panel_id):
    config = xdg("CONFIG", ".config")
    if not dry_run:
        if not shutil.which("qdbus6"):
            raise RuntimeError("qdbus6 required; install plasma-workspace and qt6-tools first")
        # Check the session before writing any settings; never restart Plasma/KWin.
        subprocess.run(["qdbus6", "org.kde.plasmashell", "/PlasmaShell"],
                       check=True, stdout=subprocess.DEVNULL)
    clip = config / "klipperrc"
    regular(clip)
    text = clip.read_text() if clip.exists() else ""
    script = panel_script(panel_id)
    print("Restore 36px floating bottom panel, battery percentage, Bluetooth, clipboard and notifications")
    if dry_run:
        write_settings(clip, merge_group(text, "General", CLIPBOARD), True)
        print("Would apply desktop/panel.js in the logged-in Plasma session (no restart)")
        return
    for name in ("plasma-org.kde.plasma.desktop-appletsrc", "plasmashellrc"):
        backup(config / name)
    result = subprocess.run(["qdbus6", "org.kde.plasmashell", "/PlasmaShell",
                             "org.kde.PlasmaShell.evaluateScript", script],
                            check=True, text=True, capture_output=True)
    if "Restored bottom panel " not in result.stdout:
        raise RuntimeError("Panel script did not complete: " + result.stdout + result.stderr)
    print(result.stdout.strip())
    write_settings(clip, merge_group(text, "General", CLIPBOARD), False)
    print("Clipboard preferences take effect on the next login; no history was read or changed")


def wallpaper_manifest():
    manifest = json.loads((ASSETS / "omarchy-wallpapers.json").read_text())
    for entry in manifest["files"]:
        path = PurePosixPath(entry["path"])
        if (path.is_absolute() or ".." in path.parts or len(path.parts) != 4
                or path.parts[0] != "themes" or path.parts[2] != "backgrounds"
                or path.suffix.lower() not in (".png", ".jpg", ".jpeg", ".webp")
                or not re.fullmatch(r"[a-f0-9]{64}", entry["sha256"])
                or not re.fullmatch(r"[a-f0-9]{40}", entry["blob"])
                or not 0 < entry["size"] < 50_000_000):
            raise RuntimeError("Invalid wallpaper manifest entry")
    return manifest


def image_matches(path, entry):
    return path.is_file() and path.stat().st_size == entry["size"] and hashlib.sha256(path.read_bytes()).hexdigest() == entry["sha256"]


def wallpaper_target(root, entry):
    path = PurePosixPath(entry["path"])
    slug = re.sub(r"[^a-z0-9]+", "-", path.stem.lower()).strip("-")[:100]
    package_id = f"omarchy-{path.parts[1]}-{slug}-{entry['blob'][:8]}"
    package = root / package_id
    image = Path("contents/images") / f"{entry['width']}x{entry['height']}{path.suffix.lower()}"
    return package, image


def restore_wallpapers(dry_run, cache_dir=None):
    manifest = wallpaper_manifest()
    destination = xdg("DATA", ".local/share") / "wallpapers"
    cache = cache_dir or xdg("CACHE", ".cache") / "dotfiles/omarchy" / manifest["tag"]
    print(f"{'Would restore' if dry_run else 'Restore'} {len(manifest['files'])} original Omarchy wallpapers to {destination}")
    # Preflight every destination before any downloads or installation.
    missing = []
    for entry in manifest["files"]:
        package, image = wallpaper_target(destination, entry)
        if package.is_symlink() or (package.exists() and not image_matches(package / image, entry)):
            raise RuntimeError(f"Preserving conflicting wallpaper: {package}")
        if not package.exists():
            missing.append(entry)
    if dry_run:
        print(f"{len(missing)} missing; downloads are checked against committed SHA-256 and Git blob hashes")
        return
    if not missing:
        print("All wallpapers already installed and verified")
        return

    def download(entry):
        target = cache / entry["path"]
        if image_matches(target, entry):
            return target
        regular(target)
        url = "https://raw.githubusercontent.com/omacom/omarchy/" + quote(manifest["tag"], safe="") + "/" + quote(entry["path"], safe="/")
        request = Request(url, headers={"User-Agent": "dotfiles-wallpaper-restore"})
        with urlopen(request, timeout=45) as response:
            data = response.read(entry["size"] + 1)
        blob = hashlib.sha1(b"blob " + str(len(data)).encode() + b"\0" + data).hexdigest()
        if len(data) != entry["size"] or blob != entry["blob"] or hashlib.sha256(data).hexdigest() != entry["sha256"]:
            raise RuntimeError(f"Wallpaper hash mismatch: {entry['path']}")
        target.parent.mkdir(parents=True, exist_ok=True)
        fd, temporary = tempfile.mkstemp(dir=target.parent, prefix=".download-")
        try:
            with os.fdopen(fd, "wb") as stream:
                stream.write(data)
            os.replace(temporary, target)
        finally:
            Path(temporary).unlink(missing_ok=True)
        return target

    with ThreadPoolExecutor(max_workers=6) as pool:
        sources = list(pool.map(download, missing))
    destination.mkdir(parents=True, exist_ok=True)
    for entry, source in zip(missing, sources):
        package, image = wallpaper_target(destination, entry)
        path = PurePosixPath(entry["path"])
        title = re.sub(r"^\d+[\s._-]+", "", path.stem).replace("_", " ").replace("-", " ")
        metadata = {"KPackageStructure": "Wallpaper/Images", "KPlugin": {
            "Id": package.name, "Name": f"Omarchy · {path.parts[1].replace('-', ' ').title()} · {title}",
            "Description": f"Original Omarchy {manifest['tag']} wallpaper: {entry['path']}",
            "Website": "https://github.com/omacom/omarchy"},
            "X-Omarchy-Source": {"Release": manifest["tag"], "Path": entry["path"], "SHA256": entry["sha256"]}}
        with tempfile.TemporaryDirectory(dir=destination, prefix=".dotfiles-") as stage:
            staging = Path(stage) / package.name
            (staging / image).parent.mkdir(parents=True)
            shutil.copyfile(source, staging / image)
            (staging / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n")
            staging.rename(package)
    print(f"Installed {len(missing)} verified KDE wallpaper packages; reopen the wallpaper selector")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--component", action="append", choices=("ssh", "plasma", "wallpapers"))
    parser.add_argument("--all", action="store_true", help="restore all three components")
    parser.add_argument("--dry-run", action="store_true", help="no writes, downloads or session changes")
    parser.add_argument("--panel-id", type=int, help="select one panel when there are multiple bottom panels")
    parser.add_argument("--cache-dir", type=Path, help="existing wallpaper download cache")
    args = parser.parse_args()
    components = ("ssh", "plasma", "wallpapers") if args.all else args.component
    if not components:
        parser.error("choose --all or --component (preview with --dry-run)")
    if os.geteuid() == 0:
        parser.error("run as your desktop user, not root")
    try:
        for component in dict.fromkeys(components):
            if component == "ssh": restore_ssh(args.dry_run)
            elif component == "plasma": restore_plasma(args.dry_run, args.panel_id)
            else: restore_wallpapers(args.dry_run, args.cache_dir)
    except (OSError, ValueError, RuntimeError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"Restore stopped: {error}\n")


if __name__ == "__main__":
    main()
