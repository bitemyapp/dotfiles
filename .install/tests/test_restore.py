"""Recovery tests use temporary homes and never change the real desktop."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


INSTALL = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("restore_desktop", INSTALL / "restore-desktop.py")
restore = importlib.util.module_from_spec(spec)
spec.loader.exec_module(restore)


class RestoreTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="desktop-restore-test-")
        self.root = Path(self.temp.name)
        self.home = self.root / "home with spaces"
        self.home.mkdir()
        self.env = patch.dict(os.environ, {"HOME": str(self.home),
            "XDG_CONFIG_HOME": str(self.home / "configuration"),
            "XDG_DATA_HOME": str(self.home / "data"),
            "XDG_STATE_HOME": str(self.home / "state"),
            "XDG_CACHE_HOME": str(self.home / "cache")})
        self.env.start()

    def tearDown(self):
        self.env.stop()
        self.temp.cleanup()

    def test_dry_run_has_no_files_or_downloads(self):
        with patch.object(restore, "urlopen", side_effect=AssertionError("Network in dry run")), \
                patch.object(restore.subprocess, "run", side_effect=AssertionError("Session mutation in dry run")):
            restore.restore_ssh(True)
            restore.restore_plasma(True, None)
            restore.restore_wallpapers(True)
        self.assertEqual(list(self.home.iterdir()), [])

    def test_ssh_preserves_config_and_only_adds_one_include(self):
        config = self.home / ".ssh/config"
        config.parent.mkdir()
        original = "Host example\n    HostName example.org\n"
        config.write_text(original)
        restore.restore_ssh(False)
        once = config.read_text()
        restore.restore_ssh(False)
        self.assertEqual(config.read_text(), once)
        self.assertTrue(once.endswith(original))
        self.assertEqual(once.count("Include config.d/dotfiles-gitlab.conf"), 1)
        fragment = config.parent / "config.d/dotfiles-gitlab.conf"
        self.assertEqual(fragment.stat().st_mode & 0o777, 0o600)
        backups = list((self.home / "state/dotfiles/backups").iterdir())
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_text(), original)

    def test_ssh_preserves_conflicting_alias_and_symlink(self):
        config = self.home / ".ssh/config"
        config.parent.mkdir()
        original = "Host gitlab.com-bitemyapp\n    HostName example.org\n"
        config.write_text(original)
        with self.assertRaisesRegex(RuntimeError, "alias differs"):
            restore.restore_ssh(False)
        self.assertEqual(config.read_text(), original)
        config.unlink()
        target = self.home / "managed-ssh-config"
        target.write_text(original)
        config.symlink_to(target)
        with self.assertRaisesRegex(RuntimeError, "symlink"):
            restore.restore_ssh(False)
        self.assertEqual(target.read_text(), original)

    def test_kconfig_changes_only_preferences_and_is_idempotent(self):
        original = "# user settings\n[General]\nMaxClipItems=10\nCustom=keep\n\n[Other]\nValue=keep\n"
        once = restore.merge_group(original, "General", restore.CLIPBOARD)
        self.assertEqual(restore.merge_group(once, "General", restore.CLIPBOARD), once)
        self.assertIn("Custom=keep", once)
        self.assertTrue(once.endswith("[Other]\nValue=keep\n"))
        self.assertTrue(once.startswith("# user settings\n"))
        self.assertEqual(once.count("MaxClipItems="), 1)

    def fixture(self):
        data = b"test image fixture"
        entry = {"path": "themes/test/backgrounds/1-example.png", "size": len(data),
                 "sha256": hashlib.sha256(data).hexdigest(), "width": 2, "height": 2,
                 "blob": hashlib.sha1(b"blob " + str(len(data)).encode() + b"\0" + data).hexdigest()}
        return {"tag": "v-test", "files": [entry]}, data

    def test_wallpaper_cache_restore_repeat_and_conflict_preservation(self):
        manifest, data = self.fixture()
        cached = self.home / "cache-source" / manifest["files"][0]["path"]
        cached.parent.mkdir(parents=True)
        cached.write_bytes(data)
        with patch.object(restore, "wallpaper_manifest", return_value=manifest), \
                patch.object(restore, "urlopen", side_effect=AssertionError("Cached file downloaded")):
            restore.restore_wallpapers(False, self.home / "cache-source")
            restore.restore_wallpapers(False, self.home / "cache-source")
            package, image = restore.wallpaper_target(self.home / "data/wallpapers", manifest["files"][0])
            self.assertEqual((package / image).read_bytes(), data)
            self.assertEqual(json.loads((package / "metadata.json").read_text())["KPlugin"]["Id"], package.name)
            (package / image).write_bytes(b"user replacement")
            with self.assertRaisesRegex(RuntimeError, "conflicting wallpaper"):
                restore.restore_wallpapers(False, self.home / "cache-source")
            self.assertEqual((package / image).read_bytes(), b"user replacement")

    def test_download_hash_mismatch_does_not_install(self):
        manifest, _ = self.fixture()
        from io import BytesIO
        with patch.object(restore, "wallpaper_manifest", return_value=manifest), \
                patch.object(restore, "urlopen", return_value=BytesIO(b"wrong download")):
            with self.assertRaisesRegex(RuntimeError, "hash mismatch"):
                restore.restore_wallpapers(False)
        self.assertFalse((self.home / "data/wallpapers").exists())

    def test_committed_manifest_covers_original_collection(self):
        manifest = restore.wallpaper_manifest()
        self.assertEqual(len(manifest["files"]), 92)
        self.assertEqual(len({restore.wallpaper_target(Path("wallpapers"), entry)[0]
                              for entry in manifest["files"]}), 92)
        for entry in manifest["files"]:
            self.assertNotIn("local_source", entry)
            self.assertGreater(entry["width"], 0)
            self.assertGreater(entry["height"], 0)

    @unittest.skipUnless(shutil.which("node"), "Node required for isolated Plasma API mock")
    def test_panel_repeat_run_keeps_widgets_and_launchers(self):
        result = subprocess.run(["node", str(INSTALL / "tests/panel-mock.cjs"),
                                 str(INSTALL / "desktop/panel.js")], check=True,
                                text=True, capture_output=True)
        self.assertIn("Panel restore checks passed", result.stdout)
