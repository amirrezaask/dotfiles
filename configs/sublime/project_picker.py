"""
Sublime Text Project Picker Plugin (Ultra-Fast with fd/find fallback)
Same idea as tmux session picker and kitty session picker.

Priority:
1. 'fd' command (fastest, Rust-based)
2. 'find' command (standard Unix, widely available)
3. Python os.walk (fallback)

Usage:
- Run "Project Picker" from Command Palette
- Or bind a key to command: "project_picker"
"""

import sublime
import sublime_plugin
import os
import subprocess
import threading
import time
from functools import partial


class ProjectCache:
    """Thread-safe cache for project list with TTL."""
    
    def __init__(self, ttl_seconds=60):
        self._cache = []
        self._last_update = 0
        self._ttl = ttl_seconds
        self._lock = threading.Lock()
        self._is_scanning = False
        self._scanner_type = None  # Will be determined on first scan
    
    def get_projects(self, force_refresh=False):
        """Get cached projects, refresh if expired."""
        with self._lock:
            is_expired = (time.time() - self._last_update) > self._ttl
            
            if not force_refresh and not is_expired and self._cache:
                return list(self._cache)
            
            if self._is_scanning:
                return list(self._cache)
        
        if force_refresh or not self._cache:
            self._scan_sync()
        else:
            self._scan_async()
        
        with self._lock:
            return list(self._cache)
    
    def invalidate(self):
        """Force cache invalidation."""
        with self._lock:
            self._last_update = 0
    
    def _get_scanner_type(self):
        """Determine which scanner to use."""
        if self._scanner_type is not None:
            return self._scanner_type
        
        # Try fd first
        try:
            subprocess.run(["fd", "--version"], capture_output=True, check=True, timeout=1)
            self._scanner_type = "fd"
            return self._scanner_type
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
            pass
        
        # Try find
        try:
            subprocess.run(["find", "--version"], capture_output=True, timeout=1)
            self._scanner_type = "find"
            return self._scanner_type
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
            pass
        
        # BSD find on macOS doesn't support --version, check differently
        try:
            result = subprocess.run(["find", ".", "-maxdepth", "0"], 
                                  capture_output=True, timeout=1, cwd="/tmp")
            if result.returncode == 0:
                self._scanner_type = "find"
                return self._scanner_type
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
            pass
        
        # Fall back to Python
        self._scanner_type = "python"
        return self._scanner_type
    
    def _scan_sync(self):
        """Synchronous scan."""
        projects = self._do_scan()
        with self._lock:
            self._cache = projects
            self._last_update = time.time()
    
    def _scan_async(self):
        """Asynchronous scan."""
        if self._is_scanning:
            return
        
        self._is_scanning = True
        
        def scan_worker():
            try:
                projects = self._do_scan()
                with self._lock:
                    self._cache = projects
                    self._last_update = time.time()
            finally:
                self._is_scanning = False
        
        thread = threading.Thread(target=scan_worker, daemon=True)
        thread.start()
    
    def _do_scan(self):
        """Fast project scanning using available tools."""
        projects_dir = os.path.expanduser("~/dev")
        
        if not os.path.exists(projects_dir):
            return []
        
        scanner = self._get_scanner_type()
        
        try:
            if scanner == "fd":
                return self._scan_with_fd(projects_dir)
            elif scanner == "find":
                return self._scan_with_find(projects_dir)
        except Exception as e:
            # If external tool fails, fall through to Python
            pass
        
        # Python fallback
        return self._scan_with_python(projects_dir)
    
    def _scan_with_fd(self, projects_dir):
        """Use fd command to find git repos (blazing fast)."""
        result = subprocess.run(
            ["fd", "-H", "-t", "d", "-d", "3", "-c", "never", "-a", "\\.git", projects_dir],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        return self._process_git_paths(result.stdout, projects_dir)
    
    def _scan_with_find(self, projects_dir):
        """Use standard Unix find command (fast and universally available)."""
        # find ~/dev -maxdepth 3 -name ".git" -type d
        result = subprocess.run(
            ["find", projects_dir, "-maxdepth", "3", "-name", ".git", "-type", "d"],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        return self._process_git_paths(result.stdout, projects_dir)
    
    def _process_git_paths(self, output, projects_dir):
        """Process output from fd/find commands."""
        projects = []
        
        for line in output.strip().split('\n'):
            if not line:
                continue
                
            git_dir = line.strip().rstrip('/')
            project_path = os.path.dirname(git_dir)
            rel_path = os.path.relpath(project_path, projects_dir)
            
            # Skip if it's a subdirectory of another git repo
            parent = os.path.dirname(rel_path)
            if parent:
                parent_has_git = False
                current_parent = parent
                while current_parent:
                    parent_git = os.path.join(projects_dir, current_parent, ".git")
                    if os.path.exists(parent_git):
                        parent_has_git = True
                        break
                    current_parent = os.path.dirname(current_parent)
                
                if parent_has_git:
                    continue
            
            projects.append((rel_path, project_path))
        
        return sorted(projects, key=lambda x: x[0].lower())
    
    def _scan_with_python(self, projects_dir):
        """Fallback Python scanning."""
        projects = []
        
        try:
            for root, dirs, files in os.walk(projects_dir):
                depth = root.count(os.sep) - projects_dir.count(os.sep)
                if depth > 3:
                    del dirs[:]
                    continue
                
                if ".git" in dirs:
                    rel_path = os.path.relpath(root, projects_dir)
                    
                    # Skip subdirectories of git repos
                    if os.path.dirname(rel_path):
                        parent_has_git = False
                        parent = os.path.dirname(rel_path)
                        while parent:
                            if os.path.exists(os.path.join(projects_dir, parent, ".git")):
                                parent_has_git = True
                                break
                            parent = os.path.dirname(parent)
                        if parent_has_git:
                            dirs.remove(".git")
                            continue
                    
                    projects.append((rel_path, root))
                    dirs.remove(".git")
        except Exception:
            pass
        
        return sorted(projects, key=lambda x: x[0].lower())


# Global cache instance
_project_cache = ProjectCache(ttl_seconds=60)


class ProjectCacheInvalidateCommand(sublime_plugin.ApplicationCommand):
    """Manually invalidate the project cache."""
    
    def run(self):
        _project_cache.invalidate()
        sublime.status_message("Project cache invalidated")


class ProjectPickerCommand(sublime_plugin.ApplicationCommand):
    """Command to show project picker and open/focus projects."""

    def run(self, force_refresh=False):
        self.projects = _project_cache.get_projects(force_refresh=force_refresh)
        self.project_names = ["scratch"] + [p[0] for p in self.projects]
        
        sublime.active_window().show_quick_panel(
            self.project_names,
            self._on_project_selected,
            placeholder="Select project..."
        )

    def _on_project_selected(self, index):
        """Handle project selection from quick panel."""
        if index == -1:
            return
        
        if index == 0:
            project_name = "scratch"
            project_path = os.path.expanduser("~/scratch")
            if not os.path.exists(project_path):
                os.makedirs(project_path)
        else:
            project_name = self.project_names[index]
            project_path = self.projects[index - 1][1]
        
        existing_window = self._find_existing_window(project_name, project_path)
        
        if existing_window:
            existing_window.bring_to_front()
        else:
            self._open_project(project_name, project_path)

    def _find_existing_window(self, project_name, project_path):
        """Find if a window with this project is already open."""
        for window in sublime.windows():
            if window.project_file_name():
                if project_name.lower() in window.project_file_name().lower():
                    return window
            
            for folder in window.folders():
                if folder == project_path or os.path.basename(folder) == project_name:
                    return window
        
        return None

    def _open_project(self, project_name, project_path):
        """Open a new window with the project folder."""
        sublime.run_command("new_window")
        
        new_window = sublime.windows()[-1] if sublime.windows() else None
        if not new_window:
            return
        
        new_window.set_project_data({
            "folders": [{
                "path": project_path,
                "name": project_name
            }],
            "settings": {
                "default_line_ending": "unix",
                "ensure_newline_at_eof_on_save": True
            }
        })
        
        os.chdir(project_path)


class ProjectPickerScratchCommand(sublime_plugin.ApplicationCommand):
    """Quick command to open scratch directory."""
    
    def run(self):
        scratch_path = os.path.expanduser("~/scratch")
        if not os.path.exists(scratch_path):
            os.makedirs(scratch_path)
        
        for window in sublime.windows():
            for folder in window.folders():
                if folder == scratch_path:
                    window.bring_to_front()
                    return
        
        sublime.run_command("new_window")
        new_window = sublime.windows()[-1] if sublime.windows() else None
        if new_window:
            new_window.set_project_data({
                "folders": [{
                    "path": scratch_path,
                    "name": "scratch"
                }]
            })


class ProjectPickerCurrentWindowCommand(sublime_plugin.WindowCommand):
    """Command to open project in current window instead of new window."""
    
    def run(self):
        self.projects = _project_cache.get_projects()
        self.project_names = ["scratch"] + [p[0] for p in self.projects]
        
        self.window.show_quick_panel(
            self.project_names,
            self._on_project_selected,
            placeholder="Open project in current window..."
        )
    
    def _on_project_selected(self, index):
        """Handle project selection."""
        if index == -1:
            return
        
        if index == 0:
            project_path = os.path.expanduser("~/scratch")
            if not os.path.exists(project_path):
                os.makedirs(project_path)
        else:
            project_path = self.projects[index - 1][1]
        
        self.window.set_project_data({
            "folders": [{"path": project_path}]
        })
        
        os.chdir(project_path)


class ProjectPickerBackgroundScanner(sublime_plugin.EventListener):
    """Listener to refresh cache in background when idle."""
    
    def on_new(self, view):
        self._maybe_refresh()
    
    def on_load(self, view):
        self._maybe_refresh()
    
    def _maybe_refresh(self):
        """Refresh cache in background if it's stale."""
        last_update = _project_cache._last_update
        if time.time() - last_update > _project_cache._ttl:
            _project_cache._scan_async()
