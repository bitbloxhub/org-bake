#!/usr/bin/env lua

local function getenv_default(name, default)
	local value = os.getenv(name)
	if value == nil or value == "" then
		return default
	end
	return value
end

local function shell_quote(s)
	return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

local function run_capture(cmd)
	local p = io.popen(cmd, "r")
	if not p then
		return nil, false, "popen failed"
	end
	local out = p:read("*a")
	local ok, why, code = p:close()
	if ok == nil then
		return out, false, string.format("%s (%s)", tostring(why), tostring(code))
	end
	if type(ok) == "number" then
		if ok ~= 0 then
			return out, false, string.format("exit %d", ok)
		end
		return out, true, nil
	end
	if ok ~= true then
		return out, false, tostring(why or "command failed")
	end
	return out, true, nil
end

local function run_ok(cmd)
	local ok = os.execute(cmd)
	if type(ok) == "number" then
		return ok == 0
	end
	return ok == true
end

local function path_dirname(path)
	return (path:gsub("/+$", ""):match("^(.*)/[^/]+$") or ".")
end

local function file_exists(path)
	local f = io.open(path, "r")
	if f then
		f:close()
		return true
	end
	return false
end

local function trim(s)
	return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function parse_key_value_lines(text)
	local map = {}
	for line in text:gmatch("[^\n]+") do
		local key, value = line:match("^([^=]+)=(.*)$")
		if key then
			map[key] = value
		end
	end
	return map
end

local script_path = arg[0]
if not script_path or script_path == "" then
	io.stderr:write("unable to determine script path\n")
	os.exit(1)
end

local script_dir = path_dirname(script_path)
local pwd_output = run_capture("pwd")
local pwd = trim(pwd_output or ".")
if script_dir == "." then
	script_dir = pwd
elseif script_dir:sub(1, 1) ~= "/" then
	script_dir = trim(pwd .. "/" .. script_dir)
end
local repo_root = path_dirname(script_dir)
local generator_script = script_dir .. "/generate-synthetic-workspace.lua"
local init_file = script_dir .. "/init-dir/benchmark-init.el"

local file_count = arg[1] or "2000"
local journal_lines = arg[2] or "3000"
local output_parent = arg[3] or ""

local keep_workspace = getenv_default("KEEP_WORKSPACE", "0")
local export_jobs = getenv_default("EXPORT_JOBS", "auto")
local export_batch_size = getenv_default("EXPORT_BATCH_SIZE", "32")
local repeats = tonumber(getenv_default("REPEATS", "1")) or 1
local materialize_jobs = getenv_default("MATERIALIZE_JOBS", "4")
local profile = getenv_default("PROFILE", "0")
local reset_each_run = getenv_default("RESET_EACH_RUN", "0")
local run_rebake_each_run = getenv_default("RUN_REBAKE_EACH_RUN", "0")

if not run_ok("[ -x " .. shell_quote(generator_script) .. " ]") then
	io.stderr:write("missing generator script: " .. generator_script .. "\n")
	os.exit(1)
end

if not file_exists(init_file) then
	io.stderr:write("missing init file: " .. init_file .. "\n")
	os.exit(1)
end

local generator_cmd
if output_parent ~= "" then
	generator_cmd = table.concat({
		"TMPDIR=/tmp nix develop -c lua",
		shell_quote(generator_script),
		shell_quote(file_count),
		shell_quote(journal_lines),
		shell_quote(output_parent),
	}, " ")
else
	generator_cmd = table.concat({
		"TMPDIR=/tmp nix develop -c lua",
		shell_quote(generator_script),
		shell_quote(file_count),
		shell_quote(journal_lines),
	}, " ")
end

local generator_output, generator_ok, generator_err = run_capture(generator_cmd)
if not generator_ok then
	io.stderr:write(generator_err .. "\n")
	os.exit(1)
end

io.write(generator_output or "")
if generator_output and generator_output:sub(-1) ~= "\n" then
	io.write("\n")
end

local workspace_dir
for line in (generator_output or ""):gmatch("[^\n]+") do
	local value = line:match("^workspace=(.*)$")
	if value and value ~= "" then
		workspace_dir = value
		break
	end
end

if not workspace_dir or not run_ok("[ -d " .. shell_quote(workspace_dir) .. " ]") then
	io.stderr:write("failed to detect generated workspace path\n")
	os.exit(1)
end

local function cleanup()
	if keep_workspace == "1" or profile == "1" then
		return
	end
	if workspace_dir ~= nil and run_ok("[ -d " .. shell_quote(workspace_dir) .. " ]") then
		run_ok("rm -rf " .. shell_quote(workspace_dir))
	end
end

local benchmark_eval = [[(let* ((action (or (getenv "ORG_BAKE_BENCH_ACTION")
                                 (error "ORG_BAKE_BENCH_ACTION not set")))
                     (profile-enabled
                      (string= (or (getenv "ORG_BAKE_BENCH_PROFILE_ENABLED") "0")
                               "1"))
                     (profile-cpu-path (getenv "ORG_BAKE_BENCH_PROFILE_CPU_PATH"))
                     (profile-mem-path (getenv "ORG_BAKE_BENCH_PROFILE_MEM_PATH")))
                (defun org-bake-bench--idle-p ()
                  (and
                   (not
                    (seq-some
                     (lambda (job)
                       (and (eq (plist-get job :workspace) (quote bench))
                            (memq (plist-get job :kind) (quote (export-document materialize)))
                            (memq (plist-get job :status) (quote (queued running)))))
                     org-bake-process-queue))
                   (not (timerp (alist-get (quote bench)
                                         org-bake--workspace-materialize-timers)))))
                (defun org-bake-bench--wait-idle ()
                  (let ((timeout-at (+ (float-time) 7200.0)))
                    (while (and (not (org-bake-bench--idle-p))
                                (< (float-time) timeout-at))
                      (accept-process-output nil 0.25))
                    (unless (org-bake-bench--idle-p)
                      (error "Timed out waiting for org-bake queue to drain"))))
                (when profile-enabled
                  (profiler-start (quote cpu+mem)))
                (defun org-bake-bench--read-materializations (workspace)
                  (let* ((materializations-dir
                          (org-bake-project-materializations-dir workspace))
                         (paths
                          (if (file-directory-p materializations-dir)
                              (directory-files materializations-dir t "\\.json$")
                            nil)))
                    (dolist (path paths)
                      (org-bake-store-read-json-file path))))
                (let ((start (float-time)))
                  (pcase action
                    ("rebake" (org-bake-rebake-workspace (quote bench)))
                    ("refresh" (org-bake-refresh-workspace (quote bench)))
                    ("read-materializations"
                     (org-bake-bench--read-materializations (quote bench)))
                    (_ (error "Unknown benchmark action: %s" action)))
                  (org-bake-bench--wait-idle)
                  (princ (format "%s_seconds=%.3f\n"
                                 action
                                 (- (float-time) start))))
                (when profile-enabled
                  (profiler-stop)
                  (when (and profile-cpu-path (> (length profile-cpu-path) 0))
                    (profiler-write-profile (profiler-cpu-profile) profile-cpu-path)
                    (princ (format "profile_cpu=%s\n" profile-cpu-path)))
                  (when (and profile-mem-path (> (length profile-mem-path) 0))
                    (profiler-write-profile (profiler-memory-profile) profile-mem-path)
                    (princ (format "profile_mem=%s\n" profile-mem-path)))))]]

local function run_action_once(
	action,
	export_jobs_value,
	batch_size,
	profile_enabled,
	profile_cpu,
	profile_mem,
	worker_profile_dir
)
	local env_parts = {
		"ORG_BAKE_BENCH_REPO=" .. shell_quote(repo_root),
		"ORG_BAKE_BENCH_ROOT=" .. shell_quote(workspace_dir),
		"ORG_BAKE_BENCH_ACTION=" .. shell_quote(action),
		"ORG_BAKE_BENCH_EXPORT_BATCH_SIZE=" .. shell_quote(batch_size),
		"ORG_BAKE_BENCH_MAX_MATERIALIZE_JOBS=" .. shell_quote(materialize_jobs),
		"ORG_BAKE_BENCH_PROFILE_ENABLED=" .. shell_quote(profile_enabled),
		"ORG_BAKE_BENCH_PROFILE_CPU_PATH=" .. shell_quote(profile_cpu),
		"ORG_BAKE_BENCH_PROFILE_MEM_PATH=" .. shell_quote(profile_mem),
		"ORG_BAKE_BENCH_WORKER_PROFILE_ENABLED=" .. shell_quote(profile_enabled),
		"ORG_BAKE_BENCH_WORKER_PROFILE_DIR=" .. shell_quote(worker_profile_dir),
	}
	if export_jobs_value ~= "" and export_jobs_value ~= "auto" then
		table.insert(env_parts, "ORG_BAKE_BENCH_MAX_EXPORT_JOBS=" .. shell_quote(export_jobs_value))
	end

	local cmd = table.concat({
		"env",
		table.concat(env_parts, " "),
		"nix develop -c emacs --batch -Q -l",
		shell_quote(init_file),
		"--eval",
		shell_quote(benchmark_eval),
	}, " ")

	local result, ok, err = run_capture(cmd)
	if not ok then
		io.stderr:write(err .. "\n")
		return "", "", ""
	end

	local parsed = parse_key_value_lines(result or "")
	local seconds = parsed[action .. "_seconds"] or ""
	local profile_cpu_result = parsed.profile_cpu or ""
	local profile_mem_result = parsed.profile_mem or ""
	return seconds, profile_cpu_result, profile_mem_result
end

local function run_benchmark_once(export_jobs_value, batch_size, run_id, do_reset, do_rebake)
	local rebake_profile_cpu = ""
	local rebake_profile_mem = ""
	local rebake_worker_profile_dir = ""
	local rebake_seconds = ""

	if do_reset then
		run_ok(
			"rm -rf "
				.. shell_quote(workspace_dir .. "/.bench-store")
				.. " "
				.. shell_quote(workspace_dir .. "/.bench-emacs")
				.. " "
				.. shell_quote(workspace_dir .. "/.bench-xdg-data")
		)
	end

	if do_rebake then
		if profile == "1" then
			run_ok("mkdir -p " .. shell_quote(workspace_dir .. "/.bench-profiles"))
			rebake_profile_cpu = string.format(
				"%s/.bench-profiles/cpu-rebake-j%s-b%s-r%d.prof",
				workspace_dir,
				export_jobs_value,
				batch_size,
				run_id
			)
			rebake_profile_mem = string.format(
				"%s/.bench-profiles/mem-rebake-j%s-b%s-r%d.prof",
				workspace_dir,
				export_jobs_value,
				batch_size,
				run_id
			)
			rebake_worker_profile_dir = string.format(
				"%s/.bench-profiles/workers-rebake-j%s-b%s-r%d",
				workspace_dir,
				export_jobs_value,
				batch_size,
				run_id
			)
		end
		rebake_seconds, rebake_profile_cpu, rebake_profile_mem = run_action_once(
			"rebake",
			export_jobs_value,
			batch_size,
			profile,
			rebake_profile_cpu,
			rebake_profile_mem,
			rebake_worker_profile_dir
		)
	end

	local refresh_seconds = select(1, run_action_once("refresh", export_jobs_value, batch_size, "0", "", "", ""))
	local read_materializations_seconds =
		select(1, run_action_once("read-materializations", export_jobs_value, batch_size, "0", "", "", ""))

	return rebake_seconds,
		refresh_seconds,
		read_materializations_seconds,
		rebake_profile_cpu,
		rebake_profile_mem,
		rebake_worker_profile_dir
end

io.write(
	"config_export_jobs,config_batch_size,run,rebake_seconds,refresh_seconds,read_materializations_seconds,profile_cpu,profile_mem,worker_profile_dir\n"
)

local ok, err = pcall(function()
	for run = 1, repeats do
		local do_reset = false
		local do_rebake = false
		if run == 1 then
			do_reset = true
			do_rebake = true
		else
			if reset_each_run == "1" then
				do_reset = true
			end
			if run_rebake_each_run == "1" then
				do_rebake = true
			end
			if do_reset and not do_rebake then
				do_rebake = true
			end
		end

		local rebake_seconds, refresh_seconds, read_materializations_seconds, profile_cpu_path, profile_mem_path, worker_profile_dir =
			run_benchmark_once(export_jobs, export_batch_size, run, do_reset, do_rebake)

		io.write(table.concat({
			export_jobs,
			export_batch_size,
			tostring(run),
			rebake_seconds,
			refresh_seconds,
			read_materializations_seconds,
			profile_cpu_path,
			profile_mem_path,
			worker_profile_dir,
		}, ",") .. "\n")
	end
end)

if keep_workspace == "1" or profile == "1" then
	io.write("workspace=" .. workspace_dir .. "\n")
	io.write("cleaned_up=0\n")
else
	io.write("workspace=" .. workspace_dir .. "\n")
	io.write("cleaned_up=1\n")
end

cleanup()

if not ok then
	io.stderr:write(tostring(err) .. "\n")
	os.exit(1)
end
