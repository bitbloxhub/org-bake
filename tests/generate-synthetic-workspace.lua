#!/usr/bin/env lua

math.randomseed(os.time() + tonumber(tostring({}):match("0x(.*)"), 16))

local file_count = tonumber(arg[1]) or 2000
local journal_lines = tonumber(arg[2]) or 3000
local output_parent = arg[3]

if journal_lines % 15 ~= 0 then
	io.stderr:write("journal line count must be divisible by 15\n")
	os.exit(1)
end

local entry_count = journal_lines / 15

local function shell_quote(s)
	return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

local function run_capture(cmd)
	local p = io.popen(cmd, "r")
	if not p then
		return nil
	end
	local out = p:read("*a")
	local ok = p:close()
	if not ok then
		return nil
	end
	return (out:gsub("%s+$", ""))
end

local function run_ok(cmd)
	local ok = os.execute(cmd)
	if type(ok) == "number" then
		return ok == 0
	end
	return ok == true
end

local workspace_dir
if output_parent and output_parent ~= "" then
	run_ok("mkdir -p " .. shell_quote(output_parent))
	workspace_dir = run_capture("mktemp -d " .. shell_quote(output_parent .. "/org-bake-synth-XXXXXX"))
else
	workspace_dir = run_capture("mktemp -d -t org-bake-synth-XXXXXX")
end

if not workspace_dir or workspace_dir == "" then
	io.stderr:write("failed to create temp workspace directory\n")
	os.exit(1)
end

local many_files_dir = workspace_dir .. "/many-files"
local journal_dir = workspace_dir .. "/journal"
local journal_file = journal_dir .. "/journal.org"

if not run_ok("mkdir -p " .. shell_quote(many_files_dir) .. " " .. shell_quote(journal_dir)) then
	io.stderr:write("failed to create workspace subdirectories\n")
	os.exit(1)
end

local id_counter = 0

local function make_id()
	id_counter = id_counter + 1

	local p1 = id_counter % 4294967296
	local p2 = math.random(0, 65535)
	local p3 = math.random(0, 65535)
	local p4 = math.random(0, 65535)
	local p5 = (id_counter * 1103515245 + math.random(0, 1048575)) % 281474976710656

	return string.format("%08x-%04x-%04x-%04x-%012x", p1, p2, p3, p4, p5)
end

local function note_content(index, doc_id, heading_id)
	return table.concat({
		string.format("#+title: Synthetic Note %05d", index),
		"#+filetags: :synthetic:benchmark:",
		":PROPERTIES:",
		":ID:       " .. doc_id,
		":END:",
		"",
		"* Summary",
		":PROPERTIES:",
		":ID:       " .. heading_id,
		":END:",
		"This is a synthetic benchmark note.",
		"It has repeated structure for exporter stress.",
		"",
		"* Context",
		"- Item one",
		"- Item two",
		"- Item three",
		"",
		"* Links",
		"- Self ID: [[id:" .. doc_id .. "][Document ID]]",
		"- Heading ID: [[id:" .. heading_id .. "][Summary heading]]",
		"",
		"* Notes",
		"Generated for org-bake stress testing.",
		"End.",
	}, "\n") .. "\n"
end

for i = 1, file_count do
	local doc_id = make_id()
	local heading_id = make_id()
	local file_path = string.format("%s/note-%05d.org", many_files_dir, i)
	local f = assert(io.open(file_path, "w"))
	f:write(note_content(i, doc_id, heading_id))
	f:close()
end

local day_names = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" }
local start_epoch = os.time({ year = 2020, month = 1, day = 1, hour = 0, min = 0, sec = 0 })

local jf = assert(io.open(journal_file, "w"))
for i = 0, entry_count - 1 do
	local epoch = start_epoch + (i * 86400)
	local date = os.date("!%Y-%m-%d", epoch)
	local weekday = day_names[tonumber(os.date("!%w", epoch)) + 1]
	local entry_id = make_id()
	local ref_id = make_id()
	jf:write(table.concat({
		"* " .. date .. " " .. weekday,
		":PROPERTIES:",
		":ID:       " .. entry_id,
		":END:",
		"- Mood: steady",
		"- Work: indexing benchmarks",
		"- Notes: repeated synthetic journal content",
		"- Link: [[id:" .. ref_id .. "][Reference]]",
		"",
		"Some reflective text for parsing load.",
		"Another sentence to keep block length fixed.",
		"",
		"CLOCK: [" .. date .. " " .. weekday .. " 09:00]--[" .. date .. " " .. weekday .. " 09:30] =>  0:30",
		"",
		"End of entry.",
	}, "\n"))
	jf:write("\n")
end
jf:close()

local function count_lines(path)
	local n = 0
	for _ in io.lines(path) do
		n = n + 1
	end
	return n
end

local function count_files(path)
	local out = run_capture("ls -1 " .. shell_quote(path) .. " | wc -l") or "0"
	return tonumber(out:match("%d+")) or 0
end

print("workspace=" .. workspace_dir)
print("many_files=" .. many_files_dir)
print("many_files_count=" .. tostring(count_files(many_files_dir)))
print("sample_file_lines=" .. tostring(count_lines(string.format("%s/note-%05d.org", many_files_dir, 1))))
print("journal_file=" .. journal_file)
print("journal_lines=" .. tostring(count_lines(journal_file)))
