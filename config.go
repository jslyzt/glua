package glua

import (
	"os"
)

// 变量定义
var (
	CompatVarArg     = true
	FieldsPerFlush   = 50
	RegistrySize     = 256 * 20
	RegistryGrowStep = 32
	CallStackSize    = 256
	MaxTableGetLoop  = 100
	MaxArrayIndex    = 67108864
)

// LNumber lua number 定义
type LNumber float64

// 常量定义
const (
	LNumberBit        = 64
	LNumberScanFormat = "%f"
	LuaVersion        = "Lua 5.1"
)

// 变量定义
var (
	LuaPath        = "LUA_PATH"
	LuaLDir        string
	LuaPathDefault string
	LuaOS          string
)

func init() {
	if os.PathSeparator == '/' { // unix-like
		LuaOS = "unix"
		LuaLDir = "/usr/local/share/lua/5.1"
		LuaPathDefault = "./?.lua;" + LuaLDir + "/?.lua;" + LuaLDir + "/?/init.lua"
	} else { // windows
		LuaOS = "windows"
		LuaLDir = "!\\lua"
		LuaPathDefault = ".\\?.lua;" + LuaLDir + "\\?.lua;" + LuaLDir + "\\?\\init.lua"
	}
}
