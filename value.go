package glua

import (
	"context"
	"fmt"
	"os"
)

// LValueType lua值类型
type LValueType int

// lua值类型枚举
const (
	LTNil LValueType = iota
	LTBool
	LTNumber
	LTString
	LTFunction
	LTUserData
	LTThread
	LTTable
	LTChannel
)

var lValueNames = [9]string{"nil", "boolean", "number", "string", "function", "userdata", "thread", "table", "channel"}

func (vt LValueType) String() string {
	return lValueNames[int(vt)]
}

// LValue lua值
type LValue interface {
	String() string
	Type() LValueType
	// to reduce `runtime.assertI2T2` costs, this method should be used instead of the type assertion in heavy paths(typically inside the VM).
	assertFloat64() (float64, bool)
	// to reduce `runtime.assertI2T2` costs, this method should be used instead of the type assertion in heavy paths(typically inside the VM).
	assertString() (string, bool)
	// to reduce `runtime.assertI2T2` costs, this method should be used instead of the type assertion in heavy paths(typically inside the VM).
	assertFunction() (*LFunction, bool)
}

// LVIsFalse returns true if a given LValue is a nil or false otherwise false.
func LVIsFalse(v LValue) bool { return v == LNil || v == LFalse }

// LVAsBool returns false if a given LValue is a nil or false otherwise true.
func LVAsBool(v LValue) bool { return v != LNil && v != LFalse }

// LVAsString returns string representation of a given LValue
// if the LValue is a string or number, otherwise an empty string.
func LVAsString(v LValue) string {
	switch sn := v.(type) {
	case LString, LNumber:
		return sn.String()
	default:
		return ""
	}
}

// LVCanConvToString returns true if a given LValue is a string or number
// otherwise false.
func LVCanConvToString(v LValue) bool {
	switch v.(type) {
	case LString, LNumber:
		return true
	default:
		return false
	}
}

// LVAsNumber tries to convert a given LValue to a number.
func LVAsNumber(v LValue) LNumber {
	switch lv := v.(type) {
	case LNumber:
		return lv
	case LString:
		if num, err := parseNumber(string(lv)); err == nil {
			return num
		}
	}
	return LNumber(0)
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// LNilType lua nil 类型
type LNilType struct{}

// String string func
func (nl *LNilType) String() string {
	return "nil"
}

// Type 类型
func (nl *LNilType) Type() LValueType {
	return LTNil
}

func (nl *LNilType) assertFloat64() (float64, bool)     { return 0, false }
func (nl *LNilType) assertString() (string, bool)       { return "", false }
func (nl *LNilType) assertFunction() (*LFunction, bool) { return nil, false }

// LNil lua nil value
var LNil = LValue(&LNilType{})

//////////////////////////////////////////////////////////////////////////////////////////////////

// LBool lua bool 类型
type LBool bool

// String string func
func (bl LBool) String() string {
	if bool(bl) {
		return "true"
	}
	return "false"
}

// Type 类型
func (bl LBool) Type() LValueType {
	return LTBool
}

func (bl LBool) assertFloat64() (float64, bool)     { return 0, false }
func (bl LBool) assertString() (string, bool)       { return "", false }
func (bl LBool) assertFunction() (*LFunction, bool) { return nil, false }

// lua bool value
var (
	LTrue  = LBool(true)
	LFalse = LBool(false)
)

//////////////////////////////////////////////////////////////////////////////////////////////////

// LString lua string 类型
type LString string

// String string func
func (st LString) String() string {
	return string(st)
}

// Type 类型
func (st LString) Type() LValueType {
	return LTString
}

func (st LString) assertFloat64() (float64, bool)     { return 0, false }
func (st LString) assertString() (string, bool)       { return string(st), true }
func (st LString) assertFunction() (*LFunction, bool) { return nil, false }

// Format fmt.Formatter interface
func (st LString) Format(f fmt.State, c rune) {
	switch c {
	case 'd', 'i':
		if nm, err := parseNumber(string(st)); err != nil {
			defaultFormat(nm, f, 'd')
		} else {
			defaultFormat(string(st), f, 's')
		}
	default:
		defaultFormat(string(st), f, c)
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// String string func
func (nm LNumber) String() string {
	if isInteger(nm) {
		return fmt.Sprint(int64(nm))
	}
	return fmt.Sprint(float64(nm))
}

// Type 类型
func (nm LNumber) Type() LValueType {
	return LTNumber
}

func (nm LNumber) assertFloat64() (float64, bool)     { return float64(nm), true }
func (nm LNumber) assertString() (string, bool)       { return "", false }
func (nm LNumber) assertFunction() (*LFunction, bool) { return nil, false }

// Format fmt.Formatter interface
func (nm LNumber) Format(f fmt.State, c rune) {
	switch c {
	case 'q', 's':
		defaultFormat(nm.String(), f, c)
	case 'b', 'c', 'd', 'o', 'x', 'X', 'U':
		defaultFormat(int64(nm), f, c)
	case 'e', 'E', 'f', 'F', 'g', 'G':
		defaultFormat(float64(nm), f, c)
	case 'i':
		defaultFormat(int64(nm), f, 'd')
	default:
		if isInteger(nm) {
			defaultFormat(int64(nm), f, c)
		} else {
			defaultFormat(float64(nm), f, c)
		}
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// LTable lua table 类型
type LTable struct {
	Metatable LValue

	array   []LValue
	dict    map[LValue]LValue
	strdict map[string]LValue
	keys    []LValue
	k2i     map[LValue]int
}

// String string func
func (tb *LTable) String() string {
	return fmt.Sprintf("table: %p", tb)
}

// Type 类型
func (tb *LTable) Type() LValueType {
	return LTTable
}

func (tb *LTable) assertFloat64() (float64, bool)     { return 0, false }
func (tb *LTable) assertString() (string, bool)       { return "", false }
func (tb *LTable) assertFunction() (*LFunction, bool) { return nil, false }

//////////////////////////////////////////////////////////////////////////////////////////////////

// LFunction lua func 类型
type LFunction struct {
	IsG       bool
	Env       *LTable
	Proto     *FunctionProto
	GFunction LGFunction
	Upvalues  []*Upvalue
}

// LGFunction lua 函数
type LGFunction func(*LState) int

// String string func
func (fn *LFunction) String() string {
	return fmt.Sprintf("function: %p", fn)
}

// Type 类型
func (fn *LFunction) Type() LValueType {
	return LTFunction
}

func (fn *LFunction) assertFloat64() (float64, bool)     { return 0, false }
func (fn *LFunction) assertString() (string, bool)       { return "", false }
func (fn *LFunction) assertFunction() (*LFunction, bool) { return fn, true }

//////////////////////////////////////////////////////////////////////////////////////////////////

// Global 全局
type Global struct {
	MainThread    *LState
	CurrentThread *LState
	Registry      *LTable
	Global        *LTable

	builtinMts map[int]LValue
	tempFiles  []*os.File
	gccount    int32
}

//////////////////////////////////////////////////////////////////////////////////////////////////

// LState 状态
type LState struct {
	G       *Global
	Parent  *LState
	Env     *LTable
	Panic   func(*LState)
	Dead    bool
	Options Options

	stop         int32
	reg          *registry
	stack        CallFrameStack
	alloc        *allocator
	currentFrame *CallFrame
	wrapped      bool
	uvcache      *Upvalue
	hasErrorFunc bool
	mainLoop     func(*LState, *CallFrame)
	ctx          context.Context
}

// String string func
func (ls *LState) String() string {
	return fmt.Sprintf("thread: %p", ls)
}

// Type 类型
func (ls *LState) Type() LValueType {
	return LTThread
}

func (ls *LState) assertFloat64() (float64, bool)     { return 0, false }
func (ls *LState) assertString() (string, bool)       { return "", false }
func (ls *LState) assertFunction() (*LFunction, bool) { return nil, false }

//////////////////////////////////////////////////////////////////////////////////////////////////

// LUserData lua 用户数据
type LUserData struct {
	Value     interface{}
	Env       *LTable
	Metatable LValue
}

// String string func
func (ud *LUserData) String() string {
	return fmt.Sprintf("userdata: %p", ud)
}

// Type 类型
func (ud *LUserData) Type() LValueType {
	return LTUserData
}

func (ud *LUserData) assertFloat64() (float64, bool)     { return 0, false }
func (ud *LUserData) assertString() (string, bool)       { return "", false }
func (ud *LUserData) assertFunction() (*LFunction, bool) { return nil, false }

//////////////////////////////////////////////////////////////////////////////////////////////////

// LChannel lua channel类型
type LChannel chan LValue

// String string func
func (ch LChannel) String() string {
	return fmt.Sprintf("channel: %p", ch)
}

// Type 类型
func (ch LChannel) Type() LValueType {
	return LTChannel
}

func (ch LChannel) assertFloat64() (float64, bool)     { return 0, false }
func (ch LChannel) assertString() (string, bool)       { return "", false }
func (ch LChannel) assertFunction() (*LFunction, bool) { return nil, false }
