package main

import (
	"flag"
	"fmt"
	"os"
	"runtime/pprof"

	"github.com/chzyer/readline"
	lua "github.com/jslyzt/glua"
	"github.com/jslyzt/glua/parse"
)

func main() {
	os.Exit(mainAux())
}

func mainAux() int {
	var (
		opte, optl, optp         string
		opti, optv, optdt, optdc bool
		optm                     int
	)

	flag.StringVar(&opte, "e", "", "")
	flag.StringVar(&optl, "l", "", "")
	flag.StringVar(&optp, "p", "", "")
	flag.IntVar(&optm, "mx", 0, "")
	flag.BoolVar(&opti, "i", false, "")
	flag.BoolVar(&optv, "v", false, "")
	flag.BoolVar(&optdt, "dt", false, "")
	flag.BoolVar(&optdc, "dc", false, "")
	flag.Usage = func() {
		fmt.Println(`Usage: glua [options] [script [args]].
Available options are:
  -e stat  execute string 'stat'
  -l name  require library 'name'
  -mx MB   memory limit(default: unlimited)
  -dt      dump AST trees
  -dc      dump VM codes
  -i       enter interactive mode after executing 'script'
  -p file  write cpu profiles to the file
  -v       show version information`)
	}
	flag.Parse()
	if len(optp) != 0 {
		f, err := os.Create(optp)
		if err != nil {
			fmt.Println(err.Error())
			os.Exit(1)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}
	if len(opte) == 0 && !opti && !optv && flag.NArg() == 0 {
		opti = true
	}

	status := 0

	L := lua.NewState()
	defer L.Close()
	if optm > 0 {
		L.SetMx(optm)
	}

	if optv || opti {
		fmt.Println(lua.PackageCopyRight)
	}

	if len(optl) > 0 {
		if err := L.DoFile(optl); err != nil {
			fmt.Println(err.Error())
		}
	}

	if nargs := flag.NArg(); nargs > 0 {
		script := flag.Arg(0)
		argtb := L.NewTable()
		for i := 1; i < nargs; i++ {
			L.RawSet(argtb, lua.LNumber(i), lua.LString(flag.Arg(i)))
		}
		L.SetGlobal("arg", argtb)
		if optdt || optdc {
			file, err := os.Open(script)
			if err != nil {
				fmt.Println(err.Error())
				return 1
			}
			chunk, err2 := parse.Parse(file, script)
			if err2 != nil {
				fmt.Println(err2.Error())
				return 1
			}
			if optdt {
				fmt.Println(parse.Dump(chunk))
			}
			if optdc {
				proto, err3 := lua.Compile(chunk, script)
				if err3 != nil {
					fmt.Println(err3.Error())
					return 1
				}
				fmt.Println(proto.String())
			}
		}
		if err := L.DoFile(script); err != nil {
			fmt.Println(err.Error())
			status = 1
		}
	}

	if len(opte) > 0 {
		if err := L.DoString(opte); err != nil {
			fmt.Println(err.Error())
			status = 1
		}
	}

	if opti {
		doREPL(L)
	}
	return status
}

// do read/eval/print/loop
func doREPL(L *lua.LState) {
	rl, err := readline.New("> ")
	if err != nil {
		panic(err)
	}
	defer rl.Close()
	for {
		if str, err := loadline(rl, L); err == nil {
			if err := L.DoString(str); err != nil {
				fmt.Println(err)
			}
		} else { // error on loadline
			fmt.Println(err)
			return
		}
	}
}

func incomplete(err error) bool {
	if lerr, ok := err.(*lua.APIError); ok {
		if perr, ok := lerr.Cause.(*parse.Error); ok {
			return perr.Pos.Line == parse.EOF
		}
	}
	return false
}

func loadline(rl *readline.Instance, L *lua.LState) (string, error) {
	rl.SetPrompt("> ")
	line, err := rl.Readline()
	if err == nil {
		if _, err := L.LoadString("return " + line); err == nil { // try add return <...> then compile
			return line, nil
		}
		return multiline(line, rl, L)
	}
	return "", err
}

func multiline(ml string, rl *readline.Instance, L *lua.LState) (string, error) {
	for {
		if _, err := L.LoadString(ml); err == nil { // try compile
			return ml, nil
		} else if !incomplete(err) { // syntax error , but not EOF
			return ml, nil
		} else {
			rl.SetPrompt(">> ")
			if line, err := rl.Readline(); err == nil {
				ml = ml + "\n" + line
			} else {
				return "", err
			}
		}
	}
}
