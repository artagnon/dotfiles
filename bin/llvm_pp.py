import lldb

def pp(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()
    res = frame.EvaluateExpression("%s->dump()" % command)
    print >>result, res.GetSummary()

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f llvm_pp.pp pp')
    print "Command pp installed"
