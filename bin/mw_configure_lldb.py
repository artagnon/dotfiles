#!/usr/bin/python
#
# mw_configure_lldb.py
#
# LLDB python reference: http://lldb.llvm.org/python-reference
#

import os
import lldb
import inspect
import re

#----------------------------------------------
# Custom formatting for fl::ustring
#----------------------------------------------

def make_string(F,L):
    strval = 'u"'
    try:
        G = F.GetData()
        data_array = F.GetPointeeData(0, L).uint16
        for X in range(0, L):
            V = data_array[X]
            if V == 0:
                break
            strval += unichr(V)
    except:
        pass
    strval = strval + '"'
    return strval.encode('utf-8')

def make_string_from_pointer(F,L):
    strval = 'u"'
    try:
        data_array = F.GetPointeeData(0, L).uint16
        for X in range(0, L):
            V = data_array[X]
            if V == 0:
                break
            strval += unichr(V)
    except:
        pass
    strval = strval + '"'
    return strval.encode('utf-8')

def remove_quotes(S):
    logger = lldb.formatters.Logger.Logger()
    end = len (S) - 1
    if S[0] == '"' and S[end] == '"':
        return S[1:end]
    else:
        return S

def MyAddress_Summary(value, unused):
    pattern = '(\s*)"(.*)"(\s*)'
    firstName = value.GetChildMemberWithName("_firstName")
    lastName = value.GetChildMemberWithName("_secondName")
    firstNameSummary = re.sub(pattern, '\\1\\2\\3', firstName.GetSummary())
    lastNameSummary = re.sub(pattern, '\\1\\2\\3', lastName.GetSummary())
    return firstNameSummary + " " + lastNameSummary

def ustring_summary(value, unused):
    try:
        r = value.GetChildAtIndex(0)
        r_unknown = r.GetChildAtIndex(0)
        first = r_unknown.GetChildAtIndex(0)
        first_unknown = first.GetChildAtIndex(0)
        l = first_unknown.GetChildAtIndex(0)
        s = first_unknown.GetChildAtIndex(1)
        s_size = l.GetChildAtIndex(0).GetValueAsUnsigned(0) & 0xFF
        if (s_size & 0x01) != 0 :
            # long form
            size = l.GetChildAtIndex(1).GetValueAsUnsigned(0)
            data = l.GetChildAtIndex(2)
        else:
            #short form
            data = s.GetChildAtIndex(1)
            #real size is s_size / 2 or we can leave it as s_size, because we check for 0 byte in make_string
            size = s_size >> 1

        return make_string(data, size)
    except:
        return value

def get_max_size(value):
    _max_size_ = None
    try:
        debugger = value.GetTarget().GetDebugger()
        _max_size_ = int(lldb.SBDebugger.GetInternalVariableValue('target.max-string-summary-length', debugger.GetInstanceName()).GetStringAtIndex(0))
    except:
        _max_size_ = 512
    return _max_size_

def flustring_summary(value, unused):
    wasHandled = False
    try:
        _M_dataplus = value.GetChildAtIndex(0)
        _M_p = _M_dataplus.GetChildAtIndex(0)
        return make_string(_M_p, get_max_size(value))
    except:
        pass
    try:
        r = value.GetChildAtIndex(0)
        r_unknown = r.GetChildAtIndex(0)
        first = r_unknown.GetChildAtIndex(0)
        first_unknown = first.GetChildAtIndex(0)
        l = first_unknown.GetChildAtIndex(0)
        s = first_unknown.GetChildAtIndex(1)
        size = s.GetChildAtIndex(0).GetChildAtIndex(0).GetValueAsUnsigned(42)
        data = s.GetChildAtIndex(1)
        return make_string(data, size)
    except:
        pass
    return value

def unsigned_short_summary(value, unused):
    return make_string(value, get_max_size(value))

class slsvdiagnosticitemProvider:
    def __init__(self, valobj, dict):
        logger = lldb.formatters.Logger.Logger()
        self.valobj = valobj
        self.max_size = None

    def get_summary(self):
        logger = lldb.formatters.Logger.Logger()
        msg = self.valobj.GetChildMemberWithName('msg')
        if msg == None:
            msgSummary = "msg undefined (1)"
        else:
            msgSummary = msg.GetSummary()
            if msgSummary == None:
                msgSummary = "msg undefined (2)"
        id = self.valobj.GetChildMemberWithName('identifier')
        if id == None:
            idSummary = "id undefined (1)"
        else:
            idSummary  = id.GetSummary()
            if idSummary == None:
                idSummary = "id undefined (2)"
        msgSummary = remove_quotes(msgSummary)
        idSummary  = remove_quotes(idSummary)
        return "id = " + idSummary + " msg = " + msgSummary

def slsvdiagnosticitem_summary(value, unused):
    logger = lldb.formatters.Logger.Logger()
    prov = slsvdiagnosticitemProvider(value, None)
    return prov.get_summary()

#----------------------------------------------
# Custom support for bex command
#----------------------------------------------

def bex(debugger, command, result, internal_dict):
    debugger.HandleCommand('pro hand -p true -n false -s false SIGSEGV SIGBUS')
    debugger.HandleCommand('b mnDebugRuntimeFault')
    debugger.HandleCommand('b fl_diag_terminate')

def make_string_from_pointer_with_offset(F,OFFS,L):
    strval = 'u"'
    try:
        data_array = F.GetPointeeData(0, L).uint16
        for X in range(OFFS, L):
            V = data_array[X]
            if V == 0:
                break
            strval += unichr(V)
    except:
        pass
    strval = strval + '"'
    return strval.encode('utf-8')

# supports qt5
def qstring_summary(value, unused):
    try:
        d = value.GetChildMemberWithName('d')
        #have to divide by 2 (size of unsigned short = 2)
        offset = d.GetChildMemberWithName('offset').GetValueAsUnsigned() / 2
        size = get_max_size(value)
        return make_string_from_pointer_with_offset(d, offset, size)
    except:
        return value

#----------------------------------------------
# Main LLDB entry point
#----------------------------------------------

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.MyAddress_Summary MyAddress')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.ustring_summary "std::__1::u16string"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.ustring_summary "std::__1::basic_string<char16_t, std::__1::char_traits<char16_t>, std::__1::allocator<char16_t> >"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.ustring_summary "std::__1::basic_string<unsigned short, std::__1::char_traits<unsigned short>, std::__1::allocator<unsigned short> >"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.ustring_summary "std::__1::__libcpp_compressed_pair_imp<std::__1::basic_string<char16_t, std::__1::char_traits<char16_t>, std::__1::allocator<char16_t> >::__rep, std::__1::allocator<char16_t> >"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.ustring_summary "fl::ustring"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.qstring_summary "QString"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "const unsigned short *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "unsigned short *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "const fl::uchar *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "fl::uchar *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "const uchar *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.unsigned_short_summary "uchar *"')
    debugger.HandleCommand('type summary add -F  mw_configure_lldb.slsvdiagnosticitem_summary "slsvDiagnosticItem"')
    debugger.HandleCommand('command script add -f mw_configure_lldb.bex bex')
    print ' '
    print 'User command bex:'
    print '    process handle -p true -n false -s false SIGSEGV SIGBUS'
    print '    b mnDebugRuntimeFault'
    print '    b fl_diag_terminate'
    print ' '
