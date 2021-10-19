
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************
#include <algorithm>
#include <stdlib.h>
#include <stdio.h>
#include <typeinfo>
#include <set>
#include "cgen.h"
#include "cgen_gc.h"
#include "emit.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);
int label_num = 0;
int var_num = 1;
SymbolTable<Symbol, ObjPosition> env;
CgenClassTable* codegen_classtable;
CgenNodeP cur_self_classP;
//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  // CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);
  codegen_classtable = new CgenClassTable(classes,os);
  codegen_classtable->code();
  codegen_classtable->exitscope();

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

static void emit_comment(char *content, ostream &s)
{
  s << "### " << content << endl;
}

static void emit_comment(char *content, Symbol name, ostream &s)
{
  s << "### " << content << " " << name << endl;
}

static void emit_copy(ostream &s)
{
  emit_jal("Object.copy", s);
}
//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

// Pop the top of stack to a register. The stack returns to higher addresses.
static void emit_pop(ostream& str)
{
  emit_addiu(SP,SP,4,str);
}

static void emit_pop(char *reg, ostream& str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      s << Str << DISPTAB_SUFFIX;
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      s << Int << DISPTAB_SUFFIX;
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      s << Bool << DISPTAB_SUFFIX;
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_class_nameTab()
{
  //
  // Add class name table.
  //
  // init_all_classes(); // find and init all class tag first

  str << CLASSNAMETAB << LABEL;
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    StringEntry* entry = stringtable.lookup_string(cur_node->get_name()->get_string());
    str << WORD; 
    entry->code_ref(str); 
    str << endl;
  }
}

void CgenClassTable::code_class_objTab()
{
  //
  // Add class object table.
  //
  str << CLASSOBJTAB << LABEL;
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    str << WORD; 
    emit_protobj_ref(cur_node->get_name(), str); 
    str << endl;
    str << WORD; 
    emit_init_ref(cur_node->get_name(), str); 
    str << endl;
  }
}

void CgenClassTable::code_dispatch_tables()
{
  //
  // Add dispatch tables for each class.
  //
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    emit_disptable_ref(cur_node->get_name(), str);
    str << LABEL;
    std::vector<method_class*> cur_all_methods = cur_node->all_methods;
    for (int j=0; j<cur_all_methods.size(); j++){
      Symbol method_name = cur_all_methods[j]->get_name();
      Symbol class_name = cur_node->all_method_class[method_name];
      str << WORD; 
      emit_method_ref(class_name, method_name, str); 
      str << endl;
    }
  }
}

void CgenClassTable::code_prototype_objects()
{
  //
  // Add prototype for each class.
  //
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    
    str << WORD << -1 << endl;
    emit_protobj_ref(cur_node->get_name(), str);
    str << LABEL;
    str << WORD << cur_node->get_tag() << endl;
    str << WORD << DEFAULT_OBJFIELDS + cur_node->all_attrs.size() << endl;
    str << WORD;
    emit_disptable_ref(cur_node->get_name(), str); 
    str << endl;
    for (int j=0; j<cur_node->all_attrs.size(); j++){
      attr_class* cur_attr = cur_node->all_attrs[j];
      str << WORD;
      if (cur_attr->type_decl == Int){
        inttable.lookup_string("0")->code_ref(str);
      }
      else if (cur_attr->type_decl == Str){
        stringtable.lookup_string("")->code_ref(str);
      }
      else if(cur_attr->type_decl == Bool){
        falsebool.code_ref(str);
      }
      else{
        str << 0;
      }
      str << endl;
    }
  }
}


void CgenClassTable::code_object_initializer()
{
  //
  // Add initializer for each class.
  //
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    
    emit_init_ref(cur_node->get_name(), str);
    str << LABEL;

    emit_addiu(SP, SP, -3 * WORD_SIZE, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, WORD_SIZE, str);
    emit_move(SELF, ACC, str);

    if (cur_node->get_parentnd()->get_name() != No_class){
      // if have a parent
      str << JAL;
      emit_init_ref(cur_node->get_parentnd()->get_name(), str);
      str << endl;
    }

    Features features = cur_node->get_features();
    cur_self_classP = cur_node;
    // str << "## not"
    for (int j=features->first(); features->more(j); j=features->next(j)){
      if(!features->nth(j)->is_method()){
        // only consider attributes here
        attr_class * cur_attr = static_cast<attr_class*> (features->nth(j));
        if (typeid(*cur_attr->get_init()) != typeid(no_expr_class)){
          cur_attr->get_init()->code(str);
          emit_store(ACC, DEFAULT_OBJFIELDS + j, SELF, str);
        }
      }
    }

    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 3 * WORD_SIZE, str);
    emit_return(str);
  }
}

void CgenClassTable::code_class_methods()
{
  //
  // Code all methods in each class.
  //
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    if (cur_node->get_name() == Object || cur_node->get_name() == Str || cur_node->get_name() == Int ||
        cur_node->get_name() == Bool || cur_node->get_name() == IO)
        continue;
    std::vector<attr_class*> cur_attrs = cur_node->all_attrs;
    cur_self_classP = all_classes[i];
    str << "### Class " << cur_node->get_name() << "'s methods !!" << endl;

    env.enterscope();
    env.addid(self, new ObjPosition(SELF, 0, SELF_TYPE));
    for (int j=0; j<cur_attrs.size(); j++){
      env.addid(cur_attrs[j]->get_name(), 
                  new ObjPosition(SELF, DEFAULT_OBJFIELDS + j, 
                                  cur_attrs[j]->type_decl));
    }

    Features features = cur_node->get_features();
    for (int j=features->first(); features->more(j); j=features->next(j)){
      if(features->nth(j)->is_method()){
        // only consider attributes here
        method_class * cur_method = static_cast<method_class*> (features->nth(j));
        // Symbol class_name = cur_node->get_name();
        cur_method->code(str, cur_node->get_name());
      }
    }

    env.exitscope();
  }
  
}

void method_class::code(ostream& s, Symbol class_name)
{
  //
  // Generate code fot the method
  //
  emit_method_ref(class_name, name, s);
  s << LABEL;
  emit_addiu(SP, SP, -3 * WORD_SIZE, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, WORD_SIZE, s);
	emit_move(SELF, ACC, s);

  env.enterscope();
  for (int i=formals->first(); formals->more(i); i=formals->next(i)){
    Symbol formal_name = formals->nth(i)->get_name();
    Symbol formal_type = formals->nth(i)->get_type();
    env.addid(formal_name, 
              new ObjPosition(FP, 3 + formals->len() - 1 - i, formal_type));
  }
  expr->code(s);
  env.exitscope();

  emit_load(FP, 3, SP, s);
	emit_load(SELF, 2, SP, s);
	emit_load(RA, 1, SP, s);
	emit_addiu(SP, SP, (3+formals->len()) * WORD_SIZE, s);
	emit_return(s);
}

std::vector<CgenNodeP> CgenClassTable::init_all_classes()
{
  int i = 0;
  for(List<CgenNode> *l = nds; l; l = l->tl()){
    CgenNodeP cur_node = l->hd();
    cur_node->set_tag(i);
    i++;
    cur_node->get_all_anc();
    cur_node->install_features();
    all_classes.push_back(cur_node);
  }
  return all_classes;
}

CgenNodeP CgenClassTable::get_cgen_node(Symbol name)
{
  str <<"## in get_cgen_node"<<endl;
  for (int i=0; i<all_classes.size(); i++){
    CgenNodeP cur_node = all_classes[i];
    if (cur_node->get_name() == name){
      return cur_node;
    }
      
  }
  str << "\t# !!!!!! wrong!!!!!!, cannnot find class "<<name<<endl;
  return nullptr;
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{

   enterscope();
  //  if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   init_all_classes(); // find and init all class tag first
   stringclasstag = probe(Str)->get_tag() /* Change to your String class tag here */;
   intclasstag = probe(Int)->get_tag() /* Change to your Int class tag here */;
   boolclasstag = probe(Bool)->get_tag() /* Change to your Bool class tag here */;

  //  code();
  //  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "# coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "# choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "# coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  emit_comment("### Start code class name table ###", str);
  code_class_nameTab();
  emit_comment("### Start code class object table ###", str);
  code_class_objTab();
  emit_comment("### Start code dispatch table ###", str);
  code_dispatch_tables();
  emit_comment("### Start code class prototypes ###", str);
  code_prototype_objects();

  if (cgen_debug) cout << "# coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  emit_comment("### Start code class initializers ###", str);
  code_object_initializer();
  emit_comment("### Start code class methods ###", str);
  code_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

std::vector<CgenNodeP> CgenNode::get_all_anc()
{
  //
  // Get all ancestor of this class node and save in all_anc
  //
  CgenNodeP curr_node = this;
  while(curr_node->get_name() != No_class){
      all_anc.push_back(curr_node);
      curr_node = curr_node->get_parentnd();
  }
  std::reverse(all_anc.begin(), all_anc.end());
  return all_anc;
}

int findMethodIndex(std::vector<method_class*> all_methods, Symbol name){
  for (int i=0; i<all_methods.size(); i++) {
    if (all_methods[i]->get_name() == name){
      return i;
    }
  }
  return -1;
}

void CgenNode::install_features()
{
  //
  // Find all methods and attributes of this class.
  //
  CgenNodeP curr_node;
  Features features;
  method_class * curr_method;
  attr_class* curr_attr;

  for (int i=0; i<all_anc.size(); i++){
    curr_node = all_anc[i];
    features = curr_node->get_features();
    // std::cout << "########" << get_name() << ": # of features " << features->len() << std::endl;
    for (int j=features->first(); features->more(j); j=features->next(j)){
        if(features->nth(j)->is_method()){
          // a method
          curr_method = static_cast<method_class*> (features->nth(j));
          if (all_method_class.find(curr_method->get_name()) != all_method_class.end()){
            // this method already in table
            all_method_class[curr_method->get_name()] = curr_node->get_name();
            int index = findMethodIndex(all_methods, curr_method->get_name());
            all_methods[index] = curr_method;
          }
          else{
            // this method not in table
            all_methods.push_back(curr_method);
            all_method_class[curr_method->get_name()] = curr_node->get_name();
          }
        }
        else{
          // an attribute
          curr_attr = static_cast<attr_class*> (features->nth(j));
          all_attrs.push_back(curr_attr);
        }
    }
  }

  
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
  if (cgen_debug) emit_comment("assign class", name, s);
  ObjPosition *pos = env.lookup(name);  /* Find this symble. */
  expr->code(s);
  emit_store(ACC, pos->get_off(), pos->get_reg(), s); /* Assign. */
}

void static_dispatch_class::code(ostream &s) {
  if (cgen_debug) emit_comment("static dispatch class", name, s);
  int label_abort = label_num++;
  int label_finish = label_num++;
  int i;
  for (i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
  }
  if (expr->get_type() == SELF_TYPE)
    emit_move(ACC, SELF, s);
  else
    expr->code(s); /* ACC: v0 */
  emit_beqz(ACC, label_abort, s); /* If void, abort. */

  /* Execute method. */
  emit_partial_load_address(T1, s);
  emit_disptable_ref(type_name, s);
  s << endl; /* Get dispatch table */
  std::vector<method_class*> cur_all_methods = codegen_classtable->get_cgen_node(type_name)->all_methods; 
  int offset = 0;
  for(method_class* method_ : cur_all_methods) {
    if (method_->get_name() == name)
      break;
    offset++;
  }
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
  emit_branch(label_finish, s);

  /* Process of aborting, filename in $a0, line number in $t1*/
  emit_label_def(label_abort, s);
  emit_load_address(ACC, "str_const0", s); /* Filename is stored at "str_const0". */
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  /* End of this part. */
  emit_label_def(label_finish, s);
}

void dispatch_class::code(ostream &s) {
  if (cgen_debug) emit_comment("dispatch class", name, s);
  int label_abort = label_num++;
  int label_finish = label_num++;
  int i;
  // s << "#- before actual" <<endl;
  for (i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
    // s << "#-- " << i <<endl;
  }
  // s << "#- after actual" <<endl;
  if (expr->get_type() == SELF_TYPE)
    emit_move(ACC, SELF, s);
  else
    expr->code(s); /* ACC: v0 */
  // s << "#- after self" <<endl;
  emit_beqz(ACC, label_abort, s); /* If void, abort. */

  /* Execute method. */
  emit_load(T1, 2, ACC, s); /* Get dispatch table, which is stored at 3rd place in objtab. */
  std::vector<method_class*> cur_all_methods; 
  Symbol type_name = expr->get_type();
  if (type_name == SELF_TYPE){
    cur_all_methods = cur_self_classP->all_methods;
  }
  else{
    CgenNodeP node = codegen_classtable->get_cgen_node(type_name);
    cur_all_methods = node->all_methods;
  }
  int offset = 0;
  for(method_class* method_ : cur_all_methods) {
    if (method_->get_name() == name)
      break;
    offset++;
  }
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
  emit_branch(label_finish, s);

  /* Process of aborting, filename in $a0, line number in $t1*/
  emit_label_def(label_abort, s);
  emit_load_address(ACC, "str_const0", s); /* Filename is stored at "str_const0". */
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_dispatch_abort", s);
  /* End of this part. */
  emit_label_def(label_finish, s);
}

void cond_class::code(ostream &s) {
  if (cgen_debug) emit_comment("cond class", s);
  int label_else = label_num++;
  int label_finish = label_num++;
  pred->code(s);
  emit_fetch_int(T1, ACC, s); /* Fetch bool val. */
  emit_beqz(T1, label_else, s); /* If false to else. */
  then_exp->code(s);
  emit_branch(label_finish, s); /* Jump to finish. */
  emit_label_def(label_else, s);
  else_exp->code(s);
  emit_label_def(label_finish, s);
}

void loop_class::code(ostream &s) {
  if (cgen_debug) emit_comment("loop class", s);
  int label_start = label_num++;
  int label_finish = label_num++;
  emit_label_def(label_start, s);
  pred->code(s);
  emit_fetch_int(T1, ACC, s);
  emit_beqz(T1, label_finish, s); /* If false, jump out of loop. */
  body->code(s);
  emit_branch(label_start, s); /* Start another iteration. */
  emit_label_def(label_finish, s);
}

std::vector<int> get_children_tags (std::vector<int> src_tags) {
  std::vector<int> children_tags;
  int i;
  for (i = 0; i < src_tags.size(); i++) {
    CgenNodeP src_node = codegen_classtable->all_classes[src_tags[i]];
    List<CgenNode>* child_nodes = src_node->get_children();
    for (List<CgenNode> *l = child_nodes; l; l = l->tl()){
      int child_tag = l->hd()->get_tag();
      if (find(children_tags.begin(), children_tags.end(), child_tag) == children_tags.end())
        children_tags.push_back(child_tag);
    }
  }
  return children_tags;
}

bool finish_match (std::vector<std::vector<int> > tags) {
  int i; bool flag = true;
  for (i = 0; i < tags.size(); i++) {
    if (!tags[i].empty())
      flag = false;
  }
  return flag;
}

void typcase_class::code(ostream &s) {
  if (cgen_debug) emit_comment("typcase class", s);
  int label_no_match = label_num++;
  int label_abort2 = label_num++;
  int label_exec = label_num++;
  int label_finish = label_num + cases->len();
  label_num = label_finish + 1;
  int i;
  expr->code(s);
  emit_beqz(ACC, label_abort2, s);

  // emit_push(ACC, s);
  emit_load(T1, 0, ACC, s);

  std::vector<std::vector<int> > tags;
  for (i = cases->first(); cases->more(i); i = cases->next(i)) {
    Symbol case_type = ((branch_class*)(cases->nth(i)))->type_decl;
    emit_comment("Now we are handling typcase_class: case_type:", case_type, s);
    int case_tag = codegen_classtable->get_cgen_node(case_type)->get_tag();
    std::vector<int> case_tags;
    case_tags.push_back(case_tag);
    tags.push_back(case_tags);
    
  }

  while (!finish_match(tags)) {
    if (cgen_debug) emit_comment("There is a while loop inside typcase_class", s);
    for (i = 0; i < tags.size(); i++) {
      std::vector<int> case_tags = tags[i];
      for (auto case_tag: case_tags) {
        emit_load_imm(T2, case_tag, s);
        emit_beq(T1, T2, label_exec + i, s);
      }
    }
    for (i = 0; i < tags.size(); i++) {
      tags[i] = get_children_tags(tags[i]);
    }
  }
  /* If reach here without jumping to other label: no match. */
  emit_label_def(label_no_match, s);
  emit_jal("_case_abort", s);
  emit_branch(label_finish, s);

  for (i = cases->first(); cases->more(i); i = cases->next(i)) {
    Symbol case_type = ((branch_class*)(cases->nth(i)))->type_decl;
    Symbol case_name = ((branch_class*)(cases->nth(i)))->name;
    Expression case_expr = ((branch_class*)(cases->nth(i)))->expr;
    emit_label_def(label_exec + i, s);

    env.enterscope();
    env.addid(case_name, new ObjPosition(FP, -var_num, case_type)); 
    var_num++;
    emit_push(ACC, s);
    case_expr->code(s);
    emit_pop(s);
    emit_branch(label_finish, s);
    var_num--;
    env.exitscope();
  }

  emit_label_def(label_abort2, s); /* Encounter error. */
  emit_load_address(ACC, "str_const0", s); /* Filename is stored at "str_const0". */
  emit_load_imm(T1, get_line_number(), s);
  emit_jal("_case_abort2", s);

  emit_label_def(label_finish, s);
}

void block_class::code(ostream &s) {
  if (cgen_debug) emit_comment("block class", s);
  int i;
  for (i = body->first(); body->more(i); i = body->next(i)){
    body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
  if (cgen_debug) emit_comment("let class", identifier, s);
  if (typeid(*init) == typeid(no_expr_class)) {
    if (type_decl == Str) 
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    else if (type_decl == Int)
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    else if (type_decl == Bool)
      emit_load_bool(ACC, falsebool, s);
    else
      init->code(s);
  }
  else 
    init->code(s);
  
  env.enterscope();
  // std::auto_ptr<ObjPosition> p(new ObjPosition(FP, -var_num, type_decl));
  env.addid(identifier, new ObjPosition(FP, -var_num, type_decl));
  var_num++;
  emit_push(ACC, s);
  body->code(s);
  emit_pop(s); /* Just pop but not assign. */
  var_num--;
  env.exitscope();
}

void arith_code(Expression e1, Expression e2, ostream &s) {
  /* Help function for arith code generation. */
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_copy(s); /* A new Object same as e2 obj. Now ACC is pointing to newly constucted obj. */
  emit_fetch_int(T2, ACC, s); /* Get e2's int value. */
  emit_pop(T1, s);
  emit_fetch_int(T1, T1, s); /* Get e1's int value. */
}

void plus_class::code(ostream &s) {
  if (cgen_debug) emit_comment("plus class", s);
  arith_code(e1, e2, s);
  emit_add(T2, T1, T2, s);
  emit_store_int(T2, ACC, s);
}

void sub_class::code(ostream &s) {
  if (cgen_debug) emit_comment("sub class", s);
  arith_code(e1, e2, s);
  emit_sub(T2, T1, T2, s);
  emit_store_int(T2, ACC, s);
}

void mul_class::code(ostream &s) {
  if (cgen_debug) emit_comment("mul class", s);
  arith_code(e1, e2, s);
  emit_mul(T2, T1, T2, s);
  emit_store_int(T2, ACC, s);
}

void divide_class::code(ostream &s) {
  if (cgen_debug) emit_comment("divide class", s);
  arith_code(e1, e2, s);
  emit_div(T2, T1, T2, s);
  emit_store_int(T2, ACC, s);
}

void neg_class::code(ostream &s) {
  if (cgen_debug) emit_comment("neg class", s);
  e1->code(s);
  emit_copy(s); /* Now ACC is pointing to new e1 obj. */
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s) {
  if (cgen_debug) emit_comment("lt class", s);
  int label_finish = label_num++;
  arith_code(e1, e2, s);
  emit_load_bool(ACC, truebool, s); /* Assume it's true. */
  emit_blt(T1, T2, label_finish, s); /* Yes, e1.val < e2.val, jump to finish. */
  emit_load_bool(ACC, falsebool, s); /* No, we have to reassign. */
  emit_label_def(label_finish, s);
}

void eq_class::code(ostream &s) {
  if (cgen_debug) emit_comment("lt class", s);
  int label_finish = label_num++;
  e1->code(s); emit_push(ACC, s);
  e2->code(s); emit_move(T2, ACC, s);
  emit_pop(T1, s);
  emit_load_bool(ACC, truebool, s); /* 2 objectives: first, initiat $a0 for equality_test function; second, assume T1 and T2 are identical. */
  emit_load_bool(A1, falsebool, s); /* Initiat $a1 for equality_test. */
  emit_beq(T1, T2, label_finish, s);
  emit_jal("equality_test", s); /* Polymorphic equality testing function in trap.s */
  emit_label_def(label_finish, s);
}

void leq_class::code(ostream &s) {
  if (cgen_debug) emit_comment("leq class", s);
  int label_finish = label_num++;
  arith_code(e1, e2, s);
  emit_load_bool(ACC, truebool, s); /* Assume it's true. */
  emit_bleq(T1, T2, label_finish, s); /* Yes, e1.val < e2.val, jump to finish. */
  emit_load_bool(ACC, falsebool, s); /* No, we have to reassign. */
  emit_label_def(label_finish, s);
}

void comp_class::code(ostream &s) {
  if (cgen_debug) emit_comment("comp class", s);
  int label_finish = label_num++;
  e1->code(s);
  emit_fetch_int(T1, ACC, s); /* Same as fetch int since offset is the same. */
  emit_load_bool(ACC, truebool, s); /* Assume transfer to true. */
  emit_beqz(T1, label_finish, s); /* If bingo: jump to finish. */
  emit_load_bool(ACC, falsebool, s); /* else: change it to false. */
  emit_label_def(label_finish, s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  if (cgen_debug) emit_comment("new class", type_name, s);
  if (type_name == SELF_TYPE) {
    /*** FIND ***/
    emit_load(T1, 0, SELF, s); /* Get class tag: a nunmber. */
    emit_load_address(T2, CLASSOBJTAB, s); /* Get label class objTab. */
    emit_sll(T1, T1, 3, s); /* << 3 is equal to x 8. */
    emit_add(T1, T1, T2, s); /* at offset 8 x t is the prototype object. */
    /*** INIT ***/
    emit_load(ACC, 0, T1, s); /* Load prototype object to $a0. */
    emit_push(T1, s); /* Since Object.copy will change $t1. */
    emit_copy(s);
    emit_pop(T1, s);
    emit_load(T1, 1, T1, s); /* Load initialization method -> 8 x t + 4. */
    emit_jalr(T1, s); /* Initialize new obj. */
  }
  else {
    /* Get prototype obj addr by name. */
    s << LA << ACC << " ";
    emit_protobj_ref(type_name, s);
    s << endl;
    emit_copy(s);
    /* Get init method and jump to it by name. */
    s << JAL << " ";
    emit_init_ref(type_name, s);
    s << endl;
  }
}

void isvoid_class::code(ostream &s) {
  if (cgen_debug) emit_comment("isvoid class", s);
  int label_finish = label_num++;
  e1->code(s);
  emit_move(T1, ACC, s); /* Save ACC to T1. */
  emit_load_bool(ACC, truebool, s); /* Assume is void. */
  emit_beqz(T1, label_finish, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label_finish, s);
}

void no_expr_class::code(ostream &s) {
  if (cgen_debug) emit_comment("no_expr class", s);
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s) {
  if (cgen_debug) emit_comment("object class", name, s);
  if (name == self)
    emit_move(ACC, SELF, s);
  else {
    ObjPosition *pos = env.lookup(name);
    emit_load(ACC, pos->get_off(), pos->get_reg(), s);
  }
}