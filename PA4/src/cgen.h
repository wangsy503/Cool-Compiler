#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_class_nameTab();
   void code_class_objTab();
   void code_dispatch_tables();
   void code_prototype_objects();
   void code_object_initializer();
   void code_class_methods();

   std::vector<CgenNodeP> init_all_classes();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   CgenNodeP get_cgen_node(Symbol name);
   std::vector<CgenNodeP> all_classes;
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

   int tag;
   

public:
   std::vector<CgenNodeP> all_anc;
   std::vector<method_class*> all_methods;
   std::map<Symbol, Symbol> all_method_class;
   std::vector<attr_class*> all_attrs;

   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   int get_tag() { return tag; };
   void set_tag(int i) { tag = i; };
   std::vector<CgenNodeP> get_all_anc();
   void install_features();
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

class ObjPosition
{
   public:
      ObjPosition(char * reg_in, int offset_in, Symbol name_in){
         reg = reg_in;
         offset = offset_in;
         name = name_in;
      }

      char * get_reg(){return reg;};
      int get_off(){return offset;}
      Symbol get_name(){return name;}

   private:
      char * reg;
      int offset;
      Symbol name;
};