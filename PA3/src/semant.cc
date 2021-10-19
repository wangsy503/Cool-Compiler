
#include <map>
#include <set>
#include <algorithm>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

ClassTable *classtable;
std::map <Symbol, Class_> class_table;
SymbolTable <Symbol, Symbol> attr_table;
typedef SymbolTable <Symbol, method_class> MethodTable;
std::map <Symbol, MethodTable> method_table_dir;


//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    install_basic_classes();

    // we first add all classes into std::map 'class_table' and check duplicates
    Class_ curr_class;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)){
        curr_class = classes->nth(i);
        if(curr_class->get_name() == SELF_TYPE){
            semant_error(curr_class) 
                << "Redefinition of basic class SELF_TYPE.\n";
        }
        if (class_table.find(curr_class->get_name()) != class_table.end()){
            semant_error(curr_class) 
                << "Class " << curr_class->get_name() 
                << " was previously defined.\n";
        }
        else{
            class_table[curr_class->get_name()] = curr_class;
        }
    }

    // check whether there is a main class, and whether there exists inheritance cycle
    check_have_main();
    check_inherit_cycle(classes);
}

void ClassTable::check_have_main() {
    if (class_table.find(Main) == class_table.end()){
        semant_error() 
            << "Class Main is not defined.\n";
    }
}

void ClassTable::check_inherit_cycle(Classes classes) {
    Class_ curr_class;
    Symbol parent;
    Class_ child;
    for (int i=classes->first(); classes->more(i); i = classes->next(i)){
        curr_class = classes->nth(i);
        child = curr_class;
        parent = curr_class->get_parent();
        if (parent == Int || parent == Bool || parent == Str)
            semant_error(curr_class) 
                << "Class "<< curr_class->get_name() 
                << " cannot inherit class "<< parent 
                <<".\n";
        std::set<Symbol> already_parent; 
        // already_parent.push_back(curr_class->get_name()); 
        while (parent != Object){
            if (class_table.find(parent) == class_table.end()){
                semant_error(child) 
                    << "Class "<< child->get_name() 
                    <<" inherits from an undefined class " << parent 
                    << ".\n";
                break;
            }
            // if (parent == curr_class->get_name()){
            if (already_parent.count(parent)){    
                semant_error(curr_class) << "Class "<< curr_class->get_name() << " or its ancestor has an inheritance cycle\n";
                break;
            }
            already_parent.insert(child->get_name());
            child = class_table[parent];
            parent = child->get_parent();
        }
    }
}

bool ClassTable::is_inherit(Symbol anc, Symbol kid, Class_ curr_class){
    if (anc == SELF_TYPE){
        anc = curr_class->get_name();
        // return kid == SELF_TYPE;
    }
    if (kid == SELF_TYPE){
        kid = curr_class->get_name();
    }
    while (kid != No_class){
        if (kid == anc){
            return true;
        }
        kid = class_table[kid]->get_parent();
    }
    return false;
}

std::vector<Symbol> ClassTable::get_all_anc(Symbol t, Class_ curr_class){
    std::vector<Symbol> all_anc;
    if (t == SELF_TYPE){
        t = curr_class->get_name();
    }
    while(t != No_class){
        all_anc.push_back(t);
        t = class_table[t]->get_parent();
    }
    std::reverse(all_anc.begin(), all_anc.end());
    return all_anc;
}

Symbol ClassTable::first_common_anc(Symbol t1, Symbol t2, Class_ curr_class){
    if (t1 == SELF_TYPE && t2 == SELF_TYPE)
        return SELF_TYPE;
    std::vector<Symbol> all_anc1 = get_all_anc(t1, curr_class);
    std::vector<Symbol> all_anc2 = get_all_anc(t2, curr_class);
    int i = 1;
    while(i<all_anc1.size() && i<all_anc2.size()){
        if (all_anc1[i] != all_anc2[i])
            break;
        i++;
    }
    return all_anc1[i-1];
}

void install_methods() {
    std::map <Symbol, Class_>::iterator iter;
    Class_ curr_class;
    Features features;
    MethodTable * curr_method_table;
    method_class * curr_method;
    for (iter = class_table.begin(); iter != class_table.end(); iter++){
        curr_class = iter->second;
        features = curr_class->get_features();
        curr_method_table = &method_table_dir[curr_class->get_name()];
        curr_method_table->enterscope();

        for (int i=features->first(); features->more(i); i=features->next(i)){
            if(!features->nth(i)->is_method())
                continue;
            curr_method = static_cast<method_class*> (features->nth(i));
            if(curr_method_table->lookup(features->nth(i)->get_name()))
                classtable->semant_error(curr_class) 
                    << "Class "<< curr_class->get_name() 
                    <<"has a duplicate declaration of method" << curr_method->get_name() 
                    <<".\n";
            else
                curr_method_table->addid(curr_method->get_name(), curr_method);
        }
    }

    if (method_table_dir[Main].lookup(main_meth) == NULL){
        classtable->semant_error(class_table[Main]) 
            << "No 'main' method in class Main.\n";
    }

    Symbol curr_type;
    Symbol anc;
    method_class * anc_method;
    Formals curr_formals;
    Formals anc_formals;
    for (iter = class_table.begin(); iter != class_table.end(); iter++){
        curr_class = iter->second;
        features = curr_class->get_features();

        for (int i=features->first(); features->more(i); i=features->next(i)){
            if(!features->nth(i)->is_method())
                continue;
            curr_method = static_cast<method_class*> (features->nth(i));
            curr_type = curr_method->get_type();
            curr_formals = curr_method->get_formals();
            
            std::vector<Symbol> all_anc = classtable->get_all_anc(curr_class->get_name(), curr_class);
            // std::cout<<"check Class "<< curr_class->get_name()<< " all ancs: \n";
            for (int j=all_anc.size()-1; j>=0; j--){
                anc = all_anc[j];
                // std::cout<<"   in class"<< curr_class->get_name()<< " has anc: "<< anc <<std::endl;
                anc_method = method_table_dir[anc].lookup(curr_method->get_name());
                if (anc_method){
                    if (anc_method->get_type() != curr_type)
                        classtable->semant_error(curr_class) 
                            << "Class "<< curr_class->get_name() 
                            << "'s method "<< curr_method->get_name() 
                            << " has a different return type " << curr_type 
                            << " with ancestor. "<< "\n";
                    anc_formals = anc_method->get_formals();
                    int k=0;
                    for (; curr_formals->more(k) && anc_formals->more(k); k++){
                        if (curr_formals->nth(k)->get_type() != anc_formals->nth(k)->get_type())
                            classtable->semant_error(curr_class) 
                                << "In redefined method " << curr_method->get_name()
                                << ", parameter type " << curr_formals->nth(k)->get_type()
                                << " is different from original type " << anc_formals->nth(k)->get_type()
                                << std::endl;
                    }
                    if (curr_formals->more(k) || anc_formals->more(k))
                        classtable->semant_error(curr_class) 
                            << "Incompatible number of formal parameters in redefined method " << curr_method->get_name() 
                            << ".\n";
                }
            }
        }
    }
}


void install_attributes_and_check_features_type() {
    std::map <Symbol, Class_>::iterator iter;
    Class_ curr_class;
    Class_ anc_class;
    Features anc_features;
    Features curr_features;
    Symbol curr_feature_type;
    attr_class * curr_attr;
    for (iter = class_table.begin(); iter != class_table.end(); iter++){
        curr_class = iter->second;

        if (curr_class->get_name()==Object || curr_class->get_name()==Int || curr_class->get_name()==Str || curr_class->get_name()==Bool || curr_class->get_name()==IO)
            continue;

        std::vector<Symbol> all_anc = classtable->get_all_anc(curr_class->get_name(), curr_class);
        for (int i=0; i<all_anc.size(); i++){
            attr_table.enterscope();
            anc_class = class_table[all_anc[i]];
            anc_features = anc_class->get_features();

            for (int j=anc_features->first(); anc_features->more(j); j=anc_features->next(j)){
                if(anc_features->nth(j)->is_method())
                    continue;
                curr_attr = static_cast<attr_class*> (anc_features->nth(j));
                if (attr_table.lookup(curr_attr->get_name())){
                    classtable->semant_error(curr_class) 
                        << "Attribute "<< curr_attr->get_name() 
                        << " is an attribute of an inherited class.\n";
                    continue;
                }
                if (curr_attr->get_name() == self)
                    classtable->semant_error(curr_class)
                        << "'self' cannot be the name of an attribute.\n";
                attr_table.addid(curr_attr->get_name(), new Symbol(curr_attr->get_type()));
            }
        }

        curr_features = curr_class->get_features();
        for (int i=curr_features->first(); curr_features->more(i); i=curr_features->next(i)){
            curr_features->nth(i)->check_type(curr_class);
        }

        for (int i=0; i<all_anc.size(); i++){
            attr_table.exitscope();
        }
    }
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    /*
     The class Str has a number of slots and operations:
           val                                  the length of the string
           str_field                            the string itself
           length() : Int                       returns length of the string
           concat(arg: Str) : Str               performs string concatenation
           substr(arg: Int, arg2: Int): Str     substring selection
    */       
    Class_ Str_class =
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
	       filename);


    class_table[Object] = Object_class;
    class_table[IO] = IO_class;
    class_table[Int] = Int_class;
    class_table[Bool] = Bool_class;
    class_table[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

/* 
    Do type checking. There are 24 expression classes, 1 case class, 1 formal class and 2 feature classes. 
*/

/* Feature classes 
   name, formals, return_type, expr */
void method_class::check_type(Class_ cur_class)
{
    Symbol formal_name, formal_type;
    std::set <Symbol> formal_names;

    attr_table.enterscope();
    /* Set return_type to Object? */
    if (return_type != SELF_TYPE && class_table.find(return_type) == class_table.end())
        classtable->semant_error(cur_class->get_filename(), this)
            << "Undefined return type " << return_type
            << " in method " << name << ".\n";
    /* Check each formal. */
    for (int i = formals->first(); formals->more(i); i = formals->next(i)){
        formal_name = formals->nth(i)->get_name();
        formal_type = formals->nth(i)->get_type();
        if (formal_name == self)
            classtable->semant_error(cur_class->get_filename(), this)
                << "'self' cannot be the name of a formal parameter."
                << std::endl;
        /* The identifiers used in the formal parameter list must be distinct.  */
        if (formal_names.count(formal_name))
            classtable->semant_error(cur_class->get_filename(), this)
                << "Formal name duplicated."
                << std::endl;
        else
            formal_names.insert(formal_name);
        if (class_table.find(formal_type) == class_table.end())
            classtable->semant_error(cur_class->get_filename(), this)
                << "Formal parameter " << formal_name
                << " cannot have type " << formal_type <<".\n";
        attr_table.addid(formal_name, new Symbol(formal_type));
    }
    /* The type of the method body must conform to the declared return type. */
    Symbol expr_type = expr->check_type(cur_class);
    if (!classtable->is_inherit(return_type, expr_type, cur_class))
        classtable->semant_error(cur_class->get_filename(), this)
            << "Inferred return type " << expr_type
            << " of method " << name
            << " does not conform to declared return type " << return_type
            << ".\n";
    if (return_type == SELF_TYPE){
        if (expr_type != SELF_TYPE){
            classtable->semant_error(cur_class->get_filename(), this)
            << "Inferred return type " << expr_type
            << " of method " << name
            << " does not conform to declared return type " << return_type
            << ".\n";
        }
    }
    attr_table.exitscope();
}

// name, type_decl, init
void attr_class::check_type(Class_ cur_class)
{
    Symbol init_type = init->check_type(cur_class); 
    if (init_type != No_type){
        if (!classtable->is_inherit(type_decl, init_type, cur_class))
        classtable->semant_error(cur_class->get_filename(), this)
            << "Type in init is Ancestor of type declared."
            << std::endl;
    }
}

/* Case classes */
// branch: name, type_decl, expr
Symbol branch_class::check_type(Class_ cur_class)
{
    attr_table.enterscope();
    attr_table.addid(name, new Symbol(type_decl));
    Symbol return_type = expr->check_type(cur_class);
    attr_table.exitscope();
    return return_type;
}

/* Expression classes */
/* TODO: expr21, expr4: should we return object? 
         multiple let? */
// Expr 1: name, expr. T′≤ T
Symbol assign_class::check_type(Class_ cur_class)
{
    Symbol *T = attr_table.lookup(name);
    Symbol T_ = expr->check_type(cur_class);
    if (!T){
        classtable->semant_error(cur_class->get_filename(), this)
            << "Cannot assign to \'" << name << "\'."
            << std::endl;
        type = Object;
        return Object;
    }
    if (!classtable->is_inherit(*T, T_, cur_class) || (*T == SELF_TYPE && T_ != SELF_TYPE)){
        classtable->semant_error(cur_class->get_filename(), this)
            << "Type " << T_
            << " of assigned expression does not conform to declared type " << *T
            << " of identifier " << name << ".\n";
        type = Object;
        return Object;
    }
    type = T_;
    return T_;
}

// Expr 2: expr e0, type_name T, name f, actual(Expressions f(e1, ..., en))
Symbol static_dispatch_class::check_type(Class_ cur_class)
{
    type = No_type;
    Expression e0 = expr;
    Symbol T = type_name, f = name, anc, actual_type, formal_type;
    method_class *anc_method = NULL;
    Symbol T0 = e0->check_type(cur_class);
    int i;
    if (T0 != SELF_TYPE && class_table.find(T0) == class_table.end()){
        classtable->semant_error(cur_class->get_filename(), this)
            << "Dispatch on undefined class "
            << T0 << "."
            << std::endl;
        type = Object;
        return type;
    }
    if (T != SELF_TYPE && class_table.find(T) == class_table.end()){
        type = Object;
        return type;
    }
    /* Check T>=T0.*/
    if (!classtable->is_inherit(T, T0, cur_class)){
        classtable->semant_error(cur_class->get_filename(), this)
            << "Expression type " << T0
            << " does not conform to declared static dispatch type " << T
            << ".\n";
        type = Object;
    }
    /* Method must exist. Got method in anc_method. */
    std::vector <Symbol> all_anc = classtable->get_all_anc(T, cur_class);
    for (i = 0; i < all_anc.size(); i++){
        anc = all_anc[i];
        anc_method = method_table_dir[anc].lookup(f);
        if (anc_method)
            break;
    }
    if (!anc_method){ /* If method cannot be found. */
        classtable->semant_error(cur_class->get_filename(), this)
            << "Dispatch to undefined method "
            << f
            << ".\n";
        type = Object;
    }
    /* The argument types of the dispatch must conform to the declared argument types. */
    if (anc_method){
        if (actual->len() == anc_method->get_formals()->len()){
            for (i = actual->first(); actual->more(i); i = actual->next(i)){
                actual_type = actual->nth(i)->check_type(cur_class);
                if (anc_method){
                    formal_type = anc_method->get_formals()->nth(i)->get_type();
                    if (!classtable->is_inherit(formal_type, actual_type, cur_class)){
                        classtable->semant_error(cur_class->get_filename(), this)
                            << "formal type is not Ancestor of actual type."
                            << std::endl;
                        type = Object;
                    }
                }
            }
        }
        else{
            classtable->semant_error(cur_class->get_filename(), this)
                << "Method "
                << anc_method->get_name()
                << " called with wrong number of arguments."
                << std::endl;
            type = Object;
        }
    }
    /* the type of the result of the dispatch is either the declared return type or T0 in the case that the declared return type is SELF TYPE */
    if (type != Object){
        type = anc_method->get_type() == SELF_TYPE ? 
            T0 : anc_method->get_type();
    }
    return type;
}

// Expr 3: expr e0, name f, actual(Expressions f(e1, ..., en)). No T.
Symbol dispatch_class::check_type(Class_ cur_class)
{
    Expression e0 = expr;
    Symbol f = name;
    Symbol anc, actual_type, formal_type;
    method_class *anc_method = NULL;
    int i;
    /* Check T0's type.*/
    Symbol T0 = e0->check_type(cur_class);
    if (T0 != SELF_TYPE && class_table.find(T0) == class_table.end()){
        classtable->semant_error(cur_class->get_filename(), this)
            << "Dispatch on undefined class "
            << T0
            << ".\n";
        type = Object;
        return type;
    }
    /* Method must exist. Got method in anc_method. */
    std::vector <Symbol> all_anc = classtable->get_all_anc(T0, cur_class);
    for (i = 0; i < all_anc.size(); i++){
        anc = all_anc[i];
        anc_method = method_table_dir[anc].lookup(f);
        if (anc_method)
            break;
    }
    if (!anc_method){ /* If method cannot be found. */
        classtable->semant_error(cur_class->get_filename(), this)
            << "Dispatch to undefined method "
            << f
            << ".\n";
        type = Object;
    }

    /* The argument types of the dispatch must conform to the declared argument types. */
    if (anc_method){
        if (actual->len() == anc_method->get_formals()->len()){
            for (i = actual->first(); actual->more(i); i = actual->next(i)){
                actual_type = actual->nth(i)->check_type(cur_class);
                if (anc_method){
                    formal_type = anc_method->get_formals()->nth(i)->get_type();
                    if (!classtable->is_inherit(formal_type, actual_type, cur_class)){
                        classtable->semant_error(cur_class->get_filename(), this)
                            << "In call of method " << anc_method->get_name()
                            << ", type " << actual_type
                            << " of parameter " << anc_method->get_formals()->nth(i)->get_name()
                            << " does not conform to declared type " << formal_type <<"."
                            << std::endl;
                        type = Object;
                    }
                }
            }
        }
        else{
            classtable->semant_error(cur_class->get_filename(), this)
                << "Method "
                << anc_method->get_name()
                << " called with wrong number of arguments."
                << std::endl;
            type = Object;
        }
    }
    
  
    /* the type of the result of the dispatch is either the declared return type or T0 in the case that the declared return type is SELF TYPE */
    if (type != Object){
        type = anc_method->get_type() == SELF_TYPE ? 
            T0 : anc_method->get_type();
    }
    
    return type;
}

// Expr 4: pred, then_exp, else_exp
Symbol cond_class::check_type(Class_ cur_class)
{
    if (pred->check_type(cur_class) != Bool)
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "Condition operation with non Bool predicate." 
            << std::endl;
    Symbol then_type = then_exp->check_type(cur_class),
           else_type = else_exp->check_type(cur_class);
    if (else_type == No_type) /* Means no else. */{
        type = then_type;
        return then_type;
    }
    type = classtable->first_common_anc(then_type, else_type, cur_class);
    return type;
}

/* Expr 5: pred, body
 * The predicate of a loop must have type Bool. 
 * The type of the entire loop is always Object. */
Symbol loop_class::check_type(Class_ cur_class)
{
    if (pred->check_type(cur_class) != Bool)
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "Loop condition does not have type Bool." 
            << std::endl;
    body->check_type(cur_class);
    type = Object;
    return type;
}

// Expr 6: expr, cases
Symbol typcase_class::check_type(Class_ cur_class)
{
    expr->check_type(cur_class);
    /* Check the branch type. */
    Symbol return_type = cases->nth(0)->check_type(cur_class);
    /* The variables declared on each branch of a case 
        must all have distinct types. */
    std::set <Symbol> decl_types;
    for (size_t i = cases->first(); cases->more(i); i = cases->next(i)){
        return_type = classtable->first_common_anc(
                    return_type, 
                    cases->nth(i)->check_type(cur_class), 
                    cur_class);
        branch_class *branch = (branch_class *)(cases->nth(i));
        Symbol decl_type = branch->get_decl_type();
        if(decl_types.count(decl_type))
            classtable->semant_error(cur_class->get_filename() ,this) 
            << "Duplicate branch "
            << decl_type
            << " in case statement." 
            << std::endl;
        else
            decl_types.insert(decl_type);
    }
    type = return_type;
    return type;
    
}

// Expr 7: Sequence. Note: expressions. Each expression should be checked.
Symbol block_class::check_type(Class_ cur_class)
{
    for (size_t i = body->first(); body->more(i); i = body->next(i))
        type = body->nth(i)->check_type(cur_class);
    return type;
}

/* Expr 8: Symbol identifier x; Symbol type_decl T0; Expression init e1: T1; Expression body e2;
    Note that the type of x may be SELF TYPE. */
Symbol let_class::check_type(Class_ cur_class)
{
    if (identifier == self)
        classtable->semant_error(cur_class->get_filename() ,this)
            << "'self' cannot be bound in a 'let' expression."
            << std::endl;
        
    Symbol T0 = type_decl;
    attr_table.enterscope();
    /* First, the initialization e1 is type checked in an environment without a new definition for x. Thus, the variable x cannot be used in e1 unless it already has a definition in an outer scope. */
    Symbol T1 = init->check_type(cur_class);
    attr_table.addid(identifier, new Symbol(T0));
    /* Second, the body e2 is type checked in the environment O extended with the typing x : T0′.  */
    Symbol T2 = body->check_type(cur_class);
    if (T1 != No_type){
        if (!classtable->is_inherit(T0, T1, cur_class))
            classtable->semant_error(cur_class->get_filename() ,this)
                << "Inferred type " << T1
                << " of initialization of " << identifier
                << " does not conform to identifier's declared type " << T0
                << ".\n";
    }
    attr_table.exitscope();
    type = T2;
    return type;
}

// Expr 9: arithmetic operation, e1 and e2 should all be Int
Symbol plus_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "non-Int arguments: " << e1->check_type(cur_class) 
            << " + " << e2->check_type(cur_class)
            << std::endl;
        type = Object;
        return type;
    }
    type = Int;
    return type;
}

// Expr 10: arithmetic operation, e1 and e2 should all be Int
Symbol sub_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "non-Int arguments: " << e1->check_type(cur_class) 
            << " - " << e2->check_type(cur_class)
            << std::endl;
        type = Object;
        return type;
    }
    type = Int;
    return type;
}

// Expr 11: arithmetic operation, e1 and e2 should all be Int
Symbol mul_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "non-Int arguments: " << e1->check_type(cur_class) 
            << " * " << e2->check_type(cur_class)
            << std::endl;
        type = Object;
        return type;
    }
    type = Int;
    return type;
}

// Expr 12: arithmetic operation, e1 and e2 should all be Int
Symbol divide_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "non-Int arguments: " << e1->check_type(cur_class) 
            << " / " << e2->check_type(cur_class)
            << std::endl;
        type = Object;
        return type;
    }
    type = Int;
    return type;
}

// Expr 13: e1 should be Bool.
Symbol neg_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "non-Int arguments: ~" << e1->check_type(cur_class)
            << std::endl;
        type = Object;
        return type;
    }
    type = Int;
    return type;
}

// Expr 14: Compare operation, e1 and e1 should be Int, and return type is Bool.
Symbol lt_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "lt Compare between non Int variables." 
            << std::endl;
        type = Object;
        return type;
    }
    type = Bool;
    return type;
}

/* Expr 15: equality. Any types may be freely compared except 
    Int, String and Bool, which may only be compared with 
    objects of the same type. */
Symbol eq_class::check_type(Class_ cur_class)
{
    Symbol e1_type = e1->check_type(cur_class), 
           e2_type = e2->check_type(cur_class);
    if (e1_type != e2_type){
        if (e1_type == Int || e1_type == Str || e1_type == Bool || 
            e2_type == Int || e2_type == Str || e2_type == Bool){
            classtable->semant_error(cur_class->get_filename() ,this) 
                << "Illegal comparison with a basic type." 
                << std::endl;
            type = Object;
            return type;
        }
    }
    type = Bool;
    return type;
}

// Expr 16: Compare operation, e1 and e1 should be Int, and return type is Bool.
Symbol leq_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Int || e2->check_type(cur_class) != Int){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "leq Compare between non Int variables." 
            << std::endl;
        type = Object;
        return type;
    }
    type = Bool;
    return type;
}

// Expr 17: e1 should be Bool.
Symbol comp_class::check_type(Class_ cur_class)
{
    if (e1->check_type(cur_class) != Bool){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "'Not' with non Bool variable." 
            << std::endl;
        type = Object;
        return type;
    }
    type = Bool;
    return type;
}

// Expr 18: Int const
Symbol int_const_class::check_type(Class_ cur_class)
{
    type = Int;
    return type;
}

// Expr 19: Bool const
Symbol bool_const_class::check_type(Class_ cur_class)
{
    type = Bool;
    return type;
}

// Expr 20: String const
Symbol string_const_class::check_type(Class_ cur_class)
{
    type = Str;
    return type;
}

// Expr 21: TODO: how to handle self_type
Symbol new__class::check_type(Class_ cur_class)
{

    if (class_table.find(type_name) == class_table.end() && type_name != SELF_TYPE){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << "'new' used with undefined class "
            << type_name << ".\n";
    }
    type = type_name;
    return type;
}

// Expr 22: Should always return Bool.
Symbol isvoid_class::check_type(Class_ cur_class)
{
    e1->check_type(cur_class);
    type = Bool;
    return type;
}

// Expr 23: No type is just No_type.
Symbol no_expr_class::check_type(Class_ cur_class)
{
    type = No_type;
    return type;
}

// Expr 24: object identifiers. Has attribute name.
Symbol object_class::check_type(Class_ cur_class)
{    
    if (name == self){
        type = SELF_TYPE;
        return type;
    }
    if (!attr_table.lookup(name)){
        classtable->semant_error(cur_class->get_filename() ,this) 
            << " Undeclared identifier " 
            << name << ".\n";
        type = Object;
        return type;
    }
    type = *attr_table.lookup(name);
    return type;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }

    install_methods();
    install_attributes_and_check_features_type();
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

