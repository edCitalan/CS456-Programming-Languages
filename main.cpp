#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>
#include <cstdlib>
#include <cctype>
using namespace std;

struct Token {
    string text;
    int line;
    bool isString;
    char quote;
    Token(string t="", int l=1, bool s=false, char q=0) : text(t), line(l), isString(s), quote(q) {}
};

static bool isIdStart(char c) { return isalpha((unsigned char)c) || c == '_'; }
static bool isIdChar(char c) { return isalnum((unsigned char)c) || c == '_'; }
static bool isDataType(const string &s) { return s == "int" || s == "char" || s == "bool"; }

static vector<Token> lex(const string &source) {
    vector<Token> out;
    int line = 1;
    size_t i = 0;
    while (i < source.size()) {
        char c = source[i];
        if (isspace((unsigned char)c)) { if (c == '\n') line++; i++; continue; }
        if (c == '/' && i + 1 < source.size() && source[i+1] == '/') {
            i += 2;
            while (i < source.size() && source[i] != '\n') i++;
            continue;
        }
        if (c == '/' && i + 1 < source.size() && source[i+1] == '*') {
            int startLine = line;
            i += 2;
            bool closed = false;
            while (i < source.size()) {
                if (source[i] == '\n') line++;
                if (source[i] == '*' && i + 1 < source.size() && source[i+1] == '/') {
                    i += 2;
                    closed = true;
                    break;
                }
                i++;
            }
            if (!closed) {
                cerr << "ERROR: Program contains C-style, unterminated comment on line " << startLine << "\n";
                exit(1);
            }
            continue;
        }
        if (c == '"' || c == '\'') {
            char q = c;
            i++;
            string val;
            while (i < source.size()) {
                if (source[i] == '\\' && i + 1 < source.size()) {
                    val += source[i];
                    val += source[i+1];
                    i += 2;
                    continue;
                }
                if (source[i] == q) { i++; break; }
                if (source[i] == '\n') line++;
                val += source[i++];
            }
            out.push_back(Token(val, line, true, q));
            continue;
        }
        if (isIdStart(c)) {
            string s;
            while (i < source.size() && isIdChar(source[i])) s += source[i++];
            out.push_back(Token(s, line));
            continue;
        }
        if (isdigit((unsigned char)c)) {
            string s;
            while (i < source.size() && isdigit((unsigned char)source[i])) s += source[i++];
            out.push_back(Token(s, line));
            continue;
        }
        if (i + 1 < source.size()) {
            string two = source.substr(i, 2);
            if (two == "<=" || two == ">=" || two == "==" || two == "!=" || two == "&&" || two == "||") {
                out.push_back(Token(two, line));
                i += 2;
                continue;
            }
        }
        out.push_back(Token(string(1, c), line));
        i++;
    }
    out.push_back(Token("EOF", line));
    return out;
}

struct Expr {
    string kind, text;
    vector<Expr*> kids;

    Expr(string k = "", string t = "") {
        kind = k;
        text = t;
    }
};
struct DeclInfo {
    string type;
    string name;
    int arraySize;

    DeclInfo(string t = "", string n = "", int a = 0) {
        type = t;
        name = n;
        arraySize = a;
    }
};

struct Statement {
    string kind;
    vector<DeclInfo> decls;
    string name;
    Expr *expr1;
    Expr *expr2;
    Expr *expr3;
    vector<Expr*> args;
    vector<Statement*> body;
    vector<Statement*> elseBody;
    Statement *singleStatement;
    Statement *elseStatement;

    Statement() {
        expr1 = nullptr;
        expr2 = nullptr;
        expr3 = nullptr;
        singleStatement = nullptr;
        elseStatement = nullptr;
    }
};

struct Function {
    string name;
    string returnType;
    bool isFunction;
    vector<DeclInfo> params;
    vector<Statement*> body;
};

vector<Token> toks;
size_t currentToken = 0;
map<string, Function*> functions;
vector<DeclInfo> globalDecls;

Token peek(int k = 0) {
    return toks[currentToken + k];
}

bool at(const string &s) {
    return !peek().isString && peek().text == s;
}

Token getTok() {
    Token t = toks[currentToken];
    currentToken++;
    return t;
}

bool match(const string &s) {
    if (at(s)) {
        currentToken++;
        return true;
    }

    return false;
}

Expr* makeNode(string kind, string text = "", Expr *first = nullptr, Expr *second = nullptr) {
    Expr *e = new Expr(kind, text);

    if (first != nullptr) {
        e->kids.push_back(first);
    }

    if (second != nullptr) {
        e->kids.push_back(second);
    }

    return e;
}
struct Parser {
    Expr* parseExpression() { return parseOr(); }

    Expr* parseOr() {
        Expr *left = parseAnd();
        while (match("||")) left = makeNode("binary", "||", left, parseAnd());
        return left;
    }
    Expr* parseAnd() {
        Expr *left = parseEquality();
        while (match("&&")) left = makeNode("binary", "&&", left, parseEquality());
        return left;
    }
    Expr* parseEquality() {
        Expr *left = parseRelation();
        while (at("==") || at("!=")) {
            string op = getTok().text;
            left = makeNode("binary", op, left, parseRelation());
        }
        return left;
    }
    Expr* parseRelation() {
        Expr *left = parseAdd();
        while (at("<") || at("<=") || at(">") || at(">=")) {
            string op = getTok().text;
            left = makeNode("binary", op, left, parseAdd());
        }
        return left;
    }
    Expr* parseAdd() {
        Expr *left = parseMul();
        while (at("+") || at("-")) {
            string op = getTok().text;
            left = makeNode("binary", op, left, parseMul());
        }
        return left;
    }
    Expr* parseMul() {
        Expr *left = parseUnary();
        while (at("*") || at("/") || at("%")) {
            string op = getTok().text;
            left = makeNode("binary", op, left, parseUnary());
        }
        return left;
    }
    Expr* parseUnary() {
        if (match("!")) return makeNode("unary", "!", parseUnary());
        if (match("-")) return makeNode("unary", "-", parseUnary());
        if (match("+")) return parseUnary();
        return parsePrimary();
    }
    Expr* parsePrimary() {
        if (match("(")) {
            Expr *e = parseExpression();
            match(")");
            return e;
        }
        Token t = getTok();
        if (t.isString) {
            Expr *e = new Expr(t.quote == '\'' ? "char" : "string", t.text);
            return e;
        }
        if (t.text == "TRUE" || t.text == "FALSE") return new Expr("number", t.text == "TRUE" ? "1" : "0");
        if (!t.text.empty() && isdigit((unsigned char)t.text[0])) return new Expr("number", t.text);
        if (at("(")) {
            Expr *call = new Expr("call", t.text);
            match("(");
            if (!at(")")) {
                while (true) {
                    call->kids.push_back(parseExpression());
                    if (!match(",")) break;
                }
            }
            match(")");
            return call;
        }
        if (at("[")) {
            Expr *arr = new Expr("array", t.text);
            match("[");
            arr->kids.push_back(parseExpression());
            match("]");
            return arr;
        }
        return new Expr("var", t.text);
    }

    vector<Statement*> parseBlockStatements() {
        vector<Statement*> list;
        match("{");
        while (!at("}") && !at("EOF")) list.push_back(parseStatement());
        match("}");
        return list;
    }

    vector<DeclInfo> parseDeclList(const string &type) {
        vector<DeclInfo> ds;
        while (!at(";") && !at(")") && !at("EOF")) {
            if (at(",")) { getTok(); continue; }
            string name = getTok().text;
            int sz = 0;
            if (match("[")) {
                if (!peek().isString) sz = atoi(getTok().text.c_str());
                match("]");
            }
            ds.push_back(DeclInfo(type, name, sz));
        }
        return ds;
    }

    Statement* parseAssignmentNoSemi() {
        Statement *s = new Statement();
        s->kind = "assign";
        string name = getTok().text;
        Expr *left = new Expr("var", name);
        if (match("[")) {
            left = new Expr("array", name);
            left->kids.push_back(parseExpression());
            match("]");
        }
        match("=");
        s->expr1 = left;
        s->expr2 = parseExpression();
        return s;
    }

    Statement* parseStatement() {
        if (match(";")) return new Statement();
        if (at("{")) {
            Statement *s = new Statement();
            s->kind = "block";
            s->body = parseBlockStatements();
            return s;
        }
        if (isDataType(peek().text)) {
            string type = getTok().text;
            Statement *s = new Statement();
            s->kind = "decl";
            s->decls = parseDeclList(type);
            match(";");
            return s;
        }
        if (match("if")) {
            Statement *s = new Statement();
            s->kind = "if";
            match("(");
            s->expr1 = parseExpression();
            match(")");
            s->singleStatement = parseStatement();
            if (match("else")) s->elseStatement = parseStatement();
            return s;
        }
        if (match("while")) {
            Statement *s = new Statement();
            s->kind = "while";
            match("(");
            s->expr1 = parseExpression();
            match(")");
            s->singleStatement = parseStatement();
            return s;
        }
        if (match("for")) {
            Statement *s = new Statement();
            s->kind = "for";
            match("(");
            s->singleStatement = parseAssignmentNoSemi();
            match(";");
            s->expr1 = parseExpression();
            match(";");
            s->elseStatement = parseAssignmentNoSemi();
            match(")");
            Statement *body = parseStatement();
            s->body.push_back(body);
            return s;
        }
        if (match("printf")) {
            Statement *s = new Statement();
            s->kind = "printf";
            match("(");
            if (peek().isString) {
                Token fmt = getTok();
                s->expr1 = new Expr("string", fmt.text);
            }
            while (match(",")) s->args.push_back(parseExpression());
            match(")");
            match(";");
            return s;
        }
        if (match("return")) {
            Statement *s = new Statement();
            s->kind = "return";
            s->expr1 = parseExpression();
            match(";");
            return s;
        }
        if (!peek().isString && peek(1).text == "(") {
            Statement *s = new Statement();
            s->kind = "callStatement";
            s->name = getTok().text;
            match("(");
            if (!at(")")) {
                while (true) {
                    s->args.push_back(parseExpression());
                    if (!match(",")) break;
                }
            }
            match(")");
            match(";");
            return s;
        }
        Statement *s = parseAssignmentNoSemi();
        match(";");
        return s;
    }

    void parseProgram() {
        while (!at("EOF")) {
            if (match("function")) {
                Function *f = new Function();
                f->isFunction = true;
                f->returnType = getTok().text;
                f->name = getTok().text;
                match("(");
                if (!match("void")) {
                    while (!at(")") && !at("EOF")) {
                        string type = getTok().text;
                        vector<DeclInfo> p = parseDeclList(type);
                        f->params.insert(f->params.end(), p.begin(), p.end());
                        match(",");
                    }
                }
                match(")");
                f->body = parseBlockStatements();
                functions[f->name] = f;
            }
            else if (match("procedure")) {
                Function *f = new Function();
                f->isFunction = false;
                f->returnType = "void";
                f->name = getTok().text;
                match("(");
                if (!match("void")) {
                    while (!at(")") && !at("EOF")) {
                        string type = getTok().text;
                        vector<DeclInfo> p = parseDeclList(type);
                        f->params.insert(f->params.end(), p.begin(), p.end());
                        match(",");
                    }
                }
                match(")");
                f->body = parseBlockStatements();
                functions[f->name] = f;
            }
            else if (isDataType(peek().text)) {
                string type = getTok().text;
                vector<DeclInfo> ds = parseDeclList(type);
                globalDecls.insert(globalDecls.end(), ds.begin(), ds.end());
                match(";");
            }
            else getTok();
        }
    }
};

struct Value {
    int num;
    vector<int> arr;
    bool isArray;
    Value(int n=0) : num(n), isArray(false) {}
};

struct Variable {
    string type;
    int arraySize;
    Value val;
};

struct Frame {
    map<string, Variable> vars;
};

struct ReturnSignal {
    bool hasReturn;
    Value val;
    ReturnSignal() : hasReturn(false), val(0) {}
};

static vector<Frame> frames;

static int hexVal(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0;
}

static vector<int> decodeString(const string &s) {
    vector<int> r;
    for (size_t i = 0; i < s.size(); i++) {
        if (s[i] == '\\' && i + 1 < s.size()) {
            char n = s[++i];
            if (n == 'n') r.push_back('\n');
            else if (n == 't') r.push_back('\t');
            else if (n == 'r') r.push_back('\r');
            else if (n == '0') r.push_back(0);
            else if (n == 'x') {
                int v = 0;
                if (i + 1 < s.size()) { v = hexVal(s[++i]); }
                if (i + 1 < s.size() && isxdigit((unsigned char)s[i+1])) { v = v * 16 + hexVal(s[++i]); }
                r.push_back(v);
            }
            else r.push_back(n);
        } else r.push_back((unsigned char)s[i]);
    }
    return r;
}

static int decodeChar(const string &s) {
    vector<int> v = decodeString(s);
    if (v.empty()) return 0;
    return v[0];
}

static Variable* findVar(const string &name) {
    for (int i = (int)frames.size() - 1; i >= 0; i--) {
        map<string, Variable>::iterator it = frames[i].vars.find(name);
        if (it != frames[i].vars.end()) return &it->second;
    }
    return nullptr;
}

static void addVariable(const DeclInfo &d) {
    Variable v;
    v.type = d.type;
    v.arraySize = d.arraySize;
    v.val.num = 0;
    v.val.isArray = d.arraySize > 0;
    if (d.arraySize > 0) v.val.arr.assign(d.arraySize, 0);
    frames.back().vars[d.name] = v;
}

static Value evalExpr(Expr *e);
static ReturnSignal execStatement(Statement *s);
static ReturnSignal execList(const vector<Statement*> &list);
static Value callFunction(const string &name, const vector<Value> &args);

static Value getArrayValue(const string &name, int idx) {
    Variable *v = findVar(name);
    if (!v || !v->val.isArray) return Value(0);
    if (idx < 0 || idx >= (int)v->val.arr.size()) return Value(0);
    return Value(v->val.arr[idx]);
}

static int asInt(const Value &v) { return v.num; }

static Value evalExpr(Expr *e) {
    if (!e) return Value(0);
    if (e->kind == "number") return Value(atoi(e->text.c_str()));
    if (e->kind == "char") return Value(decodeChar(e->text));
    if (e->kind == "string") {
        Value v;
        v.isArray = true;
        v.arr = decodeString(e->text);
        return v;
    }
    if (e->kind == "var") {
        Variable *v = findVar(e->text);
        if (!v) return Value(0);
        return v->val;
    }
    if (e->kind == "array") {
        int idx = asInt(evalExpr(e->kids[0]));
        return getArrayValue(e->text, idx);
    }
    if (e->kind == "call") {
        vector<Value> vals;
        for (size_t i = 0; i < e->kids.size(); i++) vals.push_back(evalExpr(e->kids[i]));
        return callFunction(e->text, vals);
    }
    if (e->kind == "unary") {
        int a = asInt(evalExpr(e->kids[0]));
        if (e->text == "!") return Value(!a);
        if (e->text == "-") return Value(-a);
    }
    if (e->kind == "binary") {
        int a = asInt(evalExpr(e->kids[0]));
        int b = asInt(evalExpr(e->kids[1]));
        if (e->text == "+") return Value(a + b);
        if (e->text == "-") return Value(a - b);
        if (e->text == "*") return Value(a * b);
        if (e->text == "/") return Value(b == 0 ? 0 : a / b);
        if (e->text == "%") return Value(b == 0 ? 0 : a % b);
        if (e->text == "<") return Value(a < b);
        if (e->text == "<=") return Value(a <= b);
        if (e->text == ">") return Value(a > b);
        if (e->text == ">=") return Value(a >= b);
        if (e->text == "==") return Value(a == b);
        if (e->text == "!=") return Value(a != b);
        if (e->text == "&&") return Value(a && b);
        if (e->text == "||") return Value(a || b);
    }
    return Value(0);
}

static void assignTo(Expr *left, const Value &right) {
    if (left->kind == "var") {
        Variable *v = findVar(left->text);
        if (!v) return;
        if (v->val.isArray && right.isArray) {
            for (size_t i = 0; i < v->val.arr.size(); i++) v->val.arr[i] = 0;
            for (size_t i = 0; i < right.arr.size() && i < v->val.arr.size(); i++) v->val.arr[i] = right.arr[i];
        } else {
            v->val.num = right.num;
        }
    }
    else if (left->kind == "array") {
        Variable *v = findVar(left->text);
        if (!v || !v->val.isArray) return;
        int idx = asInt(evalExpr(left->kids[0]));
        if (idx >= 0 && idx < (int)v->val.arr.size()) v->val.arr[idx] = right.num;
    }
}

static string arrayAsString(const Value &v) {
    string s;
    for (size_t i = 0; i < v.arr.size(); i++) {
        if (v.arr[i] == 0) break;
        s += (char)v.arr[i];
    }
    return s;
}

static void doPrintf(Statement *s) {
    string fmt = s->expr1 ? s->expr1->text : "";
    vector<Value> args;
    for (size_t i = 0; i < s->args.size(); i++) args.push_back(evalExpr(s->args[i]));
    size_t ai = 0;
    for (size_t i = 0; i < fmt.size(); i++) {
        if (fmt[i] == '\\' && i + 1 < fmt.size()) {
            char n = fmt[++i];
            if (n == 'n') cout << '\n';
            else if (n == 't') cout << '\t';
            else cout << n;
        }
        else if (fmt[i] == '%' && i + 1 < fmt.size()) {
            char f = fmt[++i];
            if (f == 'd') {
                if (ai < args.size()) cout << args[ai++].num;
            }
            else if (f == 'c') {
                if (ai < args.size()) cout << (char)args[ai++].num;
            }
            else if (f == 's') {
                if (ai < args.size()) cout << arrayAsString(args[ai++]);
            }
            else cout << f;
        }
        else cout << fmt[i];
    }
}

static ReturnSignal execBlockOrStatement(Statement *s) {
    if (!s) return ReturnSignal();
    return execStatement(s);
}

static ReturnSignal execStatement(Statement *s) {
    ReturnSignal ret;
    if (!s || s->kind == "") return ret;
    if (s->kind == "decl") {
        for (size_t i = 0; i < s->decls.size(); i++) addVariable(s->decls[i]);
    }
    else if (s->kind == "assign") {
        assignTo(s->expr1, evalExpr(s->expr2));
    }
    else if (s->kind == "printf") {
        doPrintf(s);
    }
    else if (s->kind == "return") {
        ret.hasReturn = true;
        ret.val = evalExpr(s->expr1);
        return ret;
    }
    else if (s->kind == "callStatement") {
        vector<Value> vals;
        for (size_t i = 0; i < s->args.size(); i++) vals.push_back(evalExpr(s->args[i]));
        callFunction(s->name, vals);
    }
    else if (s->kind == "block") {
        ret = execList(s->body);
        if (ret.hasReturn) return ret;
    }
    else if (s->kind == "if") {
        if (asInt(evalExpr(s->expr1))) ret = execBlockOrStatement(s->singleStatement);
        else ret = execBlockOrStatement(s->elseStatement);
        if (ret.hasReturn) return ret;
    }
    else if (s->kind == "while") {
        while (asInt(evalExpr(s->expr1))) {
            ret = execBlockOrStatement(s->singleStatement);
            if (ret.hasReturn) return ret;
        }
    }
    else if (s->kind == "for") {
        execStatement(s->singleStatement);
        while (asInt(evalExpr(s->expr1))) {
            if (!s->body.empty()) {
                ret = execBlockOrStatement(s->body[0]);
                if (ret.hasReturn) return ret;
            }
            execStatement(s->elseStatement);
        }
    }
    return ret;
}

static ReturnSignal execList(const vector<Statement*> &list) {
    ReturnSignal ret;
    for (size_t i = 0; i < list.size(); i++) {
        ret = execStatement(list[i]);
        if (ret.hasReturn) return ret;
    }
    return ret;
}

static Value callFunction(const string &name, const vector<Value> &args) {
    map<string, Function*>::iterator it = functions.find(name);
    if (it == functions.end()) return Value(0);
    Function *f = it->second;
    Frame fr;
    frames.push_back(fr);
    for (size_t i = 0; i < f->params.size(); i++) {
        addVariable(f->params[i]);
        Variable *v = findVar(f->params[i].name);
        if (v && i < args.size()) v->val = args[i];
    }
    ReturnSignal ret = execList(f->body);
    frames.pop_back();
    if (ret.hasReturn) return ret.val;
    return Value(0);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " input_file\n";
        return 1;
    }
    ifstream in(argv[1]);
    if (!in) {
        cerr << "Could not open input file.\n";
        return 1;
    }
    stringstream buffer;
    buffer << in.rdbuf();
    toks = lex(buffer.str());
    Parser p;
    p.parseProgram();
    Frame globals;
    frames.push_back(globals);
    for (size_t i = 0; i < globalDecls.size(); i++) addVariable(globalDecls[i]);
    callFunction("main", vector<Value>());
    return 0;
}
