#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <queue>
#include <cctype>
using namespace std;

struct ASTNode {
    string text;
    ASTNode *leftChild;
    ASTNode *rightSibling;
    ASTNode(const string &t) : text(t), leftChild(nullptr), rightSibling(nullptr) {}
};

struct Token {
    string text;
    int line;
    bool isString;
    char quote;
    Token(string t="", int l=1, bool s=false, char q=0) : text(t), line(l), isString(s), quote(q) {}
};

static vector<Token> toks;
static size_t posi = 0;
static ASTNode *root = nullptr;
static ASTNode *lastTop = nullptr;

static bool isDataType(const string &s) { return s == "int" || s == "char" || s == "bool"; }
static bool isIdStart(char c) { return isalpha((unsigned char)c) || c == '_'; }
static bool isIdChar(char c) { return isalnum((unsigned char)c) || c == '_'; }

static void addTop(ASTNode *n) {
    if (!root) root = n;
    else lastTop->rightSibling = n;
    lastTop = n;
}

static ASTNode* addNode(const string &s) {
    ASTNode *n = new ASTNode(s);
    addTop(n);
    return n;
}

static string join(const vector<string> &v) {
    string out;
    for (size_t i = 0; i < v.size(); i++) {
        if (i) out += "   ";
        out += v[i];
    }
    return out;
}

static vector<Token> lex(const string &Source) {
    vector<Token> out;
    int line = 1;
    size_t i = 0;
    while (i < Source.size()) {
        char c = Source[i];
        if (isspace((unsigned char)c)) { if (c == '\n') line++; i++; continue; }
        if (c == '/' && i + 1 < Source.size() && Source[i+1] == '/') {
            i += 2;
            while (i < Source.size() && Source[i] != '\n') i++;
            continue;
        }
        if (c == '/' && i + 1 < Source.size() && Source[i+1] == '*') {
            int startLine = line;
            i += 2;
            bool closed = false;
            while (i < Source.size()) {
                if (Source[i] == '\n') line++;
                if (Source[i] == '*' && i + 1 < Source.size() && Source[i+1] == '/') { i += 2; closed = true; break; }
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
            while (i < Source.size()) {
                if (Source[i] == '\\' && i + 1 < Source.size()) {
                    val += Source[i];
                    val += Source[i+1];
                    i += 2;
                    continue;
                }
                if (Source[i] == q) { i++; break; }
                if (Source[i] == '\n') line++;
                val += Source[i++];
            }
            out.push_back(Token(val, line, true, q));
            continue;
        }
        if (isIdStart(c)) {
            string s;
            while (i < Source.size() && isIdChar(Source[i])) s += Source[i++];
            out.push_back(Token(s, line));
            continue;
        }
        if (isdigit((unsigned char)c) || (c == '-' && i + 1 < Source.size() && isdigit((unsigned char)Source[i+1]))) {
            string s;
            s += Source[i++];
            while (i < Source.size() && isdigit((unsigned char)Source[i])) s += Source[i++];
            out.push_back(Token(s, line));
            continue;
        }
        if (i + 1 < Source.size()) {
            string two = Source.substr(i, 2);
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

static Token peek(int k=0) { return toks[posi + k]; }
static bool at(const string &s) { return peek().text == s && !peek().isString; }
static Token getTok() { return toks[posi++]; }
static bool match(const string &s) { if (at(s)) { posi++; return true; } return false; }

static string tokenOut(const Token &t) {
    if (!t.isString) return t.text;
    vector<string> v;
    v.push_back(string(1, t.quote));
    v.push_back(t.text);
    v.push_back(string(1, t.quote));
    return join(v);
}

static bool isOperator(const string &s) {
    return s == "!" || s == "^" || s == "*" || s == "/" || s == "%" || s == "+" || s == "-" ||
           s == "<" || s == "<=" || s == ">" || s == ">=" || s == "==" || s == "!=" ||
           s == "&&" || s == "||";
}

static int prec(const string &s) {
    if (s == "!") return 7;
    if (s == "^") return 6;
    if (s == "*" || s == "/" || s == "%") return 5;
    if (s == "+" || s == "-") return 4;
    if (s == "<" || s == "<=" || s == ">" || s == ">=") return 3;
    if (s == "==" || s == "!=") return 2;
    if (s == "&&") return 1;
    if (s == "||") return 0;
    return -1;
}

static bool rightAssoc(const string &s) { return s == "!" || s == "^"; }

static bool isFunctionLike(const vector<Token> &v, int i) {
    if (i + 1 >= (int)v.size()) return false;
    if (v[i].isString) return false;
    if (v[i+1].text != "(") return false;
    string name = v[i].text;
    if (name == "if" || name == "while" || name == "for" || name == "printf") return false;
    return isIdStart(name[0]);
}

static vector<string> postfixExpr(const vector<Token> &v) {
    vector<string> output;
    vector<string> ops;
    for (int i = 0; i < (int)v.size(); i++) {
        Token t = v[i];
        string s = t.text;
        if (t.isString) {
            output.push_back(string(1, t.quote));
            output.push_back(t.text);
            output.push_back(string(1, t.quote));
        }
        else if (isFunctionLike(v, i)) {
            int depth = 0;
            output.push_back(s);
            while (i + 1 < (int)v.size()) {
                i++;
                output.push_back(v[i].isString ? tokenOut(v[i]) : v[i].text);
                if (v[i].text == "(") depth++;
                else if (v[i].text == ")") {
                    depth--;
                    if (depth == 0) break;
                }
            }
        }
        else if (s == "[") {
            output.push_back("[");
            i++;
            vector<Token> inside;
            int depth = 1;
            while (i < (int)v.size() && depth > 0) {
                if (v[i].text == "[") depth++;
                else if (v[i].text == "]") {
                    depth--;
                    if (depth == 0) break;
                }
                inside.push_back(v[i]);
                i++;
            }
            vector<string> inPost = postfixExpr(inside);
            output.insert(output.end(), inPost.begin(), inPost.end());
            output.push_back("]");
        }
        else if (s == "(") {
            ops.push_back(s);
        }
        else if (s == ")") {
            while (!ops.empty() && ops.back() != "(") { output.push_back(ops.back()); ops.pop_back(); }
            if (!ops.empty()) ops.pop_back();
        }
        else if (isOperator(s)) {
            while (!ops.empty() && isOperator(ops.back()) &&
                   ((rightAssoc(s) && prec(s) < prec(ops.back())) || (!rightAssoc(s) && prec(s) <= prec(ops.back())))) {
                output.push_back(ops.back());
                ops.pop_back();
            }
            ops.push_back(s);
        }
        else if (s == ",") {
            // top-level comma only used in calls/printf; it is usually handled outside expressions
        }
        else {
            output.push_back(s);
        }
    }
    while (!ops.empty()) { if (ops.back() != "(") output.push_back(ops.back()); ops.pop_back(); }
    return output;
}

static vector<Token> collectUntilTop(const string &stop) {
    vector<Token> v;
    int par = 0, br = 0;
    while (!at("EOF")) {
        if (par == 0 && br == 0 && at(stop)) break;
        Token t = getTok();
        if (!t.isString) {
            if (t.text == "(") par++;
            else if (t.text == ")") par--;
            else if (t.text == "[") br++;
            else if (t.text == "]") br--;
        }
        v.push_back(t);
    }
    return v;
}

static vector<Token> collectParenContents() {
    vector<Token> v;
    match("(");
    int depth = 1;
    while (!at("EOF") && depth > 0) {
        Token t = getTok();
        if (!t.isString) {
            if (t.text == "(") depth++;
            else if (t.text == ")") {
                depth--;
                if (depth == 0) break;
            }
        }
        v.push_back(t);
    }
    return v;
}

static vector<string> lhsTokens(const vector<Token> &v) {
    vector<string> out;
    for (const Token &t : v) {
        if (t.isString) out.push_back(tokenOut(t));
        else out.push_back(t.text);
    }
    return out;
}

static void parseStatement();

static void parseDeclarationStmt(bool alreadySawType=false) {
    if (!alreadySawType) getTok();
    int count = 0;
    while (!at(";") && !at("EOF")) {
        Token t = getTok();
        if (!t.isString && isIdStart(t.text[0])) {
            count++;
            if (at("[")) {
                while (!at("]") && !at("EOF")) getTok();
                match("]");
            }
        }
    }
    match(";");
    if (count == 0) count = 1;
    for (int i = 0; i < count; i++) addNode("DECLARATION");
}

static vector<string> rhsOutput(const vector<Token> &rhs) {
    if (rhs.size() == 1 && rhs[0].isString) {
        vector<string> out;
        out.push_back(string(1, rhs[0].quote));
        out.push_back(rhs[0].text);
        out.push_back(string(1, rhs[0].quote));
        return out;
    }
    return postfixExpr(rhs);
}

static void parseAssignmentOrCall() {
    // call statement: id (...);
    if (!peek().isString && peek(1).text == "(") {
        string name = getTok().text;
        vector<Token> args = collectParenContents();
        match(";");
        vector<string> out;
        out.push_back("CALL"); out.push_back(name); out.push_back("(");
        for (const Token &t : args) {
            if (t.text == "," && !t.isString) continue;
            if (t.isString) out.push_back(t.text);
            else out.push_back(t.text);
        }
        out.push_back(")");
        addNode(join(out));
        return;
    }

    vector<Token> left;
    while (!at("=") && !at("EOF")) left.push_back(getTok());
    match("=");
    vector<Token> rhs = collectUntilTop(";");
    match(";");
    vector<string> out;
    out.push_back("ASSIGNMENT");
    vector<string> l = lhsTokens(left);
    out.insert(out.end(), l.begin(), l.end());
    vector<string> r = rhsOutput(rhs);
    out.insert(out.end(), r.begin(), r.end());
    out.push_back("=");
    addNode(join(out));
}

static void parsePrintf() {
    getTok();
    match("(");
    vector<string> out;
    out.push_back("PRINTF");
    if (peek().isString) {
        Token fmt = getTok();
        out.push_back(fmt.text);
    }
    while (!at(")") && !at("EOF")) {
        if (match(",")) continue;
        Token t = getTok();
        if (t.isString) out.push_back(t.text);
        else out.push_back(t.text);
    }
    match(")"); match(";");
    addNode(join(out));
}

static void parseReturn() {
    getTok();
    vector<Token> expr = collectUntilTop(";");
    match(";");
    vector<string> out;
    out.push_back("RETURN");
    vector<string> p = rhsOutput(expr);
    out.insert(out.end(), p.begin(), p.end());
    addNode(join(out));
}

static void parseBlock() {
    match("{");
    addNode("BEGIN BLOCK");
    while (!at("}") && !at("EOF")) parseStatement();
    match("}");
    addNode("END BLOCK");
}

static void parseIf() {
    getTok();
    vector<Token> cond = collectParenContents();
    vector<string> out;
    out.push_back("IF");
    vector<string> p = postfixExpr(cond);
    out.insert(out.end(), p.begin(), p.end());
    addNode(join(out));
    parseStatement();
    if (match("else")) {
        addNode("ELSE");
        parseStatement();
    }
}

static void parseWhile() {
    getTok();
    vector<Token> cond = collectParenContents();
    vector<string> out;
    out.push_back("WHILE");
    vector<string> p = postfixExpr(cond);
    out.insert(out.end(), p.begin(), p.end());
    addNode(join(out));
    parseStatement();
}

static void parseFor() {
    getTok();
    match("(");
    vector<Token> e1 = collectUntilTop(";"); match(";");
    vector<Token> e2 = collectUntilTop(";"); match(";");
    vector<Token> e3 = collectUntilTop(")"); match(")");

    auto emitForAssign = [](const string &label, const vector<Token> &v) {
        vector<string> out;
        out.push_back(label);
        vector<Token> left, right;
        int eq = -1;
        for (int i = 0; i < (int)v.size(); i++) if (v[i].text == "=" && !v[i].isString) { eq = i; break; }
        if (eq >= 0) {
            for (int i = 0; i < eq; i++) left.push_back(v[i]);
            for (int i = eq + 1; i < (int)v.size(); i++) right.push_back(v[i]);
            vector<string> l = lhsTokens(left);
            vector<string> r = rhsOutput(right);
            out.insert(out.end(), l.begin(), l.end());
            out.insert(out.end(), r.begin(), r.end());
            out.push_back("=");
        } else {
            vector<string> p = postfixExpr(v);
            out.insert(out.end(), p.begin(), p.end());
        }
        addNode(join(out));
    };
    emitForAssign("FOR EXPRESSION 1", e1);
    vector<string> out2; out2.push_back("FOR EXPRESSION 2");
    vector<string> p2 = postfixExpr(e2); out2.insert(out2.end(), p2.begin(), p2.end()); addNode(join(out2));
    emitForAssign("FOR EXPRESSION 3", e3);
    parseStatement();
}

static void parseStatement() {
    if (at("EOF")) return;
    if (at("{")) { parseBlock(); return; }
    if (at(";")) { getTok(); return; }
    if (at("if")) { parseIf(); return; }
    if (at("while")) { parseWhile(); return; }
    if (at("for")) { parseFor(); return; }
    if (at("printf")) { parsePrintf(); return; }
    if (at("return")) { parseReturn(); return; }
    if (isDataType(peek().text)) { parseDeclarationStmt(); return; }
    parseAssignmentOrCall();
}

static void skipParameterList() {
    match("(");
    int depth = 1;
    while (!at("EOF") && depth > 0) {
        Token t = getTok();
        if (!t.isString) {
            if (t.text == "(") depth++;
            else if (t.text == ")") depth--;
        }
    }
}

static void parseTopLevel() {
    while (!at("EOF")) {
        if (at("function")) {
            getTok(); // function
            if (isDataType(peek().text)) getTok();
            if (!at("EOF")) getTok(); // name
            skipParameterList();
            addNode("DECLARATION");
            parseBlock();
        }
        else if (at("procedure")) {
            getTok(); // procedure
            if (!at("EOF")) getTok(); // name
            skipParameterList();
            addNode("DECLARATION");
            parseBlock();
        }
        else if (isDataType(peek().text)) {
            parseDeclarationStmt();
        }
        else {
            // unknown top-level token: consume safely
            getTok();
        }
    }
}

static void printBreadthFirst(ASTNode *r) {

    for (ASTNode *cur = r; cur != nullptr; cur = cur->rightSibling) {
        cout << cur->text << "\n";
    }
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
    parseTopLevel();
    printBreadthFirst(root);
    return 0;
}
