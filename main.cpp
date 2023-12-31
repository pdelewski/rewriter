#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

#include <iostream>
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolingSampleCategory("CLifter");

class CLifterVisitor : public RecursiveASTVisitor<CLifterVisitor> {
  public:
    explicit CLifterVisitor(ASTContext *Context, Rewriter &R)
        : Context(Context), TheRewriter(R) {}

    bool VisitCompoundStmt(CompoundStmt *stmt) {
        dumpParents(stmt);
        return true;
    }

    bool VisitFunctionDecl(FunctionDecl *f) {
        addOutParam(f);
        replaceReturnType(f);
        return true;
    }

  private:
    void addOutParam(FunctionDecl *f) {
        QualType returnType = f->getReturnType();

        if (returnType.getAsString() == "void") {
            return;
        }
        FunctionTypeLoc functionTypeLoc = f->getFunctionTypeLoc();
        SourceLocation rparenLoc = functionTypeLoc.getRParenLoc();
        std::stringstream SSOutParam;
        if (f->getNumParams() > 0) {
            SSOutParam << ", ";
            SSOutParam << returnType.getAsString();
            SSOutParam << "* " << "__returnVal";
        } else {
            SSOutParam << returnType.getAsString();
            SSOutParam << "* __returnVal";
        }
        TheRewriter.InsertText(rparenLoc, SSOutParam.str(), true, true);
    }
    void replaceReturnType(FunctionDecl *f) {
        auto returnTypeSourceRange = f->getReturnTypeSourceRange();
        auto rangeSize = TheRewriter.getRangeSize(returnTypeSourceRange);
        std::stringstream ss;
        ss << "void";
        TheRewriter.ReplaceText(returnTypeSourceRange.getBegin(), rangeSize,
                                ss.str());
    }
    void dumpParents(Stmt *stmt) {
        const auto &parents = Context->getParents(*stmt);
        std::cout << "parents size " << parents.size() << ": \n";

        if (!parents.empty()) {
            for (int i = 0; i < parents.size(); i++) {
                std::cout << "parent at " << i << ": \n";
                const Stmt *parentStmt = parents[i].get<Stmt>();
                if (parentStmt) {
                    parentStmt->dump();
                }
                const FunctionDecl *decl = parents[i].get<FunctionDecl>();
                if (decl) {
                    decl->dump();
                }
            }
        }
    }
    ASTContext *Context;
    Rewriter &TheRewriter;
};

class CLifterConsumer : public clang::ASTConsumer {
  public:
    explicit CLifterConsumer(ASTContext *Context, Rewriter &R)
        : Visitor(Context, R) {}

    virtual void HandleTranslationUnit(clang::ASTContext &Context) {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }

  private:
    CLifterVisitor Visitor;
};

class CLifterClassAction : public clang::ASTFrontendAction {
  public:
    virtual std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance &Compiler,
                      llvm::StringRef InFile) override {
        TheRewriter.setSourceMgr(Compiler.getSourceManager(),
                                 Compiler.getLangOpts());
        return std::make_unique<CLifterConsumer>(&Compiler.getASTContext(),
                                                 TheRewriter);
    }

    void EndSourceFileAction() override {
        SourceManager &SM = TheRewriter.getSourceMgr();
        llvm::errs() << "** EndSourceFileAction for: "
                     << SM.getFileEntryForID(SM.getMainFileID())->getName()
                     << "\n";

        // Now emit the rewritten buffer.
        TheRewriter.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
    }

  private:
    Rewriter TheRewriter;
};

int main(int argc, const char **argv) {
    llvm::Expected<clang::tooling::CommonOptionsParser> op =
        CommonOptionsParser::create(argc, argv, ToolingSampleCategory);
    //  CommonOptionsParser op (argc, argv, ToolingSampleCategory);
    ClangTool Tool(op->getCompilations(), op->getSourcePathList());

    // ClangTool::run accepts a FrontendActionFactory, which is then used to
    // create new objects implementing the FrontendAction interface. Here we use
    // the helper newFrontendActionFactory to create a default factory that will
    // return a new MyFrontendAction object every time.
    // To further customize this, we could create our own factory class.
    return Tool.run(newFrontendActionFactory<CLifterClassAction>().get());
}
