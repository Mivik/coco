
#include <algorithm>
#include <cerrno>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <vector>

#include <clang/AST/ASTConsumer.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Format/Format.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/Core/Replacement.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;

namespace fs = std::filesystem;

const char endl = '\n';

inline bool starts_with(std::string &str, const std::string &what) {
	if (what.size() > str.size()) return false;
	for (size_t i = 0; i < what.size(); ++i) if (str[i] != what[i]) return false;
	return true;
}

inline std::string read_string(std::istream &input) {
	static char buffer[2048];
	auto *buf = input.rdbuf();
	std::string ret;
	for (size_t read;
		(read = buf->sgetn(buffer, sizeof(buffer)));
		ret += std::string(buffer, buffer + read));
	return ret;
}

namespace coco {

clang::Rewriter *writer;
std::string code, file_name;

class DCEHandler : public MatchFinder::MatchCallback {
public:
	virtual void run(const MatchFinder::MatchResult &result) {
		if (const auto *func = result.Nodes.getNodeAs<FunctionDecl>("funcDecl")) {
			if (!func->hasBody()
				|| func->isMain()) return;
			if (auto *tmp = func->getInstantiatedFromMemberFunction())
				func = tmp;
			if (func->isTemplateInstantiation())
				func = func->getTemplateInstantiationPattern();
			if (result.SourceManager->isInSystemHeader(func->getBeginLoc())) return;
			if (const auto *method = dyn_cast<CXXMethodDecl>(func)) {
				if (method->isVirtual() && !method->isPure() && method->size_overridden_methods())
					return;
				if (isa<CXXDestructorDecl>(method))
					return;
			}
			defined.push_back(func);
		} else if (const auto *decl = result.Nodes.getNodeAs<VarDecl>("varDecl")) {
			if (result.SourceManager->isInSystemHeader(decl->getBeginLoc())) return;
			if (isa<ParmVarDecl>(decl)) return;
			defined.push_back(decl);
		} else if (const auto *expr = result.Nodes.getNodeAs<DeclRefExpr>("declRefExpr"))
			handleUse(result.SourceManager, expr->getDecl());
		else if (const auto *expr = result.Nodes.getNodeAs<MemberExpr>("memberExpr"))
			handleUse(result.SourceManager, expr->getMemberDecl());
		else if (const auto *expr = result.Nodes.getNodeAs<CXXConstructExpr>("cxxConstructExpr"))
			handleUse(result.SourceManager, expr->getConstructor());
	}
	void finalize() {
		std::vector<const ValueDecl*> unused_decls;
		for (const auto *decl : defined)
			if (used.find((ValueDecl*) decl->getCanonicalDecl()) == used.end())
				unused_decls.push_back(decl);
		std::sort(unused_decls.begin(), unused_decls.end(),
			[](const ValueDecl *a, const ValueDecl *b) {
				return a->getBeginLoc() > b->getBeginLoc();
			});
		Rewriter::RewriteOptions options;
		options.RemoveLineIfEmpty = true;
		for (const auto *decl : unused_decls) {
			if (dyn_cast<FunctionDecl>(decl)) {
				auto print = [](const Decl *decl) {
					const unsigned begin = writer->getSourceMgr().getFileOffset(decl->getBeginLoc()),
									end = writer->getSourceMgr().getFileOffset(decl->getEndLoc());
					llvm::errs() << "Removing " << code.substr(begin, end - begin + 1) << '\n';
				};
				if (const auto *temp = decl->getDescribedTemplate())
					writer->RemoveText(temp->getSourceRange(), options), print(temp);
				else writer->RemoveText(decl->getSourceRange(), options), print(decl);
			}
		}
	}
private:
	inline void handleUse(const SourceManager *sm, const ValueDecl *decl) {
		if (sm->isInSystemHeader(decl->getBeginLoc())) return;
		if (const auto *func = dyn_cast<FunctionDecl>(decl)) {
			if (func->isTemplateInstantiation())
				func = func->getTemplateInstantiationPattern();
			used.insert(func->getCanonicalDecl());
		} else if (const auto *var = dyn_cast<VarDecl>(decl))
			used.insert(var->getCanonicalDecl());
	}

	std::vector<const ValueDecl*> defined;
	std::set<const ValueDecl*> used;
};

class DCEConsumer : public ASTConsumer {
public:
	DCEConsumer() {
		matcher.addMatcher(functionDecl(isDefinition(), unless(isImplicit())).bind("funcDecl"), &handler);
		matcher.addMatcher(varDecl(isDefinition(), unless(isImplicit())).bind("varDecl"), &handler);

		matcher.addMatcher(declRefExpr().bind("declRefExpr"), &handler);
		matcher.addMatcher(memberExpr(member(hasType(functionProtoType()))).bind("memberExpr"), &handler);
		matcher.addMatcher(cxxConstructExpr().bind("cxxConstructExpr"), &handler);
	}
	virtual void HandleTranslationUnit(ASTContext &context) {
		matcher.matchAST(context);
		handler.finalize();
	}
private:
	DCEHandler handler;
	MatchFinder matcher;
};

class DCEAction : public ASTFrontendAction {
public:
	virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(
		CompilerInstance &compiler, llvm::StringRef file) {
		writer->setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
		return std::make_unique<DCEConsumer>();
	}
	virtual void EndSourceFileAction() {
		auto &buffer = writer->getEditBuffer(writer->getSourceMgr().getMainFileID());
		code = std::string(buffer.begin(), buffer.end());
	}
};

class MivikStyle : public format::FormatStyle {
public:
	MivikStyle() {
		AccessModifierOffset = 0;
		AlignAfterOpenBracket = BAS_AlwaysBreak;
		AlignConsecutiveMacros = false;
		AlignConsecutiveAssignments = false;
		AlignConsecutiveDeclarations = false;
		AlignEscapedNewlines = ENAS_DontAlign;
		AlignOperands = false;
		AlignTrailingComments = false;
		AllowAllArgumentsOnNextLine = true;
		AllowAllConstructorInitializersOnNextLine = false;
		AllowAllParametersOfDeclarationOnNextLine = false;
		AllowShortBlocksOnASingleLine = SBS_Always;
		AllowShortCaseLabelsOnASingleLine = true;
		AllowShortFunctionsOnASingleLine = SFS_All;
		AllowShortIfStatementsOnASingleLine = SIS_WithoutElse;
		AllowShortLambdasOnASingleLine = SLS_All;
		AllowShortLoopsOnASingleLine = true;
		AlwaysBreakAfterReturnType = RTBS_None;
		AlwaysBreakBeforeMultilineStrings = true;
		AlwaysBreakTemplateDeclarations = BTDS_MultiLine;
		BinPackArguments = true;
		BinPackParameters = true;
		BreakBeforeBinaryOperators = BOS_NonAssignment;
		BreakBeforeBraces = BS_Attach;
		BraceWrapping.AfterCaseLabel = false; // ?
		BraceWrapping.AfterClass = true;
		BraceWrapping.AfterControlStatement = BWACS_Never;
		BraceWrapping.AfterEnum = false;
		BraceWrapping.AfterFunction = false;
		BraceWrapping.AfterNamespace = false;
		BraceWrapping.AfterObjCDeclaration = false;
		BraceWrapping.AfterStruct = false;
		BraceWrapping.AfterUnion = false;
		BraceWrapping.AfterExternBlock = false;
		BraceWrapping.BeforeCatch = false;
		BraceWrapping.BeforeElse = false;
		BraceWrapping.IndentBraces = false; // ?
		BraceWrapping.SplitEmptyFunction = false;
		BraceWrapping.SplitEmptyRecord = false;
		BraceWrapping.SplitEmptyNamespace = false;
		BreakBeforeTernaryOperators = true;
		BreakConstructorInitializers = BCIS_AfterColon;
		BreakStringLiterals = false;
		ColumnLimit = 200; // needs tweaks
		CommentPragmas = true;
		BreakInheritanceList = BILS_AfterColon;
		CompactNamespaces = false;
		ConstructorInitializerAllOnOneLineOrOnePerLine = true;
		ContinuationIndentWidth = 2; // how about tabs?!
		Cpp11BracedListStyle = false;
		DeriveLineEnding = true; // ?
		DerivePointerAlignment = false;
		DisableFormat = false;
		FixNamespaceComments = true;
		ForEachMacros = { "go" };
		TypenameMacros = {};
		StatementMacros = {};
		NamespaceMacros = {};
		IncludeStyle.IncludeBlocks = IncludeStyle::IncludeBlocksStyle::IBS_Regroup;
		IndentCaseLabels = true;
		IndentGotoLabels = true;
		IndentPPDirectives = PPDIS_AfterHash;
		IndentWidth = 4; // how about my tabs!!!
		IndentWrappedFunctionNames = true;
		KeepEmptyLinesAtTheStartOfBlocks = false;
		Language = LK_Cpp;
		MaxEmptyLinesToKeep = 1;
		NamespaceIndentation = NI_None;
		PenaltyBreakAssignment = 1; // ?
		PenaltyBreakBeforeFirstCallParameter = 50;
		PenaltyBreakComment = 5;
		PenaltyBreakFirstLessLess = 1;
		PenaltyBreakString = 50;
		PenaltyBreakTemplateDeclaration = 20;
		PenaltyExcessCharacter = 1;
		PenaltyReturnTypeOnItsOwnLine = 0;
		PointerAlignment = PAS_Right;
		ReflowComments = true;
		SortIncludes = true;
		SortUsingDeclarations = true;
		SpaceAfterCStyleCast = false;
		SpaceAfterLogicalNot = false;
		SpaceAfterTemplateKeyword = false;
		SpaceBeforeAssignmentOperators = true;
		SpaceBeforeCpp11BracedList = false;
		SpaceBeforeCtorInitializerColon = false;
		SpaceBeforeInheritanceColon = true;
		SpaceBeforeParens = SBPO_ControlStatements;
		SpaceBeforeRangeBasedForLoopColon = true;
		SpaceInEmptyBlock = false;
		SpaceInEmptyParentheses = false;
		SpacesBeforeTrailingComments = 1; // needs tweaks
		SpacesInAngles = false;
		SpacesInConditionalStatement = false;
		SpacesInContainerLiterals = false;
		SpacesInCStyleCastParentheses = false;
		SpacesInParentheses = false;
		SpacesInSquareBrackets = false;
		SpaceBeforeSquareBrackets = false;
		Standard = LS_Auto; // is it ok?
		TabWidth = 4; // ummm...
		UseCRLF = false;
		UseTab = UT_Always;
	}
};

class DummyConsumer : public ASTConsumer {};

inline void tokenize(std::function<void(Token)> handler, bool keep_comment = true) {
	LangOptions lang;
	lang.LineComment = 1;
	lang.CPlusPlus = 1;
	lang.CPlusPlus11 = 1;
	lang.CPlusPlus14 = 1;
	lang.CPlusPlus17 = 1;
	lang.Bool = 1;
	const SourceManager &sm = writer->getSourceMgr();
	const FileID main = sm.getMainFileID();
	Lexer lexer(sm.getLocForStartOfFile(main), lang, code.data(), code.data(), code.data() + code.size());
	lexer.SetCommentRetentionState(keep_comment);
	Token token;
	while (!lexer.LexFromRawLexer(token) && lexer.getCurrentBufferOffset() < code.size())
		handler(token);
	if (sm.getFileOffset(token.getLocation()) < code.size()) handler(token);
}

class FormatCodeAction : public ASTFrontendAction {
public:
	virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(
		CompilerInstance &compiler, llvm::StringRef file) {
		writer->setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
		return std::make_unique<DummyConsumer>();
	}
	virtual void EndSourceFileAction() {
		const SourceManager &sm = writer->getSourceMgr();
		const std::string_view sv = code;
		unsigned last = 0;
		int preserve_depth = 0;
		std::vector<unsigned> points;
		tokenize([&](Token token) {
			if (token.getKind() != tok::TokenKind::comment) return;
			const unsigned begin = sm.getFileOffset(token.getLocation());
			unsigned end = begin + token.getLength();
			if (end != code.size()) ++end; // extend the comment range to the next line
			std::string_view comment = sv.substr(begin, token.getLength());
			const std::string &pattern = "// coco: ";
			if (pattern.size() > comment.size() || comment.substr(0, pattern.size()) != pattern)
				return;
			std::string_view flag = comment.substr(pattern.size());
			if (flag == "preserve_begin") {
				++preserve_depth;
				last = end;
			} else if (flag == "preserve_end") {
				if (!preserve_depth) {
					llvm::errs() << "Unexpected flag: preserve_end\n";
					exit(1);
				}
				if (!--preserve_depth) {
					points.push_back(last);
					points.push_back(begin - 1);
				}
				last = end;
			} else {
				llvm::errs() << "Unknown coco flag: " << (std::string)flag << '\n';
				exit(1);
			}
		});
		if (preserve_depth) {
			llvm::errs() << "Unterminated preserve part\n";
			exit(1);
		}

		auto replacements =
			format::reformat(
				MivikStyle(),
				code,
				tooling::Range(0, code.size()),
				sm.getFileEntryForID(sm.getMainFileID())->getName()
			);
		for (auto iter = replacements.rbegin(); iter != replacements.rend(); ++iter) {
			if (!iter->isApplicable()) continue;
			const unsigned begin = iter->getOffset(), end = begin + iter->getLength();
			const size_t p1 = std::lower_bound(points.begin(), points.end(), begin) - points.begin();
			const size_t p2 = std::lower_bound(points.begin(), points.end(), end) - points.begin();
			if (p1 != p2 || p1 & 1) continue;
			iter->apply(*writer);
		}
		auto &buffer = writer->getEditBuffer(writer->getSourceMgr().getMainFileID());
		code = std::string(buffer.begin(), buffer.end());
	}
};

class RemoveRinFlagAction : public ASTFrontendAction {
public:
	virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(
		CompilerInstance &compiler, llvm::StringRef file) {
		writer->setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
		return std::make_unique<DummyConsumer>();
	}
	virtual void EndSourceFileAction() {
		const SourceManager &sm = writer->getSourceMgr();
		const std::string_view sv = code;
		std::string ret;
		unsigned last = 0;
		tokenize([&](Token token) {
			if (token.getKind() != tok::TokenKind::comment) return;
			const unsigned begin = sm.getFileOffset(token.getLocation());
			unsigned end = begin + token.getLength();
			if (end != code.size()) ++end;
			std::string_view comment = sv.substr(begin, token.getLength());
			const std::string &pattern = "// coco: ";
			if (pattern.size() > comment.size() || comment.substr(0, pattern.size()) != pattern)
				return;
			ret += sv.substr(last, begin - last);
			last = end;
		});
		ret += sv.substr(last);

		code = ret;
	}
};

class ExpandHeaderAction : public ASTFrontendAction {
public:
	virtual std::unique_ptr<ASTConsumer> CreateASTConsumer(
		CompilerInstance &compiler, llvm::StringRef file) {
		writer->setSourceMgr(compiler.getSourceManager(), compiler.getLangOpts());
		return std::make_unique<DummyConsumer>();
	}
	virtual void EndSourceFileAction() {
		const SourceManager &sm = writer->getSourceMgr();
		const std::string_view sv = code;
		/*
			0 -> waiting for # (hash) at the beginning of the line
			1 -> waiting for include
			2 -> waiting for < (less)
			3 -> waiting for > (greater)
		*/
		unsigned state = 0;
		unsigned last_line;
		SourceLocation begin_loc;
		std::string include_path;
		std::vector<std::pair<std::pair<SourceLocation, unsigned>, std::string> >
			includes;
		tokenize([&](Token token) {
			const unsigned begin = sm.getFileOffset(token.getLocation());
			std::string_view str =
				sv.substr(begin, token.getLength());
			auto get_line = [&]() { return writer->getSourceMgr().getSpellingLineNumber(token.getLocation()); };
			switch (state) {
				case 0: {
					if (token.isNot(tok::hash) ||
						!token.isAtStartOfLine()
					) break;
					state = 1;
					last_line = get_line();
					begin_loc = token.getLocation();
					break;
				}
				case 1: {
					if (token.isNot(tok::raw_identifier)
						|| str != "include"
						|| get_line() != last_line
					) state = 0;
					else state = 2;
					break;
				}
				case 2: {
					if (token.isNot(tok::less) || get_line() != last_line)
						state = 0;
					else{
						state = 3;
						include_path.clear();
					}
					break;
				}
				case 3: {
					if (get_line() != last_line) state = 0;
					else if (token.is(tok::greater)) {
						state = 0;
						if (starts_with(include_path, "mivik"))
							includes.push_back(
								std::make_pair(
									std::make_pair(begin_loc, begin + token.getLength() - sm.getFileOffset(begin_loc)),
									include_path
								)
							);
					} else include_path += str;
					break;
				}
				default: assert(false);
			}
		});
		for (auto iter = includes.rbegin(); iter != includes.rend(); ++iter) {
			std::ifstream input("/usr/include/" + iter->second);
			if (!input) {
				llvm::errs() << "Include file \"" << iter->second << "\" not found under /usr/include\n";
				exit(1);
			}
			std::string content = read_string(input);
			input.close();
			writer->ReplaceText(iter->first.first, iter->first.second, content);
		}
		auto &buffer = writer->getEditBuffer(writer->getSourceMgr().getMainFileID());
		code = std::string(buffer.begin(), buffer.end());
	}
};

} // namespace coco

inline bool run_action(std::unique_ptr<FrontendAction> action, const std::vector<std::string> &args) {
	coco::writer = new Rewriter();
	const bool ret = tooling::runToolOnCodeWithArgs(
		std::move(action),
		coco::code,
		args,
		coco::file_name);
	delete coco::writer;
	return ret;
}

struct rin_args {
	bool no_expand: 1;
	bool no_dce: 1;
	bool no_remove: 1;
	std::vector<std::string> extra_args;
	const char *source_file;

	rin_args():
		no_expand(0),
		no_dce(0),
		no_remove(0),
		source_file(nullptr) {}
};

inline rin_args parse_args(int argc, const char **argv) {
	rin_args args;
	for (int i = 1; i < argc; ++i) {
		if (argv[i][0] != '-' || !strcmp(argv[i], "-")) {
			if (args.source_file) {
				llvm::errs() << "Multiple input files is not supported yet\n";
				exit(1);
			}
			args.source_file = argv[i];
			continue;
		}
		if (!strcmp(argv[i], "-no-expand")) args.no_expand = true;
		else if (!strcmp(argv[i], "-no-dce")) args.no_dce = true;
		else if (!strcmp(argv[i], "-no-remove")) args.no_remove = true;
		else args.extra_args.push_back(argv[i]);
	}
	return args;
}

int main(int argc, const char **argv) {
	if (argc < 2) {
		llvm::errs() << "Usage: " << argv[0] << " [-no-expand] [-no-dce] <cpp_source_file>" << endl;
		return 1;
	}
	const rin_args args = parse_args(argc, argv);
	if (!args.source_file) {
		llvm::errs() << "Missing source file\n";
		return 1;
	}
	const bool use_stdin = !strcmp(args.source_file, "-");
	std::istream *input =
		use_stdin? &std::cin: new std::ifstream(args.source_file, std::ios::in);
	if (!input) {
		llvm::errs() << "Failed to open file: " << strerror(errno) << endl;
		return 1;
	}
	coco::code = read_string(*input);
	if (!use_stdin) {
		std::ifstream *file_input = (std::ifstream*)input;
		file_input->close();
		delete file_input;
	}

	coco::file_name = use_stdin? "input.cpp": fs::path(args.source_file).filename();

	if (!args.no_expand && !run_action(std::make_unique<coco::ExpandHeaderAction>(), args.extra_args)) return 1;
	if (!args.no_dce && !run_action(std::make_unique<coco::DCEAction>(), args.extra_args)) return 1;
	if (!run_action(std::make_unique<coco::FormatCodeAction>(), args.extra_args)) return 1;
	if (!args.no_remove && !run_action(std::make_unique<coco::RemoveRinFlagAction>(), args.extra_args)) return 1;

	llvm::outs() << coco::code;
	return 0;
}