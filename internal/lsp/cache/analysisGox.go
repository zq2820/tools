package cache

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"
	"golang.org/x/tools/go/ast/astutil"
)

type ChildType int

const (
	NotElement ChildType = iota
	NotElementSlice
	HostElement
	HostElementSlice
	CustomElement
	CustomElementSlice
)

var caseTitle = cases.Title(language.English, cases.NoLower)

type analysisJsx struct {
	pkg     *pkg
	fileTok *token.File
	file    *ast.File
}

func newAnalysisJsx(target *pkg) *analysisJsx {
	return &analysisJsx{
		pkg: target,
	}
}

func (analysis *analysisJsx) run() {
	for _, goFile := range analysis.pkg.compiledGoFiles {
		if goFile.File.IsJsx {
			astutil.Apply(goFile.File, nil, func(cursor *astutil.Cursor) bool {
				analysis.fileTok = goFile.Tok
				analysis.file = goFile.File
				return analysis.analysisExpr(cursor)
			})
		}
	}
}

func (analysis *analysisJsx) analysisExpr(cursor *astutil.Cursor) bool {
	switch node := cursor.Node().(type) {
	case *ast.GoExpr:
		if _, ok := cursor.Parent().(*ast.GoxExpr); ok {
			if callExpr, ok := node.X.(*ast.CallExpr); ok {
				var kindObj types.Object
				if selectorExpr, ok := callExpr.Fun.(*ast.SelectorExpr); ok {
					kindObj = analysis.pkg.typesInfo.Uses[selectorExpr.Sel]
				} else if ident, ok := callExpr.Fun.(*ast.Ident); ok {
					kindObj = analysis.pkg.typesInfo.Uses[ident]
				}
				if kindObj != nil {
					kind := kindObj.Type()
					if funcType, ok := kind.(*types.Signature); ok {
						tuple := funcType.Results()
						if tuple != nil {
							if tuple.Len() > 1 {
								analysis.pkg.typeErrors = append(analysis.pkg.typeErrors, types.NewError(
									analysis.fileTok.Set(),
									node.Rbrace-token.Pos(analysis.fileTok.Base()),
									"GoExpr is only support return one element",
									false,
									0,
									node.Lbrace-token.Pos(analysis.fileTok.Base()),
									node.Rbrace-token.Pos(analysis.fileTok.Base()),
								))
								return false
							}
							childType := getJsxType(tuple.At(0).Type().Underlying())
							replaceGox(cursor, callExpr, childType)
						} else {
							return false
						}
					}
				}
			} else {
				var kindObj types.Object
				var childType ChildType
				if selectorExpr, ok := node.X.(*ast.SelectorExpr); ok {
					kindObj = analysis.pkg.typesInfo.Uses[selectorExpr.Sel]
				} else if ident, ok := node.X.(*ast.Ident); ok {
					kindObj = analysis.pkg.typesInfo.Uses[ident]
				} else if _, ok := node.X.(*ast.BinaryExpr); ok {
					childType = NotElement
				} else if compositeLit, ok := node.X.(*ast.CompositeLit); ok {
					switch compositeLitType := compositeLit.Type.(type) {
					case *ast.Ident:
						kindObj = analysis.pkg.typesInfo.Uses[compositeLitType]
					case *ast.SelectorExpr:
						kindObj = analysis.pkg.typesInfo.Uses[compositeLitType.Sel]
					case *ast.ArrayType:
						if _, ok := compositeLitType.Elt.(*ast.Ident); ok {
							childType = NotElementSlice
						}
					}
				}
				if kindObj != nil {
					childType = getJsxType(kindObj.Type())
				}
				replaceGox(cursor, node.X, childType)
			}
		} else {
			cursor.Replace(node.X)
		}
	case *ast.GoxExpr:
		var id *ast.Ident 
		if selectorExpr, ok := node.TagName.(*ast.SelectorExpr); ok {
			id = selectorExpr.Sel
		} else if ident, ok := node.TagName.(*ast.Ident); ok {
			id = ident
		}

		tagType := analysis.pkg.typesInfo.Uses[id]
		_ = tagType
		// propsType := analysis.getProps(tagType, node.TagName.End() + 2)
		propsType := &ast.SelectorExpr{
			X: &ast.Ident{Name: "react", NamePos: node.TagName.End() + 2},
			Sel: &ast.Ident{Name: "DivProps", NamePos: node.TagName.End() + 2},
		}

		propsDef := &ast.CompositeLit{
			Type: propsType,
			Lbrace: node.TagName.End(),
			Elts: []ast.Expr{},
		}
		props := &ast.UnaryExpr{
			Op: token.AND,
			X: propsDef,
			OpPos: node.TagName.End() + 1,
		}

		for _, attr := range node.Attrs {
			propsDef.Elts = append(propsDef.Elts, &ast.KeyValueExpr{
				Key: attr.Lhs,
				Value: attr.Rhs,
				Colon: attr.Lhs.End(),
			})
		}
		if len(node.Attrs) > 0 {
			propsDef.Rbrace = node.Attrs[len(node.Attrs) - 1].End()
			if node.Attrs[len(node.Attrs) - 1].IsEllipsis {
				propsDef.Rbrace -= 4
			}
		} else {
			propsDef.Rbrace = propsDef.Lbrace + 2
		}

		children := &ast.CallExpr{
			Fun: &ast.Ident{
				Name: "make",
				NamePos: propsDef.Rbrace,
				Hidden: true,
			},
			Lparen: propsDef.Rbrace,
			Args: []ast.Expr{
				&ast.ArrayType{
					Elt: &ast.SelectorExpr{
						X: &ast.Ident{
							Name: "react",
							NamePos: propsDef.Rbrace,
							Hidden: true,
						},
						Sel: &ast.Ident{
							Name: "Element",
							NamePos: propsDef.Rbrace,
							Hidden: true,
						},
					},
				},
				&ast.BasicLit{
					Kind: token.INT,
					Value: "0",
					ValuePos: propsDef.Rbrace,
				},
				&ast.BasicLit{
					Kind: token.INT,
					Value: "16",
					ValuePos: propsDef.Rbrace,
				},
			},
			Rparen: propsDef.Rbrace,
		}

		for i, child := range node.X {
			childDef := child
			pos := token.NoPos
			if goExpr, ok := child.(*ast.GoExpr); ok {
				if goExpr.IsSlice {
					pos = goExpr.End() + 1
				}
				childDef = goExpr.X
			}

			children = &ast.CallExpr{
				Fun: &ast.Ident{
					Name: "append",
					NamePos: propsDef.Rbrace - token.Pos(i),
					Hidden: true,
				},
				Lparen: propsDef.Rbrace - token.Pos(i),
				Args: []ast.Expr{
					children,
					childDef,
				},
				Ellipsis: pos,
				Rparen: child.End(),
			}
		}
		cursor.Replace(&ast.CallExpr{
			Fun: node.TagName,
			Lparen: node.TagName.End(),
			Args: append([]ast.Expr{
				props,
			}, children),
			Ellipsis: node.End() + 1,
			Rparen: node.End(),
			Element: node,
		})
	case *ast.BareWordsExpr:
		if node.Value != "" {
			replaceGox(cursor, &ast.BasicLit{
				ValuePos: node.ValuePos,
				Kind:     token.STRING,
				Value:    fmt.Sprintf("\"%s\"", node.Value),
			}, NotElement)
		} else {
			cursor.Delete()
		}
	}

	return true
}

func getJsxType(obj types.Type) ChildType {
	kind := obj.Underlying()

	switch result := kind.(type) {
	case *types.Interface:
		if result.Obj().Pkg().Name() == "core" && result.Obj().Name() == "Element" {
			return HostElement
		}
	case *types.Slice:
		elem := getJsxType(result.Elem())
		if elem == HostElement {
			return HostElementSlice
		} else {
			return NotElementSlice
		}
	}
	return NotElement
}

func (analysis *analysisJsx)getProps(obj types.Object, pos token.Pos) ast.Expr {
	if funcType, ok := obj.(*types.Func); ok {
		tuples := funcType.Type().(*types.Signature).Params()
		if tuples.Len() > 0 {
			propsType, ok := tuples.At(0).Type().(*types.Pointer)
			if ok {
				propsTypeDef := propsType.Elem().(types.Object)
				if propsTypeDef.Pkg().Path() == string(analysis.pkg.m.PkgPath) {
					return &ast.SelectorExpr{
						X: &ast.Ident{
							Name: propsTypeDef.Pkg().Path(),
							NamePos: pos,
						},
						Sel: &ast.Ident{
							Name: propsTypeDef.Name(),
							NamePos: pos,
						},
					}
				}
			}
		}
		return nil
	} else {
		return nil
	}
}

func replaceGox(cursor *astutil.Cursor, expr ast.Expr, childType ChildType) {
	if childType == HostElement {
		switch node := expr.(type) {
		case *ast.Ident:
			node.Element = cursor.Node()
		case *ast.SelectorExpr:
			node.Element = cursor.Node()
		case *ast.IndexExpr:
			node.Element = cursor.Node()
		case *ast.CallExpr:
			node.Element = cursor.Node()
		case *ast.IndexListExpr:
			node.Element = cursor.Node()
		case *ast.CompositeLit:
			node.Element = cursor.Node()
		}
		cursor.Replace(expr)
	} else if childType == HostElementSlice {		
		if goxExpr, ok := cursor.Node().(*ast.GoExpr); ok {
			goxExpr.IsSlice = true
		}
	} else if childType == NotElement {
		cursor.Replace(&ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   &ast.Ident{
					Name: "react",
					NamePos: cursor.Node().Pos(),
					Hidden: true,
				},
				Sel: &ast.Ident{
					Name: "S",
					NamePos: cursor.Node().Pos(),
					Hidden: true,
				},
			},
			Lparen: cursor.Node().Pos(),
			Args: []ast.Expr{
				expr,
			},
			Rparen: cursor.Node().End(),
			Element: cursor.Node(),
		})
	} else {
		cursor.Replace(&ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   &ast.Ident{
					Name: "react",
					NamePos: cursor.Node().Pos(),
					Hidden: true,
				},
				Sel: &ast.Ident{
					Name: "Sprintln",
					NamePos: cursor.Node().Pos(),
					Hidden: true,
				},
			},
			Lparen: cursor.Node().Pos(),
			Args: []ast.Expr{
				expr,
			},
			Rparen: cursor.Node().End(),
			Element: cursor.Node(),
		})
	}
}