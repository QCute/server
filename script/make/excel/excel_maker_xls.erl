%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to excel and excel to database data
%%% @end
%%%-------------------------------------------------------------------
-module(excel_maker_xls).
-compile(nowarn_export_all).
-compile(export_all).
-export([write/2]).
-export([read/1]).
-include_lib("xmerl/include/xmerl.hrl").
%%%===================================================================
%%% Table to XLS
%%%===================================================================
%% @doc table to xls
-spec write(Name :: file:name(), Xls :: #{}) -> binary().
write(Name, Xls) ->
    Files = #{
        "_rels" => #{
            ".rels" => <<>>
        },
        "docProps" => #{
            "app.xml" => <<>>,
            "core.xml" => <<>>,
            "custom.xml" => <<>>
        },
        "xl" => #{
            "_rels" => #{
                "workbook.xml.rels" => <<>>
            },
            "JDEData.bin" => <<>>,
            "styles.xml" => <<>>,
            "sharedStrings.xml" => <<>>,
            "theme" => #{
                "theme1.xml" => <<>>
            },
            "workbook.xml" => <<>>,
            "worksheets" => #{
                "sheet1.xml" => <<>>
            }
        },
        "[Content_Types].xml" => <<>>
    },
    FileList = export_loop(maps:to_list(Files), Xls, "", []),
    {ok, {_, Binary}} = zip:create(Name, FileList, [memory]),
    Binary.


export_loop([], _, _, Files) ->
    Files;
export_loop([{Name, Dir = #{}} | T], Xls, Path, Files) ->
    PathName = lists:concat([Path, Name]),
    PathNameFunction = list_to_atom(PathName),
    case erlang:function_exported(?MODULE, PathNameFunction, 1) of
        true ->
            SubList = apply(?MODULE, PathNameFunction, [Xls]),
            RootAttributes = [
                #xmlAttribute{name = encoding, value = "UTF-8"},
                #xmlAttribute{name = standalone, value = "yes"}
            ],
            SubFileList = [{SubName, unicode:characters_to_binary(xmerl:export_simple([SubData], xmerl_xml, RootAttributes))} || #{name := SubName, data := SubData} <- SubList],
            export_loop(T, Xls, Path, lists:append(SubFileList, Files));
        false ->
            DeepPath = lists:concat([Path, Name, "/"]),
            NewFiles = export_loop(maps:to_list(Dir), Xls, DeepPath, Files),
            export_loop(T, Xls, Path, NewFiles)
    end;
export_loop([{Name, <<>>} | T], Xls, Path, Files) ->
    PathName = lists:concat([Path, Name]),
    PathNameFunction = list_to_atom(PathName),
    case erlang:function_exported(?MODULE, PathNameFunction, 1) of
        true ->
            Data = apply(?MODULE, PathNameFunction, [Xls]),
            RootAttributes = [
                #xmlAttribute{name = encoding, value = "UTF-8"},
                #xmlAttribute{name = standalone, value = "yes"}
            ],
            Binary = unicode:characters_to_binary(xmerl:export_simple([Data], xmerl_xml, RootAttributes)),
            export_loop(T, Xls, Path, [{PathName, Binary} | Files]);
        false ->
            export_loop(T, Xls, Path, Files)
    end.

%%%===================================================================
%%% XLS
%%%===================================================================

'_rels/.rels'(_) ->
    #xmlElement{
        name = 'Relationships',
        attributes = [
            #xmlAttribute{name = 'xmlns', value = "http://schemas.openxmlformats.org/package/2006/relationships"}
        ],
        content = [
            #xmlElement{
                name = 'Relationship',
                attributes = [
                    #xmlAttribute{name = 'Id', value = "rId1"},
                    #xmlAttribute{name = 'Type', value = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"},
                    #xmlAttribute{name = 'Target', value = "xl/workbook.xml"}
                ]
            }
        ]
    }.


'[Content_Types].xml'(_) ->
    #xmlElement{
        name = 'Types',
        attributes = [
            #xmlAttribute{
                name = 'xmlns',
                value = "http://schemas.openxmlformats.org/package/2006/content-types"
            }
        ],
        content = [
            #xmlElement{
                name = 'Default',
                attributes = [
                    #xmlAttribute{
                        name = 'Extension',
                        value = "rels"
                    },
                    #xmlAttribute{
                        name = 'ContentType',
                        value = "application/vnd.openxmlformats-package.relationships+xml"
                    }
                ]
            },
            #xmlElement{
                name = 'Default',
                attributes = [
                    #xmlAttribute{
                        name = 'Extension',
                        value = "xml"
                    },
                    #xmlAttribute{
                        name = 'ContentType',
                        value = "application/xml"
                    }
                ]
            }
        ]
    }.


'xl/_rels/workbook.xml.rels'(Sheet) ->

    SheetFiles = make_sheet_relation_ship(maps:keys(Sheet), 1, []),

    JsMacro = #xmlElement{
        name = 'Relationship',
        attributes = [
            #xmlAttribute{name = 'Id', value = lists:concat(["rId", length(SheetFiles) + 2])},
            #xmlAttribute{name = 'Type', value = "http://www.wps.cn/officeDocument/2018/jdeExtension"},
            #xmlAttribute{name = 'Target', value = "JDEData.bin"}
        ]
    },

    Styles = #xmlElement{
        name = 'Relationship',
        attributes = [
            #xmlAttribute{name = 'Id', value = lists:concat(["rId", length(SheetFiles) + 1])},
            #xmlAttribute{name = 'Type', value = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"},
            #xmlAttribute{name = 'Target', value = "styles.xml"}
        ]
    },

    #xmlElement{
        name = 'Relationships',
        attributes = [
            #xmlAttribute{name = 'xmlns', value = "http://schemas.openxmlformats.org/package/2006/relationships"}
        ],
        content = [JsMacro, Styles | SheetFiles]
    }.


make_sheet_relation_ship([], _, List) ->
    List;
make_sheet_relation_ship([_ | SheetName], Index, List) ->
    Element = #xmlElement{
        name = 'Relationship',
        attributes = [
            #xmlAttribute{name = 'Id', value = lists:concat(["rId", Index])},
            #xmlAttribute{name = 'Type', value = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"},
            #xmlAttribute{name = 'Target', value = lists:concat(["worksheets/sheet", Index, ".xml"])}
        ]
    },
    make_sheet_relation_ship(SheetName, Index + 1, [Element | List]).


'xl/styles.xml'(_) ->
    #xmlElement{
        name = styleSheet,
        attributes = [
            #xmlAttribute{
                name = xmlns,
                value = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
            },
            #xmlAttribute{
                name = 'xmlns:mc',
                value = "http://schemas.openxmlformats.org/markup-compatibility/2006"
            },
            #xmlAttribute{
                name = 'mc:Ignorable',
                value = "xr9"
            },
            #xmlAttribute{
                name = 'xmlns:xr9',
                value = "http://schemas.microsoft.com/office/spreadsheetml/2016/revision9"
            }
        ],
        content = [
            #xmlElement{
                name = fills,
                attributes = [
                    #xmlAttribute{
                        name = count,
                        value = "3"
                    }
                ],
                content = [
                    #xmlElement{
                        name = fill,
                        content = [
                            #xmlElement{
                                name = patternFill,
                                attributes = [
                                    #xmlAttribute{
                                        name = patternType,
                                        value = "none"
                                    }
                                ],
                                content = []
                            }
                        ]
                    },
                    #xmlElement{
                        name = fill,
                        content = [
                            #xmlElement{
                                name = patternFill,
                                attributes = [
                                    #xmlAttribute{
                                        name = patternType,
                                        value = "gray125"
                                    }
                                ],
                                content = []
                            }
                        ]
                    },
                    #xmlElement{
                        name = fill,
                        content = [
                            #xmlElement{
                                name = patternFill,
                                attributes = [
                                    #xmlAttribute{
                                        name = patternType,
                                        value = "solid"
                                    }
                                ],
                                content = [
                                    #xmlElement{
                                        name = fgColor,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FFF8F8F8"
                                            }
                                        ]
                                    },
                                    #xmlElement{
                                        name = bgColor,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FF333333"
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
            #xmlElement{
                name = borders,
                attributes = [
                    #xmlAttribute{
                        name = count,
                        value = "2"
                    }
                ],
                content = [
                    #xmlElement{
                        name = border
                    },
                    #xmlElement{
                        name = border,
                        content = [
                            #xmlElement{
                                name = left,
                                attributes = [
                                    #xmlAttribute{
                                        name = style,
                                        value = "thin"
                                    }
                                ],
                                content = [
                                    #xmlElement{
                                        name = color,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FFE7E6E6"
                                            }
                                        ]
                                    }
                                ]
                            },
                            #xmlElement{
                                name = right,
                                attributes = [
                                    #xmlAttribute{
                                        name = style,
                                        value = "thin"
                                    }
                                ],
                                content = [
                                    #xmlElement{
                                        name = color,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FFE7E6E6"
                                            }
                                        ]
                                    }
                                ]
                            },
                            #xmlElement{
                                name = top,
                                attributes = [
                                    #xmlAttribute{
                                        name = style,
                                        value = "thin"
                                    }
                                ],
                                content = [
                                    #xmlElement{
                                        name = color,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FFE7E6E6"
                                            }
                                        ]
                                    }
                                ]
                            },
                            #xmlElement{
                                name = bottom,
                                attributes = [
                                    #xmlAttribute{
                                        name = style,
                                        value = "thin"
                                    }
                                ],
                                content = [
                                    #xmlElement{
                                        name = color,
                                        attributes = [
                                            #xmlAttribute{
                                                name = rgb,
                                                value = "FFE7E6E6"
                                            }
                                        ]
                                    }
                                ]
                            },
                            #xmlElement{
                                name = diagonal
                            }
                        ]
                    }
                ]
            },
            #xmlElement{
                name = cellXfs,
                attributes = [
                    #xmlAttribute{
                        name = count,
                        value = "4"
                    }
                ],
                content = [
                    #xmlElement{
                        name = xf
                    },
                    #xmlElement{
                        name = xf,
                        attributes = [
                            #xmlAttribute{
                                name = applyProtection,
                                value = "1"
                            }
                        ]
                    },
                    #xmlElement{
                        name = xf,
                        attributes = [
                            #xmlAttribute{
                                name = fillId,
                                value = "2"
                            },
                            #xmlAttribute{
                                name = borderId,
                                value = "1"
                            },
                            #xmlAttribute{
                                name = applyFill,
                                value = "1"
                            },
                            #xmlAttribute{
                                name = applyBorder,
                                value = "1"
                            },
                            #xmlAttribute{
                                name = applyProtection,
                                value = "1"
                            }
                        ]
                    },
                    #xmlElement{
                        name = xf,
                        attributes = [
                            #xmlAttribute{
                                name = applyProtection,
                                value = "1"
                            }
                        ],
                        content = [
                            #xmlElement{
                                name = protection,
                                attributes = [
                                    #xmlAttribute{
                                        name = locked,
                                        value = "0"
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }.


'xl/JDEData.bin'(_) ->
    #xmlElement{
        name = document,
        attributes = [
            #xmlAttribute{
                name = version,
                value = "2.0"
            }
        ],
        content = [
            #xmlElement{
                name = name,
                content = [
                    #xmlText{
                        value = "Project"
                    }
                ]
            },
            #xmlElement{
                name = property,
                attributes = [
                    #xmlAttribute{
                        name = desc,
                        value = ""
                    },
                    #xmlAttribute{
                        name = lock,
                        value = "false"
                    },
                    #xmlAttribute{
                        name = password,
                        value = ""
                    }
                ]
            },
            #xmlElement{
                name = activemodule,
                content = [
                    #xmlText{
                        value = "1"
                    }
                ]
            },
            #xmlElement{
                name = codemodule,
                attributes = [
                    #xmlAttribute{
                        name = id,
                        value = "1"
                    },
                    #xmlAttribute{
                        name = name,
                        value = "JsMacro"
                    }
                ],
                content = [
                    #xmlElement{
                        name = window,
                        attributes = [
                            #xmlAttribute{
                                name = cursorpos,
                                value = "1"
                            },
                            #xmlAttribute{
                                name = actived,
                                value = "true"
                            },
                            #xmlAttribute{
                                name = visible,
                                value = "true"
                            }
                        ]
                    },
                    #xmlElement{
                        name = codetext,
                        content = [
                            #xmlText{
                                value = multiple_select_macro()
                            }
                        ]
                    }
                ]
            },
            #xmlElement{
                name = functionsdata
            }
        ]
    }.


multiple_select_macro() ->
    "// compose select on sheet change
function Workbook_SheetChange(sheet, range) {
    Delete_Validation(range);
    const name = Get_Validation(range);
    if(name.split('-').pop() !== 'set') return;
    // begin
    Application.EnableEvents = false;
    const value = Get_Value(range);
    // Set_Sheet(Get_Sheet(name), value);
    range.Value2 = value;
    Application.EnableEvents = true;
}

function Delete_Validation(range) {
    if(range.EntireRow.Address() == '$1:$1048576') {
        try {
            range.Validation.Delete();
        } catch(e) {

        }
    }
}

function Get_Validation(range) {
    try {
        const formula = range.Validation.Formula1.split('=')[1] || '';
        const table = formula.split('!')[0] || '';
        return table.replace(/^'/, '').replace(/'$/, '');
    } catch (e) {
        return '';
    }
}

function Get_Value(range) {
    const newValue = range.Text || '';
    Application.Undo();
    const oldValue = range.Text || '';
    // empty
    if(!oldValue) return newValue;
    // add or remove
    return oldValue.indexOf(newValue) >= 0 ? oldValue.split(',').filter(i => i !== newValue).join(',') : oldValue + ',' + newValue;
}

function Get_Sheet(name) {
    const sheets = Application.ActiveWorkbook.Sheets
    // get validation sheet
    for (let i = 1; i <= sheets.Count; i++) {
        if(name == sheets.Item(i).Name) {
            return sheets.Item(i);
        }
    }
}

function Set_Sheet(sheet, data) {

    // find duplicate
    for(let row = 1; row <= sheet.Rows.Count; row++) {
        if(!(sheet.Range(`A${row}`).Text)) break;
        if(data == sheet.Range(`A${row}`).Text) return;
    }

    // take value-key pair
    const map = {};
    let row = 1;
    for(; row <= sheet.Rows.Count; row++) {
        const value = sheet.Range(`A${row}`).Text;
        const key = sheet.Range(`B${row}`).Text;
        if(!value || !key) break;
        map[value] = key;
    }

    // compose new value key pair
    const key = data.split(',').map(i => map[i]).join(',');
    sheet.Unprotect({});
    sheet.Range(`A${row}`).Value2 = data;
    sheet.Range(`B${row}`).Value2 = key;
    sheet.Protect({});
}".


'xl/workbook.xml'(Sheet) ->

    SheetNames = make_sheet_name(maps:values(Sheet), 1, []),

    #xmlElement{
        name = workbook,
        attributes = [
            #xmlAttribute{
                name = xmlns,
                value = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
            },
            #xmlAttribute{
                name = 'xmlns:r',
                value = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
            }
        ],
        content = [
            #xmlElement{
                name = 'fileVersion',
                attributes = [
                    #xmlAttribute{
                        name = 'appName',
                        value = "xl"
                    }
                ]
            },
            #xmlElement{
                name = sheets,
                content = SheetNames
            }
        ]
    }.


make_sheet_name([], _, List) ->
    List;
make_sheet_name([#{name := Name, state := State} | SheetName], Index, List) ->
    Element = #xmlElement{
        name = 'sheet',
        attributes = [
            #xmlAttribute{name = 'name', value = Name},
            #xmlAttribute{name = 'sheetId', value = lists:concat([Index])},
            #xmlAttribute{name = 'r:id', value = lists:concat(["rId", Index])},
            #xmlAttribute{name = 'state', value = State}
        ]
    },
    make_sheet_name(SheetName, Index + 1, [Element | List]).


'xl/worksheets'(Sheet) ->
    make_work_sheet(maps:values(Sheet), 1, []).


make_work_sheet([], _, List) ->
    lists:reverse(List);
make_work_sheet([Sheet = #{data := [Head | _] = Data, meta := Meta} | T], Index, List) ->

    SheetData = make_sheet_row(Data, 1, length(Head), []),

    DataValidation = make_sheet_validation(maps:values(Meta), []),

    Protection = make_sheet_protect(Sheet),

    Element = #xmlElement{
        name = worksheet,
        attributes = [
            #xmlAttribute{
                name = xmlns,
                value = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
            },
            #xmlAttribute{
                name = 'xmlns:r',
                value = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
            },
            #xmlAttribute{
                name = 'xmlns:xdr',
                value = "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"
            },
            #xmlAttribute{
                name = 'xmlns:x14',
                value = "http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
            },
            #xmlAttribute{
                name = 'xmlns:mc',
                value = "http://schemas.openxmlformats.org/markup-compatibility/2006"
            },
            #xmlAttribute{
                name = 'xmlns:etc',
                value = "http://www.wps.cn/officeDocument/2017/etCustomData"
            }
        ],
        content = [
            #xmlElement{
                name = sheetFormatPr,
                attributes = [
                    #xmlAttribute{
                        name = 'defaultColWidth',
                        value = "12.26"
                    },
                    #xmlAttribute{
                        name = 'defaultRowHeight',
                        value = "22"
                    },
                    #xmlAttribute{
                        name = 'customWidth',
                        value = "1"
                    },
                    #xmlAttribute{
                        name = 'customHeight',
                        value = "1"
                    }
                ]
            },
            #xmlElement{
                name = sheetViews,
                content = [
                    #xmlElement{
                        name = sheetView,
                        content = [
                            #xmlElement{
                                name = pane,
                                attributes = [
                                    #xmlAttribute{
                                        name = ySplit,
                                        value = "3"
                                    },
                                    #xmlAttribute{
                                        name = state,
                                        value = "frozen"
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
            #xmlElement{
                name = cols,
                content = [
                    #xmlElement{
                        name = col,
                        attributes = [
                            #xmlAttribute{
                                name = min,
                                value = "1"
                            },
                            #xmlAttribute{
                                name = max,
                                value = "16384"
                            },
                            #xmlAttribute{
                                name = width,
                                value = "12.26"
                            },
                            #xmlAttribute{
                                name = height,
                                value = "22"
                            },
                            %% @doc style 3 is applyProtection="1" but locked="0"
                            #xmlAttribute{
                                name = style,
                                value = "3"
                            }
                        ]
                    }
                ]
            },
            #xmlElement{
                name = sheetData,
                content = SheetData
            },
            #xmlElement{
                name = dataValidations,
                attributes = [
                    #xmlAttribute{
                        name = count,
                        value = lists:concat([length(DataValidation)])
                    }
                ],
                content = DataValidation
            },
            Protection
        ]
    },

    make_work_sheet(T, Index + 1, [#{name => lists:concat(["xl/worksheets/sheet", Index, ".xml"]), data => Element} | List]).


make_sheet_row([], _, _, List) ->
    lists:reverse(List);
make_sheet_row([Row | RowData], RowIndex, Column, List) ->
    %% @todo change test data row index
    %% @doc style 1 is applyProtection="1"
    %% @doc style 2 is applyProtection="1" and fillId="2" and borderId="1"
    %% @doc style 3 is applyProtection="1" but locked="0"
    Style = maps:get(RowIndex, #{1 => <<"1">>, 3 => <<"1">>, 2 => <<"1">>}, "3"),
    Columns = make_sheet_column(Row, RowIndex, 1, []),
    Element = #xmlElement{
        name = 'row',
        attributes = [
            #xmlAttribute{name = 'r', value = lists:concat([RowIndex])},
            #xmlAttribute{name = 's', value = Style},
            #xmlAttribute{name = 'customFormat', value = "1"},
            #xmlAttribute{name = 'customHeight', value = "1"},
            #xmlAttribute{name = 'spans', value = lists:concat(["1:", Column])}
        ],
        content = Columns
    },
    make_sheet_row(RowData, RowIndex + 1, Column, [Element | List]).


make_sheet_column([], _, _, List) ->
    lists:reverse(List);
make_sheet_column([Column | ColumnData], RowIndex, ColumnIndex, List) ->
    %% @todo change test data row index
    %% @doc style 1 is applyProtection="1"
    %% @doc style 2 is applyProtection="1" and fillId="2" and borderId="1"
    %% @doc style 3 is applyProtection="1" but locked="0"
    Style = maps:get(RowIndex, #{1 => <<"1">>, 3 => <<"1">>, 2 => <<"1">>}, "3"),
    Element = #xmlElement{
        name = 'c',
        attributes = [
            #xmlAttribute{
                name = 'r',
                value = unicode:characters_to_list([$@ + ColumnIndex, integer_to_list(RowIndex)])
            },
            #xmlAttribute{
                name = 's',
                value = Style
            }
        ],
        content = [
            #xmlElement{
                name = 'v',
                content = [
                    #xmlText{
                        value = make_value(Column)
                    }
                ]
            }
        ]
    },
    make_sheet_column(ColumnData, RowIndex, ColumnIndex + 1, [Element | List]).


%% <Data ss:Type="Number"></Data>
make_value(Text) when is_number(Text) ->
    lists:flatten(io_lib:format("~w", [Text]));
%% <Data ss:Type="String"></Data>
make_value(Text) when is_binary(Text) orelse is_atom(Text) orelse is_list(Text) ->
    lists:flatten(io_lib:format("~ts", [Text])).


%% worksheet property
make_sheet_validation([], List) ->
    List;
%% <DataValidation></DataValidation>
make_sheet_validation([#{type := Type, column := Column, sheet := Sheet} | T], List) when Type == enum orelse Type == set ->
    Element = #xmlElement{
        name = 'dataValidation',
        attributes = [
            #xmlAttribute{
                name = type,
                value = "list"
            },
            #xmlAttribute{
                name = allowBlank,
                value = "1"
            },
            #xmlAttribute{
                name = showInputMessage,
                value = "1"
            },
            #xmlAttribute{
                name = showErrorMessage,
                value = "1"
            },
            #xmlAttribute{
                name = sqref,
                value = unicode:characters_to_list([$@ + Column, "3:", $@ + Column, "1048576"])
            }
        ],
        content = [
            #xmlElement{
                name = formula1,
                content = [
                    #xmlText{
                        %% value = lists:concat(["\"", string:join([Value || [Value, _] <- Validation], ","), "\""])
                        value = lists:concat(["'", Sheet, "'", "!", "$A:$A"])
                    }
                ]
            }
        ]
    },
    make_sheet_validation(T, [Element | List]);
%% other column
make_sheet_validation([_ | T], List) ->
    make_sheet_validation(T, List).


make_sheet_protect(#{}) ->
    #xmlElement{
        name = sheetProtection,
        attributes = [
            #xmlAttribute{
                name = sheet,
                value = "1"
            },
            #xmlAttribute{
                name = formatCells,
                value = "0"
            },
            #xmlAttribute{
                name = formatColumns,
                value = "0"
            },
            #xmlAttribute{
                name = formatRows,
                value = "0"
            },
            #xmlAttribute{
                name = insertRows,
                value = "0"
            },
            #xmlAttribute{
                name = insertColumns,
                value = "0"
            },
            #xmlAttribute{
                name = insertHyperlinks,
                value = "0"
            },
            #xmlAttribute{
                name = deleteColumns,
                value = "0"
            },
            #xmlAttribute{
                name = deleteRows,
                value = "0"
            },
            #xmlAttribute{
                name = sort,
                value = "0"
            },
            #xmlAttribute{
                name = autoFilter,
                value = "0"
            },
            #xmlAttribute{
                name = pivotTables,
                value = "0"
            },
            #xmlAttribute{
                name = objects,
                value = "1"
            }
        ]
    };
make_sheet_protect(#{protected := true}) ->
    #xmlElement{
        name = sheetProtection,
        attributes = [
            #xmlAttribute{
                name = sheet,
                value = "1"
            },
            #xmlAttribute{
                name = object,
                value = "1"
            }
        ]
    };
make_sheet_protect(#{}) ->
    #xmlText{value = ""}.


%%%===================================================================
%%% XLS to Table
%%%===================================================================
%% @doc xls to table
-spec read(Binary :: binary()) -> {ok, Xls :: #{}} | {error, Reason :: term()}.
read(Binary) ->
    {ok, FileList} = zip:extract(Binary, [memory]),
    Files = maps:from_list([{Name, binary_to_list(Data)} || {Name, Data} <- FileList]),
    read_relation_ship(Files).


read_relation_ship(Files) ->
    Data = maps:get("_rels/.rels", Files),
    {Xml, []} = xmerl_scan:string(Data, [{encoding, "utf-8"}]),
    RelationShip = xmerl_xpath:string("//Relationships//Relationship", Xml),
    [Element] = [Element || Element = #xmlElement{attributes = Attributes} <- RelationShip, lists:any(fun(#xmlAttribute{value = Value}) -> filename:basename(Value) == "officeDocument" end, Attributes)],
    read_document(Files, Element).


read_document(Files, #xmlElement{attributes = Attributes}) ->
    #xmlAttribute{value = File} = lists:keyfind('Target', #xmlAttribute.name, Attributes),
    Data = maps:get(File, Files),
    {Xml, []} = xmerl_scan:string(Data),
    SheetElement = xmerl_xpath:string("//workbook//sheets//sheet", Xml),
    WorkBookSheet = read_document_sheet_attributes(SheetElement, #{}),
    read_relation_ship_sheet(Files, File, WorkBookSheet).


read_document_sheet_attributes([], Map) ->
    Map;
read_document_sheet_attributes([#xmlElement{attributes = Attributes} | T], Map) ->
    #xmlAttribute{value = Name} = listing:key_find('name', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    #xmlAttribute{value = SheetId} = listing:key_find('sheetId', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    #xmlAttribute{value = RelationShipId} = listing:key_find('r:id', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    #xmlAttribute{value = State} = listing:key_find('state', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    Item = #{rid => RelationShipId, id => SheetId, name => Name, state => State},
    read_document_sheet_attributes(T, Map#{RelationShipId => Item});
read_document_sheet_attributes([_ | T], Map) ->
    read_document_sheet_attributes(T, Map).


read_relation_ship_sheet(Files, BookFile, WorkBookSheet) ->
    Path = filename:dirname(BookFile),
    Name = filename:basename(BookFile),
    RelationShipFile = lists:concat([Path, "/_rels/", Name, ".rels"]),
    Data = maps:get(RelationShipFile, Files),
    {Xml, []} = xmerl_scan:string(Data),
    RelationShip = xmerl_xpath:string("//Relationships//Relationship", Xml),
    %% stare string
    ShareStringElement = [Element || Element = #xmlElement{attributes = Attributes} <- RelationShip, lists:any(fun(#xmlAttribute{value = Value}) -> filename:basename(Value) == "sharedStrings" end, Attributes)],
    ShareString = read_share_string(Files, ShareStringElement),
    %% work book
    WorkBookElement = [Element || Element = #xmlElement{attributes = Attributes} <- RelationShip, lists:any(fun(#xmlAttribute{value = Value}) -> filename:basename(Value) == "worksheet" end, Attributes)],
    read_work_book(Files, Path, WorkBookSheet, ShareString, WorkBookElement, #{}).


read_share_string(_, []) ->
    #{};
read_share_string(Files, [#xmlElement{attributes = Attributes}]) ->
    #xmlAttribute{value = File} = lists:keyfind('Target', #xmlAttribute.name, Attributes),
    Data = maps:get(lists:flatten(lists:concat(["xl/", File])), Files),
    {Xml, []} = xmerl_scan:string(Data),
    StringElement = xmerl_xpath:string("//sst//si//t//text()", Xml),
    read_share_string_text(StringElement, 0, []).


read_share_string_text([], _, List) ->
    maps:from_list(List);
read_share_string_text([#xmlText{value = Value} | T], Index, List) ->
    read_share_string_text(T, Index + 1, [{Index, Value} | List]).


read_work_book(_, _, _, _, [], Map) ->
    %% restore after load work book
    restore(Map, maps:values(Map), #{});
read_work_book(Files, AppPath, WorkBookSheet, ShareString, [#xmlElement{attributes = Attributes} | T], Map) ->
    %% name
    #xmlAttribute{value = Id} = lists:keyfind('Id', #xmlAttribute.name, Attributes),
    SheetProperty = #{name := Name} = maps:get(Id, WorkBookSheet),
    %% file
    #xmlAttribute{value = File} = lists:keyfind('Target', #xmlAttribute.name, Attributes),
    Data = maps:get(lists:flatten(lists:concat([AppPath, "/", File])), Files),
    {Xml, []} = xmerl_scan:string(Data),
    %% first work book
    [#xmlElement{name = worksheet, content = BookElement}] = xmerl_xpath:string("//worksheet", Xml),
    Sheet = read_work_sheet(Files, WorkBookSheet, ShareString, Name, BookElement, SheetProperty),
    read_work_book(Files, AppPath, WorkBookSheet, ShareString, T, Map#{Name => Sheet}).


read_work_sheet(_, _, _, _, [], Map) ->
    Map;
read_work_sheet(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = sheetData, content = Content} | T], Map) ->
    %% load work book sheetData/row/c/v
    Columns = read_work_sheet_data_row_columns(Files, WorkBookSheet, ShareString, Name, Content, 0),
    Data = read_work_sheet_data_row(Files, WorkBookSheet, ShareString, Name, Content, Columns, []),
    read_work_sheet(Files, WorkBookSheet, ShareString, Name, T, Map#{data => Data});
read_work_sheet(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = dataValidations, content = Content} | T], Map) ->
    %% load work book dataValidations/dataValidation/formula1
    Validation = read_work_sheet_data_validation(Files, WorkBookSheet, ShareString, Name, Content, []),
    read_work_sheet(Files, WorkBookSheet, ShareString, Name, T, Map#{validation => Validation});
read_work_sheet(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = sheetProtection} | T], Map) ->
    %% protected sheet
    read_work_sheet(Files, WorkBookSheet, ShareString, Name, T, Map#{protected => true});
read_work_sheet(Files, WorkBookSheet, ShareString, Name, [_ | T], Map) ->
    read_work_sheet(Files, WorkBookSheet, ShareString, Name, T, Map).


read_work_sheet_data_row_columns(_, _, _, _, [#xmlElement{name = row, attributes = Attributes} | _], 0) ->
    %% spans="1:15"
    #xmlAttribute{value = Span} = lists:keyfind('spans', #xmlAttribute.name, Attributes),
    [_, Columns] = string:tokens(Span, ":"),
    list_to_integer(Columns);
read_work_sheet_data_row_columns(Files, WorkBookSheet, ShareString, Name, [_ | T], Columns) ->
    read_work_sheet_data_row_columns(Files, WorkBookSheet, ShareString, Name, T, Columns).


read_work_sheet_data_row(_, _, _, _, [], _, List) ->
    lists:reverse(List);
read_work_sheet_data_row(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = row, content = Content} | T], Columns, List) ->
    Array = array:new(Columns, [{default, []}]),
    Row = read_work_sheet_data_column(Files, WorkBookSheet, ShareString, Name, Content, Array),
    read_work_sheet_data_row(Files, WorkBookSheet, ShareString, Name, T, Columns, [Row | List]);
read_work_sheet_data_row(Files, WorkBookSheet, ShareString, Name, [_ | T], Columns, List) ->
    read_work_sheet_data_row(Files, WorkBookSheet, ShareString, Name, T, Columns, List).


read_work_sheet_data_column(_, _, _, _, [], Array) ->
    array:to_list(Array);
read_work_sheet_data_column(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = c, attributes = Attributes, content = Content} | T], Array) ->
    %% position
    #xmlAttribute{value = Position} = lists:keyfind('r', #xmlAttribute.name, Attributes),
    Index = read_work_sheet_data_column_from_position(Position, 0, 0),
    %% type
    #xmlAttribute{value = Type} = listing:key_find('t', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    Value = read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type, Content, []),
    NewArray = array:set(Index, Value, Array),
    read_work_sheet_data_column(Files, WorkBookSheet, ShareString, Name, T, NewArray);
read_work_sheet_data_column(Files, WorkBookSheet, ShareString, Name, [_ | T], Array) ->
    read_work_sheet_data_column(Files, WorkBookSheet, ShareString, Name, T, Array).


%% convert column ABC66 to index
%% ($A + 0 * 26) - $A
%% ($B + 1 * 26) - $A
%% ($C + 2 * 26) - $A
read_work_sheet_data_column_from_position([], Index, _) ->
    Index;
read_work_sheet_data_column_from_position([C | _], Index, _) when $0 =< C andalso C =< $9 ->
    Index;
read_work_sheet_data_column_from_position([C | T], Index, Depth) ->
    read_work_sheet_data_column_from_position(T, (C + Depth * 26) - $A + Index, Depth + 1).


read_work_sheet_data_column_value(_, _, _, _, _, [], List) ->
    lists:append(List);
read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type = "s", [#xmlElement{name = 'v', content = [#xmlText{value = Index}]} | T], List) ->
    %% find from share string by index
    Value = maps:get(list_to_integer(Index), ShareString),
    read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type, T, [Value | List]);
read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type = undefined, [#xmlElement{name = 'v', content = [#xmlText{value = Value}]} | T], List) ->
    %% literal value
    read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type, T, [Value | List]);
read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type, [_ | T], List) ->
    %% literal value
    read_work_sheet_data_column_value(Files, WorkBookSheet, ShareString, Name, Type, T, ["" | List]).


read_work_sheet_data_validation(_, _, _, _, [], List) ->
    lists:reverse(List);
read_work_sheet_data_validation(Files, WorkBookSheet, ShareString, Name, [#xmlElement{name = 'dataValidation', attributes = Attributes, content = Content} | T], List) ->
    %% name
    #xmlAttribute{value = Type} = lists:keyfind('type', #xmlAttribute.name, Attributes),

    %% position
    #xmlAttribute{value = Ref} = lists:keyfind('sqref', #xmlAttribute.name, Attributes),
    [Position | _] = string:tokens(Ref, ":"),
    Index = read_work_sheet_data_column_from_position(Position, 1, 0),

    %% formula
    [Formula] = [Value || #xmlElement{content = [#xmlText{value = Value}]} <- Content],
    [Token | _] = string:tokens(Formula, "!"),
    SheetName = tl(lists:droplast(Token)),

    read_work_sheet_data_validation(Files, WorkBookSheet, ShareString, Name, T, [#{column => Index, type => list_to_atom(Type), name => SheetName} | List]);
read_work_sheet_data_validation(Files, WorkBookSheet, ShareString, Name, [_ | T], List) ->
    read_work_sheet_data_validation(Files, WorkBookSheet, ShareString, Name, T, List).


%%%===================================================================
%%% restore part
%%%===================================================================

restore(_, [], Map) ->
    Map;
restore(Book, [Sheet = #{name := SheetName, data := [Column, Comment | RowData], validation := ValidationData} | T], Map) ->
    NewTableData = restore_row_validation(RowData, Book, SheetName, ValidationData, 1, []),
    NewSheet = Sheet#{data => [Column, Comment | NewTableData]},
    restore(Book, T, Map#{SheetName => NewSheet});
restore(Book, [Sheet = #{name := SheetName} | T], Map) ->
    restore(Book, T, Map#{SheetName => Sheet});
restore(Book, [{Name, Sheet} | T], Map) ->
    restore(Book, T, Map#{Name => Sheet}).


restore_row_validation([], _, _, _, _, List) ->
    lists:reverse(List);
restore_row_validation([Row | RowData], Book, SheetName, ValidationData, Index, List) ->
    NewRow = restore_cell_validation(Row, Book, SheetName, ValidationData, 1, []),
    restore_row_validation(RowData, Book, SheetName, ValidationData, Index + 1, [NewRow | List]).


restore_cell_validation([], _, _, _, _, List) ->
    lists:reverse(List);
restore_cell_validation([Cell | CellData], Book, SheetName, ValidationData, Index, List) ->
    NewCell = restore_validation(ValidationData, Book, SheetName, Index, Cell),
    restore_cell_validation(CellData, Book, SheetName, ValidationData, Index + 1, [NewCell | List]).


restore_validation([], _, _, _, Cell) ->
    Cell;
%% SheetName!R3C6:R1048576C6
restore_validation([#{type := list, column := Column, name := Name} | T], Book, SheetName, Column, Cell) ->
    #{data := Data} = maps:get(Name, Book),
    %% enum/set
    NewCell = string:join([find_loop(Data, Option) || Option <- string:tokens(Cell, ",")], ","),
    restore_validation(T, Book, SheetName, Column, NewCell);
restore_validation([_ | T], Book, SheetName, Column, Cell) ->
    restore_validation(T, Book, SheetName, Column, Cell).


find_loop([], Value) ->
    Value;
find_loop([[Value, Key | _] | _], Value) ->
    Key;
find_loop([_ | T], Value) ->
    find_loop(T, Value).
