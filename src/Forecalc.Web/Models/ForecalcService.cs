using Forecalc.Library;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Forecalc.Web.Models;

public class ForecalcService(FSharpMap<string, QT4<CellContent>> workbook)
{
    private const string Sheet = "Sheet1";

    public CellContentDTO[][] Put(int row, int col, string expr)
    {
        var absCell = new AbsCell(Sheet, row, col);

        try
        {
            Workbook.recalculate(absCell, expr, workbook);
        }
        catch
        {
            Workbook.recalculate(absCell, string.Empty, workbook);
            throw;
        }

        var cells = Workbook.toArray(Sheet, 1, 1, 20, 100, workbook);

        var values = new CellContentDTO[100][];

        for (var r = 0; r < cells.GetLength(1); r++)
        {
            values[r] = new CellContentDTO[20];

            for (var c = 0; c < cells.GetLength(0); c++)
            {
                values[r][c] = ConvertToTuple(cells[c, r]);
            }
        }

        return values;
    }

    public bool Reset()
    {
        var clear = FSharpFunc<CellContent, CellContent>.FromConverter(Empty);
        
        QT4.apply(clear, workbook[Sheet]);

        return true;
    }

    private static CellContent Empty(CellContent cell) => new(Expr.Blank, CellValue.NullValue, false);

    private static CellContentDTO ConvertToTuple(CellValue value)
    {
        var type = "";
        var str = Eval.toString(value);
        if (value.IsBooleanValue)
        {
            type = "bool";
        }
        if (value.IsErrorValue)
        {
            type = "error";
        }
        if (value.IsFloatValue)
        {
            type = "float";
        }
        if (value.IsStringValue)
        {
            type = "string";
        }
        if (value.IsNullValue)
        {
            type = "";
        }
        if (value.IsValueList)
        {
            type = "error";
        }
        return new CellContentDTO(type, str);
    }
}