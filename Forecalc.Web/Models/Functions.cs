using System;
using System.Web;
using Forecalc.Library;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Forecalc.Web.Models
{
    public static class Functions
    {
        private const string Sheet = "Sheet1";

        public static Tuple<string, string>[,] Put(int row, int col, string expr)
        {
            var workbook = LoadWorkbook();

            var absCell = new Ast.AbsCell(Sheet, row, col);

            try
            {
                Workbook.recalculate(absCell, expr, workbook);
            }
            catch
            {
                Workbook.recalculate(absCell, string.Empty, workbook);
                SaveWorkbook(workbook);
                throw;
            }

            var cells = Workbook.toArray(Sheet, 1, 1, 20, 100, workbook);

            var mapper = FSharpFunc<CellValue, Tuple<string, string>>.FromConverter(ToTuple);
            var values = Array2DModule.Map(mapper, cells);

            SaveWorkbook(workbook);

            return values;
        }

        private static FSharpMap<string, QT4.qt4<CellContent>> LoadWorkbook()
        {
            return HttpContext.Current.Session["Workbook"] as FSharpMap<string, QT4.qt4<CellContent>> ??
                   MapModule.Add(Sheet, QT4.create<CellContent>(),
                                 MapModule.Empty<string, QT4.qt4<CellContent>>());
        }

        private static void SaveWorkbook(FSharpMap<string, QT4.qt4<CellContent>> workbook)
        {
            HttpContext.Current.Session["Workbook"] = workbook;
        }

        public static void ResetWorkbook()
        {
            SaveWorkbook(null);
        }

        private static Tuple<string, string> ToTuple(CellValue value)
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
            return Tuple.Create(type, str);
        }
    }
}