$(function () {
    function put(row, col, expr) {
        edit = false;
        $.ajax({
            method: "PUT",
            url: "/forecalc",
            data: JSON.stringify({ row: row, col: col, expr: expr }),
            contentType: "application/json; charset=utf-8",
            dataType: "json",
            success: function (data) {
                $("#R" + (row + 1) + "C" + (col + 1))
                    .find(".expr").data("old", expr).hide()
                    .parent().find(".text").show();
                for (var r = 0; r < 100; r++) {
                    for (var c = 0; c < 20; c++) {
                        var tuple = data[r][c];
                        var cell = $("#R" + (r + 1) + "C" + (c + 1) + "-text");
                        cell.text(tuple.value);
                        cell.attr("class", "text");
                        cell.addClass(tuple.type);
                    }
                }
            },
            error: function (xhr, status, error) {
                var msg = JSON.parse(xhr.responseText);
                window.alert(msg.Message);
            },
        });
    }

    $(document).on("click", ".cell", function () {
        $(this).find(".text").hide();
        $(this).find(".expr").show().trigger("focus").css("background", "white");
    });
    $(document).on("blur", ".expr", function () {
        $(this).hide().parent().find(".text").show();
    });
    $(document).on("change", ".expr", function () {
        var row = $(this).data("row");
        var col = $(this).data("col");
        var expr = $(this).val();
        put(row, col, expr);
    });
    $("#refstyle").on("change", function () {
        $(".r1c1").toggle();
        $(".a1").toggle();
    });
    $("#reset").on("click", function () {
        $.ajax({
            method: "DELETE",
            url: "/forecalc",
            dataType: "json",
            success: function () {
                $(".expr").val("").data("old", "");
                $(".text").text("").attr("class", "text");
            }
        });
    });
    var edit = false;
    $(document).on("keydown", ".expr", function (event) {
        var row = parseInt($(this).data("row"));
        var col = parseInt($(this).data("col"));
        var old = $(this).data("old");
        var expr = $(this);
        switch (event.which) {
            case 27: // esc
                expr.val(old);
                expr.trigger("blur");
                break;
            case 13: // return
                expr.trigger("blur");
                if (expr.val() != old) {
                    put(row, col, expr.val());
                }
                activate(row + 1, col);
                break;
            case 37: // left
                if (!edit) {
                    expr.trigger("blur");
                    if (expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row, col - 1);
                }
                break;
            case 38: // up
                if (!edit) {
                    expr.trigger("blur");
                    if (expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row - 1, col);
                }
                break;
            case 39: // right
                if (!edit) {
                    expr.trigger("blur");
                    if (expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row, col + 1);
                }
                break;
            case 40: // down
                if (!edit) {
                    expr.trigger("blur");
                    if (expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row + 1, col);
                }
                break;
            case 113: // f2
                edit = true;
                expr.caretToEnd().css("background", "pink");
                break;
        }
    });
    function activate(row, col) {
        $("#R" + row + "C" + col)
            .find(".text").hide().end()
            .find(".expr").show().trigger("focus").caretToEnd().css("background", "white");
    }
});