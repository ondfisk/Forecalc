$(function() {
    function put(row, col, expr) {
        edit = false;
        var url = $("#put-url").val();
        $.ajax({
            type: "POST",
            url: url,
            data: { row: row, col: col, expr: expr },
            success: function(data) {
                $("#R" + (row + 1) + "C" + (col + 1))
                    .find(".expr").data("old", expr).hide()
                    .parent().find(".text").show();
                for(var r = 0; r < 100; r++) {
                    for(var c = 0; c < 20; c++) {
                        var tuple = data[c * 100 + r];
                        var cell = $("#R" + (r + 1) + "C" + (c + 1) + "-text");
                        cell.text(tuple.Item2);
                        cell.attr("class", "text " + tuple.Item1);
                    }
                }
            },
            error: function (xhr, status, error) {
                var msg = JSON.parse(xhr.responseText);
                window.alert(msg.Message);
            }
        });
    }

    $(".cell").live("click", function() {
        $(this).find(".text").hide();
        $(this).find(".expr").show().focus().css("background", "white");
    });
    $(".expr").live("blur", function() {
        $(this).hide().parent().find(".text").show();
    });
    $(".expr").live("change", function() {
        var row = $(this).data("row");
        var col = $(this).data("col");
        var expr = $(this).val();
        put(row, col, expr);
    });
    $("#refstyle").bind("change", function() {
        $(".r1c1").toggle();
        $(".a1").toggle();
    });
    $("#reset").bind("click", function() {
        var url = $("#reset-url").val();
        $.post(url, function() {
            $(".expr").val("").data("old", "");
            $(".text").text("").attr("class", "text");
        });
    });
    var edit = false;
    $(".expr").live("keydown", function(event) {
        var row = parseInt($(this).data("row"));
        var col = parseInt($(this).data("col"));
        var old = $(this).data("old");
        var expr = $(this);
        switch(event.which) {
            case 27: // esc
                expr.val(old);
                expr.blur();
                break;
            case 13: // return
                expr.blur();
                if(expr.val() != old) {
                    put(row, col, expr.val());
                }
                activate(row + 1, col);
                break;
            case 37: // left
                if(!edit) {
                    expr.blur();
                    if(expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row, col - 1);
                }
                break;
            case 38: // up
                if(!edit) {
                    expr.blur();
                    if(expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row - 1, col);
                }
                break;
            case 39: // right
                if(!edit) {
                    expr.blur();
                    if(expr.val() != old) {
                        put(row, col, expr.val());
                    }
                    activate(row, col + 1);
                }
                break;
            case 40: // down
                if(!edit) {
                    expr.blur();
                    if(expr.val() != old) {
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
            .find(".expr").show().focus().caretToEnd().css("background", "white");
    }
});