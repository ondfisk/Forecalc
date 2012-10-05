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

    function recalculate() {
        var url = $("#recalculate-url").val();
        $.ajax({
            type: "POST",
            url: url,
            data: {},
            success: function (data) {
                for (var r = 0; r < 100; r++) {
                    for (var c = 0; c < 20; c++) {
                        var tuple = data[c * 100 + r];
                        var cell = $("#R" + (r + 1) + "C" + (c + 1) + "-text");
                        cell.text(tuple.Item2);
                        cell.attr("class", "text " + tuple.Item1);
                    }
                }
            },
            error: function (xhr) {
                var response = JSON.parse(xhr.responseText);
                window.alert(response.Message);
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
    
    $("#recalculate").bind("click", function () {
        recalculate();
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
    
    var bar = $('.bar');
    var percent = $('.percent');
    var status = $('#status');

    $("#load-extension-dialog").dialog({ autoOpen: false });
    
    $("#load-extension").click(function () {
        $("#file-paragraph").html('<input id="file" name="file" type="file" />');
        var percentVal = "0%";
        bar.width(percentVal);
        percent.html(percentVal);
        status.html("");
        $("#load-extension-dialog").dialog("open");
    });

    $("#load-extension-form").ajaxForm({
        beforeSend: function () {
            status.empty();
            var percentVal = "0%";
            bar.width(percentVal);
            percent.html(percentVal);
        },
        uploadProgress: function (event, position, total, percentComplete) {
            var percentVal = percentComplete + "%";
            bar.width(percentVal);
            percent.html(percentVal);
        },
        complete: function (xhr) {
            var response = JSON.parse(xhr.responseText);
            status.html(response.Message);
            $("#file-paragraph").html('<input id="file" name="file" type="file" />');
            if (response.Count > 0) {
                $("#load-extension-dialog").dialog("close");
                recalculate();
            }
        }
    });
});