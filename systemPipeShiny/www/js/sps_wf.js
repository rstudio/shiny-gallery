// code for workflow modules

//change background on workflow setup step
$(document).ready(function(){
    var li_find = false;
    var setup_box_img_src = "";
    $("#wf-wf_setup-choose_wf").parent().on('DOMNodeInserted', function(){
      if($(this).find('div').length > 0 && li_find == false){
        //li_find = true;
        let dropdown = $(this).find('.selectize-dropdown');
        // on hover
        dropdown.on('mouseenter', '.option', function(){
          var current_li = $(this).text();
          let img_src = findWFimage(current_li);
          if(img_src != setup_box_img_src){
            $("#setup-box-img").fadeOut('fast', function () {
              $(this).attr('src', img_src);
              $(this).fadeIn('fast');
            });
            setup_box_img_src = img_src;
          }
        });
        // on final selection
        dropdown.on('mouseleave', 'div', function(){
          let final_select = $('#wf-wf_setup-choose_wf').text();
          let img_src = findWFimage(final_select);
          if(setup_box_img_src != img_src){
            $("#setup-box-img").fadeOut('fast', function () {
               $(this).attr('src', img_src);
               $(this).fadeIn('fast');
            });
            setup_box_img_src = img_src;
          }
        });
      }
    });
});

// find img src based on choice
function findWFimage(choice){
  let img_src;
  switch(choice) {
     case 'RNAseq':
       img_src = 'sps/img/rnaseq.png';
       break;
     case 'Varseq':
       img_src = 'sps/img/varseq.png';
       break;
     case 'Riboseq':
       img_src = 'sps/img/riboseq.png';
       break;
     case 'Chipseq':
       img_src = 'sps/img/chipseq.png';
       break;
     case 'Empty':
       img_src = 'sps/img/empty.png';
       break;
     case 'Existing':
       img_src = 'sps/img/exist.png';
       break;
     case 'Example':
       img_src = 'sps/img/example.png';
       break;
     default:
       img_src = 'sps/img/spr.png';
  }
    return img_src;
}


// main panel heading icons
$(document).ready(function(){
  // CWL tab panel icons
  $("#wf-wf_panel>div>.panel-heading>h4").prepend('<img src="sps/img/spr.png" height="40" width="40">');
  $("#wf-wf_cwl-cwl_panel>div>.panel-heading>h4:first").prepend('<img src="sps/img/spr.png" height="40" width="60">');
  // update icons
  $("#wf-wf_cwl-cwl_panel>div>.panel-heading>h4:not(:first)").prepend('<img src="sps/img/cwl.png" height="40" width="60">');
});

var rsPlotDefault = 'sps/img/spr.png'; // image src will be used for R session plot panel
Shiny.addCustomMessageHandler('change-panel-icon', function(data) {
    let img_src = findWFimage(data.choice);
    $("#wf-wf_panel>div>.panel-heading>h4 img")
    .attr('src', img_src);
    rsPlotDefault = img_src;
    $("#rs_plot img").attr('src', img_src);
});


// workflow logs, always scroll to bottom
$(function(){
    var logs = $('#core_top-logs');
    this.observerRsLog = new MutationObserver( function(mutations) {
        logs.parent().scrollTop(logs.get(0).scrollHeight);
    }.bind(this));
    this.observerRsLog.observe(logs.get(0), {characterData: true, childList: true});
});

// workflow output, always scroll to bottom
// TODO fix scroll
$(function(){
    var output = $('#core_top-output');
    this.observerRsConsole = new MutationObserver( function(mutations) {
        output.parent().scrollTop(output.get(0).scrollHeight);
    }.bind(this));
    this.observerRsConsole.observe(output.get(0), {characterData: true, childList: true});
});

// tree refresh
$(document).ready(function(){
  $("[id*=rmd_tree]").on("refresh.jstree", function(e){
    $(this).jstree("select_all");
  });
});



// split panel
var splitList = {};
$(function(){
  splitList.lr = Split(['#core_top-leftcol', '#core_top-rightcol'], {
    sizes: [50, 50],
    minSize: 35,
    gutterSize: 5
    });
  splitList.c13 = Split(['#core_top-source_code', '#core_top-console'], {
      minSize: 35,
      gutterSize: 3,
      cursor: 'row-resize',
      direction: 'vertical'
  });
  splitList.c24 = Split(['#core_top-wf_log', '#core_top-plot_panel'], {
      minSize: 35,
      gutterSize: 3,
      cursor: 'row-resize',
      direction: 'vertical'
  });
});


// plots display scroll
var rsPlot = {}; //define object
rsPlot.len = 0;
rsPlot.cur = 0;
rsPlot.srcs = [];
rsPlot.firstPlot = true;
function rsToggleArrow(rsPlot) {
    if(rsPlot.cur=== 0){
        $("#rs-plot-left").prop("disabled", true);
    } else {
        $("#rs-plot-left").prop("disabled", false);
    }
    if(rsPlot.len - 1 === rsPlot.cur || rsPlot.len === 0){
        $("#rs-plot-right").prop("disabled", true);
    } else {
        $("#rs-plot-right").prop("disabled", false);
    }
    if(rsPlot.len === 0) {
        let trashTip = $("#rs-plot-trash")
          .prop("disabled", true)
          .attr('aria-describedby');
        $("#" + trashTip).remove();
        let clearTip = $("#rs-plot-clear")
          .prop("disabled", true)
          .attr('aria-describedby');
        $("#" + clearTip).remove();
        let canvasTip = $("#rs-plot-canvas")
          .prop("disabled", true)
          .attr('aria-describedby');
        $("#" + canvasTip).remove();
        let pngTip = $("#rs-plot-png")
          .prop("disabled", true)
          .attr('aria-describedby');
        $("#" + pngTip).remove();
    } else {
        // enable buttons when there is a plot
        $("#rs-plot-trash").prop("disabled", false);
        $("#rs-plot-clear").prop("disabled", false);
        $("#rs-plot-canvas").prop("disabled", false);
        $("#rs-plot-png").prop("disabled", false);
    }
}

function rsPlotText(rsPlot){
    if(rsPlot.len > 0){
        $('.rs_plot_footer h5').text(`${rsPlot.cur + 1} / ${rsPlot.len}`);
    } else {
        $('.rs_plot_footer h5').text(`0 / 0`);
    }

}

$(document).ready(function(){
    rsToggleArrow(rsPlot);
    $("#rs_plot img").attr('src', rsPlotDefault).addClass('rs-plot-default');
    $("#rs-plot-left").on("click", function(){
        rsPlot.cur -= 1;
        $("#rs_plot img").attr('src', rsPlot.srcs[rsPlot.cur]);
        rsToggleArrow(rsPlot);
        rsPlotText(rsPlot);
    });
    $("#rs-plot-right").on("click", function(){
        rsPlot.cur += 1;
        $("#rs_plot img").attr('src', rsPlot.srcs[rsPlot.cur]);
        rsToggleArrow(rsPlot);
        rsPlotText(rsPlot);
    });
});
// when backend send data
Shiny.addCustomMessageHandler("rs_plot", function(data){
    rsPlot.srcs = rsPlot.srcs.concat(data);
    rsPlot.len = rsPlot.srcs.length;
    rsPlot.cur = rsPlot.len - 1;
    $("#rs_plot img")
    .attr('src', rsPlot.srcs[rsPlot.cur])
    .removeClass('rs-plot-default');
    rsToggleArrow(rsPlot);
    rsPlotText(rsPlot);
});

// plots delete and clear
$(document).ready(function(){
    $(".rs_plot_footer")
    .on("click", '#rs-plot-trash', function(){
        Shiny.setInputValue("core_top-clear_src", rsPlot.srcs[rsPlot.cur]);
        rsPlot.srcs.splice(rsPlot.cur, 1);
        rsPlot.len = rsPlot.srcs.length;
        if(rsPlot.len > 0 && rsPlot.cur > 0){
            rsPlot.cur -= 1;
        } else {
            rsPlot.cur = 0;
        }
        if(rsPlot.len < 1){
            $("#rs_plot img")
            .attr('src', rsPlotDefault)
            .addClass('rs-plot-default');
        } else {
            $("#rs_plot img").attr('src', rsPlot.srcs[rsPlot.cur]);
        }
        rsPlotText(rsPlot);
        rsToggleArrow(rsPlot);
    })
    .on("click", '#rs-plot-clear', function(){
        Shiny.setInputValue("core_top-clear_src", rsPlot.srcs);
        $("#rs_plot img")
        .attr('src', rsPlotDefault)
        .addClass('rs-plot-default');
        rsPlot.len = 0;
        rsPlot.cur = 0;
        rsPlot.srcs = [];
        rsPlotText(rsPlot);
        rsToggleArrow(rsPlot);
    });
})







