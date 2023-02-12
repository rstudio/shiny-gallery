var rna_tabs;
$(document).ready(function(){
  // disable unwanted tabs first
  rna_tabs =  $('#vs_rnaseq-main_panel li:not(:first-of-type)');

  rna_tabs.addClass("disabled");

  //disable click
  $('#vs_rnaseq-main_panel')
    .on('click', 'li.disabled', function(e) {
      e.preventDefault();
      return false;
    })
    .on('click', 'li:not(.disabled)', function(){
        $("html, body").animate({ scrollTop: 0 }, "slow");
    });
});

// enable tabs based on shiny singals
Shiny.addCustomMessageHandler("sps-rna-panel", function(data) {
  rna_tabs.addClass("disabled");
  let index = data.index.map((num)=>{return num-1;});
  rna_tabs.filter(function(i) {
    return index.indexOf(i) > -1;
  }).removeClass("disabled");
});


$('#vs_rnaseq-normal-main_panel').on('resize', function(){
    $('#vs_rnaseq-normal-res_status').height($(this).height());
});

// watch normal tab left panel height change so change height of right panel
$(function() {
  var rb = new ResizeObserver(entries => {
    $('#vs_rnaseq-normal-res_status').height(entries[0].contentRect.height);
  });
  rb.observe($('#vs_rnaseq-normal-main_panel').get(0));
});


// change normalize data download panel text color
Shiny.addCustomMessageHandler("rnaseq-down-text", function(data){
  el = $(`#${data.id}`).next();
  if(data.state === true){
      el.children("p")
        .removeClass('text-gray')
        .children("span")
        .removeClass('text-danger')
        .addClass('text-success')
        .text("Ready");
  } else {
      el.children("p")
        .addClass('text-gray')
        .children("span")
        .addClass('text-danger')
        .removeClass('text-success')
        .text("Not ready");
  }
});


// gallery click to change bt3 tab set
$(function(){
  $('#shiny-tab-vs_rnaseq').on('click', '.sps-gallery a', function(event){
    let href = this.getAttribute("href");
    if(!href.startsWith('#') && !href.startsWith('http')){
      event.preventDefault();
      $('#vs_rnaseq-main_panel a[data-value=' + href + ']').click();
    }
  });
});

// watch DEG tab to match the height of left to right panel
$(function(){
  var rb = new ResizeObserver(entries => {
    $('#vs_rnaseq-deg-summary-left').height(entries[0].contentRect.height);
  });
  rb.observe($('#vs_rnaseq-deg-summary-right').get(0));
});
