// need to load  jQuery and bootstrap3
// default shiny has both, only library shiny is enough

// find tab hyperlink reference for tabpanel items
findTabHref = (panel_id, value) =>{
    var tab_href = $("#" + panel_id)
      .find("li:contains(" + value + ")")
      .find("[href]").attr("href");
    return tab_href;
};

// for shinydashboardPlus
// allow the main title in a menuItem object also has the ability to jump a tab
// replace old main menu jump with this new function
$(document).ready(() =>{
   $(".sidebar-menu").children(".treeview").children("a").attr({"data-toggle":"tab"});
});


// Enable a tag to change tab by clicking text with links
$(document).ready(function(){
  $('.tab-content').on('click', '.sps-tab-link', function(e){
    var link = $(this).attr('href');
    if (link.match('#')) {
      e.preventDefault();
      $('a[href="' + $(this).attr('href') + '"]').tab('show');
    }
  });
});

// Keep leftside menu subitems always expanded
$(document).ready(() =>{
  $(".treeview-menu").css("display", "block");
});



// change fileInput color to bs primary
// change text bar in file input local mode to read only
$(document).ready(() =>{
  $('.btn-file').removeClass('btn-default').addClass('btn-primary');
  $(".sps-file input").attr("readonly", true);
});


// disconnect
var spsDisconImg = "data:image/png;base64,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";

function showDiscon() {
    $('#ss-connect-dialog').show()
    .addClass("shiny-discon")
    .prepend('<span>If you think this is a bug, report to us on <a href="https://github.com/systemPipeR/systemPipeShiny/issues">Github</a></span>')
    .prepend(`<img src=${spsDisconImg} alt="">`);
    $('#ss-overlay').show();
      $('#ss-connect-dialog > p').after();
}

$(function() {
  $(document)
  .on('shiny:disconnected', function(event) {
    showDiscon();
  });
  let disconCheck;
  let num = 0;
  disconCheck = setInterval(function(){
      num += 1;
      if(num >= 3) {
        clearInterval(disconCheck);
        showDiscon();
        $("#ss-connect-dialog")
        .removeClass('shiny-discon')
        .addClass("shiny-discon-noserver");
      }
      if(typeof Shiny !== undefined){
        if(Shiny.shinyapp.$socket !== null) {
          clearInterval(disconCheck);
        }
      }
  }, 1000);
});


// notification and guide on click
$(function(){
    let notificationTrigger = false;
    $('li.notifications-menu:contains("Notifications") a.dropdown-toggle').click(function(){
        if (!notificationTrigger){
            $(this).find('i').removeClass('fa-warning').addClass('fa-check');
            $(this).find('span').text('0').removeClass('label-warning').addClass('label-success');
            notificationTrigger = true;
        }
    })
    let guideTrigger = false;
        $('li.messages-menu:contains("Choose a guide") a.dropdown-toggle').click(function(){
        if (!guideTrigger){
            $(this).find('span').text('0').removeClass('label-primary').addClass('label-success');
            guideTrigger = true;
        }
    })
})

// add target blank
$(()=>{
  $('.sps a:contains("{blk}")').each(function(){
    $(this)
      .text($(this).text().replace("{blk}", ""))
      .attr("target", "_blank");
  });
});
// remove default go top button
$(function(){
     $('.wrapper > div > i.fa.fa-chevron-up').parent().remove()
})

