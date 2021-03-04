$( document ).ready(function() {
  $(document).on('click','#toggle-network-filters',function(){
    $("#network-filters-cell").toggleClass("open");

    let button_text = $("#network-filters-cell").hasClass("open")
      ? "Close Filters"
      : "Open Filters"

    $("#toggle-network-filters").html(button_text);
  });
});
