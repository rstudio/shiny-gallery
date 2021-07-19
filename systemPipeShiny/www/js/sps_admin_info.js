$(function(){
    Shiny.setInputValue("admin-left_sidebar", 'admin-info', {priority: "event"});
    $('section.sidebar li:not(.treeview)').on("click", function() {
        Shiny.setInputValue(
            "admin-left_sidebar",
            $(this).find('a').attr('data-value'),
            {priority: "event"}
        );
    })
})

