

$(function(){
    // dash resize on ui rendering
    $('body').on('admin-displayed', (e)=>{
      setTimeout(function() {
        $('#page_admin ul.sidebar-menu > li:first-of-type a').trigger('click')
        $(window).trigger("resize");
        // remove default go top button
        $('.wrapper i.fa.fa-chevron-up').parent().remove();
      }, 1000);
    });
});

//watch enter key
$(function(){
    document.querySelector('#admin-login .login-box').addEventListener('keypress', function (e) {
        if (e.key === 'Enter') {
          $('#admin-login_click').trigger('click');
        }
    });
})

// remove default go top button
$(function(){
    $('.wrapper i.fa.fa-chevron-up').parent().remove()
})
