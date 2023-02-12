

$(function(){
    // dash resize on ui rendering
    $('body').on('user-displayed', ()=>{
        setTimeout(function() {
            $(window).trigger("resize");
            $('#page_user ul.sidebar-menu > li:first-of-type a').trigger('click')
        }, 1000);
    });
});


// display login box
$(function(){
    $("#user-toapp").click(function(){
        $(this).fadeOut();
        $("#user-login").fadeIn();
        setTimeout(function() {
             $(".img-container > img").css('transform', 'translateY(0)');
        }, 300);
        $('#user-bg').css('opacity', '0.3');
        // specail for biomatrix
        $('#user-bg main.matrix').css({'background': '#FFF', 'opacity': '0.1'});
        $('#user-theme-container').slideUp();
    })
    //watch enter key
    document.querySelector('.login-box').addEventListener('keypress', function (e) {
        if (e.key === 'Enter') {
          $('#user-login_click').trigger('click');
        }
    });
})

// display theme chooser
$(function(){
    $("#user-theme-container")
      .on('mouseenter', function(){
        $(this).find('.form-group').stop().slideDown('slow');
      })
      .on('mouseleave', function(){
        $(this).find('.form-group').stop().slideUp();
      })
})


// watch for loading bg change
$(function(){
  Shiny.addCustomMessageHandler('sps-loading-bg', function(data) {
      var loginBtn = $('#user-toapp');
      var themeChooser = $("#user-theme-container h4");
      switch(data.bg) {
        case "vhelix":
          whiteBg(loginBtn, themeChooser);
          break;
        case "hhelix":
          whiteBg(loginBtn, themeChooser);
          break;
        case "biomatrix":
          blackBg(loginBtn, themeChooser);
          break;
        case "empty":
          $('#user-toapp').trigger('click');
          break;
      }
  });
  function whiteBg(loginBtn, themeChooser) {
    loginBtn.css('color', '#1d89ff');
    themeChooser.css('color', 'black');
  }
  function blackBg(loginBtn, themeChooser) {
    loginBtn.css('color', '#FFF');
    themeChooser.css('color', '#FFF');
  }
})
