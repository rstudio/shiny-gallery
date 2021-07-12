function runCountdown(time_in_sec) {
  var time_past = 0;
  $(".diver, .shark, .boat, .organic, .glass, .metal, .plastic, .electro, .radio").addClass("glow");
  $(".countdown").text(time_in_sec);
  var countdown_interval = setInterval(function() {
    time_past++;
    $(".countdown").text(time_in_sec - time_past);
    if(time_past == time_in_sec) {
      $(".countdown").text("CLEAN!");
    }
    if(time_past > time_in_sec) {
      clearInterval(countdown_interval);
      $(".countdown").text("");
      $(".diver, .shark, .boat, .organic, .glass, .metal, .plastic, .electro, .radio").removeClass("glow");
    }
  }, 1000);
}
