function updateTutorial(step, text) {
  $(".start-element--tutorial .image > img").attr("src", "../assets/tutorial" + step + ".png");
  $(".start-element--tutorial .text > h4").text(text);
}
