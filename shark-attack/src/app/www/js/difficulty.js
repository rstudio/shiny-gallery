function setLevelClick() {
  var elementExists = !!document.getElementsByClassName("level-icon--easy")[0];
  if(elementExists) {
    addClickToLevel("easy");
    addClickToLevel("medium");
    addClickToLevel("hard");
  } else {
    setTimeout(setLevelClick, 200);
  }
}

function addClickToLevel(level) {
  document.getElementsByClassName("level-icon--" + level)[0].addEventListener("click", function(e) { setLevel(level) });
}

function setLevel(level) {
  Shiny.setInputValue("level", level);
}
