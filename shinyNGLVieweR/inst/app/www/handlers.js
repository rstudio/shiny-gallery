$(document).ready(function() {
//Custom input handlers
$(document).on('click', '.example_link', function () {
Shiny.onInputChange('example_link_id', this.id);
});
$(document).on('click', '.selectionRemove', function () {
  Shiny.onInputChange('selectionRemove_id', this.id);
});
$(document).on('click', '.labelRemove', function () {
Shiny.onInputChange('labelRemove_id', this.id);
});
$(document).on('click', '.contactRemove', function () {
Shiny.onInputChange('contactRemove_id', this.id);
});
$(document).on('click', '.selectionLink', function () {
Shiny.onInputChange('selectionLink_id', this.id);
});
$(document).on('click', '.labelLink', function () {
Shiny.onInputChange('labelLink_id', this.id);
});
$(document).on('click', '.contactLink', function () {
Shiny.onInputChange('contactLink_id', this.id);
});
});

