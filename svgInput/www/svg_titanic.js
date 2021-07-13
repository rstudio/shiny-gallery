var shinyBinding = new Shiny.InputBinding();
$.extend(shinyBinding, {
  
  find: function find(scope) {
    return $(scope).find(".svg_titanic")
  },
  
  // get the data-svginput of the element with class selected as shiny input
  getValue: function getValue(el) {
    var value = $(el).find('.selected').data('shinyclick')
    console.log(value)
    return value
  },

  subscribe: function(el, callback) {
    $(el).on("click.shinyBinding", function(evt) {
      // remove all of the selected classes inside our element
      $(el).find(".selected").removeClass("selected");
      // set the selected class to the closest clicked part
      //console.log($(evt.target).attr('id'))
      $(evt.target).addClass('selected');
      callback();
    })
  },
  unsubscribe: function(el) {
    $(el).off(".shinyBinding");
  }
});

Shiny.inputBindings.register(shinyBinding);
