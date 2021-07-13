var shinyBinding = new Shiny.InputBinding();
$.extend(shinyBinding, {
  
  // find the dom element with input$id
  // this becomes el downstream
  find: function find(scope) {
    return $(scope).find(".my_div_too")
  },
  
  // get the data-anatomy of the element with class selected
  // use this as the input's value
  // SEE subscribe
  getValue: function getValue(el) {
    var value = $(el).find('.selected').data('subcomponent')
    console.log(value)
    return value
  },

  // on click, remove any previous selected classes
  // then add the selected class to the clicked limb
  // this is used in getValue
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