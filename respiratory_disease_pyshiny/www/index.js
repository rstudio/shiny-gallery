$(() => {
  Shiny.addCustomMessageHandler("toggleActiveTab", (payload) => {
    $(".page-main").toggleClass("main-visible");
  });
});
