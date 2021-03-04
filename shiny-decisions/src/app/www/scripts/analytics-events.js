let sendAnalyticsEvent = function(options) {
  gtag('event', options.action, {'event_category' : options.category,
    'event_label' : options.label});
}
Shiny.addCustomMessageHandler('trackEvent', sendAnalyticsEvent)
