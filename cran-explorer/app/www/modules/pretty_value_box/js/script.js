$(document).ready(function() {
  $('.inlinebar').sparkline('html', {
    type: 'bar',
    height: '36px',
    width: '100%', 
    barSpacing: 2, 
    negBarColor: '#8A3E0B', 
    barColor: '#424956', 
    barWidth: 18
  });
});