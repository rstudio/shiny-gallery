// Modified from https://github.com/phobetron/double-helix, credit to Charles Hudson
(function($) {
  $.fn.DoubleHelix = function(options) {
    var settings = {
      fps: 24,
      fgColor: "0,0,0",
      bgColor: "transparent"
    }

    if (options) { settings = $.extend(settings, options); }

    var fill_text = "ATCG"
    var fill_text_color = ["255, 179, 179", "255, 194, 102", "136, 191, 195", "179, 179, 255"]
    var fill_no = "10"
    var calculator = function(c, dim, t, y, n) {
      var x1 = 0, x2 = 0, z1 = 0, z2 = 0;
      c.font= "bold 15px Arial";
      var drawText = function(x, y, opacity) {
        c.fillStyle = "rgba(" + fill_text_color[n%4] +","+opacity+")";
        c.fillText(fill_text[n%4], x, y+15);
        c.fill();
      }

      var drawNo = function(x, y, opacity) {
        c.fillStyle = "rgba(51, 51, 0"+","+opacity+")";
        c.fillText(fill_no[n%2], x, y+15);
        c.fill();
      }

      var drawLine = function(y, x1, x2, z1, z2) {
        c.beginPath();
        c.moveTo(x1+8, y+10);
        c.lineTo(x2+8, y+13);
        var g = c.createLinearGradient(x1, y, x2, y);
        g.addColorStop(0, "rgba("+settings.fgColor+","+z1+")");
        g.addColorStop(1, "rgba("+settings.fgColor+","+z2+")");
        c.strokeStyle = g;
        c.stroke();
      }

      var fix = function(n) {
        return Math.round(n*10)/10;
      }

      return {
        calculate: function() {
          t+=2;
          x1 = Math.cos(t/360 * (Math.PI*2));
          x2 = Math.sin((t+270)/360 * (Math.PI*2));
          z1 = Math.cos((t+90)/360 * (Math.PI*2)) + 0.1;
          z2 = Math.sin((t+360)/360 * (Math.PI*2));
        },

        draw: function() {
          var _x1 = fix((x1*(dim.halfWidth-5))+dim.halfWidth-5);
          var _x2 = fix((x2*(dim.halfWidth-5))+dim.halfWidth-5);
          var _z1 = (z1+1)/2;
          var _z2 = (z2+1)/2;

          if ( _x1 != 45) { drawLine(y, _x1, _x2, _z1, _z2); }

          drawText(_x1, y, _z1);
          drawNo(_x2, y, _z2);
        }
      }
    }

    return this.each(function(i) {
      var c = this.getContext('2d');

      var dim = {
        width: c.canvas.width,
        height: c.canvas.height,
        halfHeight: c.canvas.height/2,
        halfWidth: c.canvas.width/2
      }
      var buffer = document.createElement('canvas');
      buffer.setAttribute('width', dim.width);
      buffer.setAttribute('height', dim.height);

      var cb = buffer.getContext('2d');

      var calculators = [];
      for (n = 0; n < dim.height/20; n++) {
        var calc = new calculator(cb, dim, n*20, n*20, n);
        calculators[n] = calc;
      }
      var now, delta;
      var then = Date.now();
      var interval = 1000/settings.fps;

      var draw = function() {
        requestAnimationFrame(draw);

        now = Date.now();
        delta = now - then;

        if (delta >= interval) {
          cb.clearRect(0, 0, dim.width, dim.height);
          cb.fillStyle = settings.bgColor;
          cb.fillRect(0, 0, dim.width, dim.height);
          $.each(calculators, function(i, calc) {
            calc.calculate();
            calc.draw();
          });

          c.clearRect(0, 0, dim.width, dim.height);
          c.drawImage(buffer, 0, 0);

          then = now - (delta % interval);
        }
      }
      draw();
    });
  }
})(jQuery);

  // listen to mouse move
$(function() {
  var canvas = $("<canvas />");
  canvas.attr({ height: $("#helix").height()});
  $("#helix").append(canvas);
  canvas.DoubleHelix({fps: 30, fgColor:'117,199,255'});

  var angle, angleRaw, x, y;
  var el = $('#helix');
  document.getElementById('particles-js').addEventListener("mousemove", function (e) {
    x = e.clientX - window.innerWidth / 2;
    y = window.innerHeight / 2 - e.clientY;
    angleRaw = Math.atan(x/ y);

    if (y < 0) {
        angle = Math.PI + angleRaw;
    } else if (x < 0 && y > 0) {
        angle = 2 * Math.PI + angleRaw;
    } else {
        angle = angleRaw;
    }
    el.css('transform', `rotate(${angle}rad)`);
  });
});
















