$(function(){
  let max_particles    = 500;
  let particles        = [];
  let frequency        = 20;
  let init_num         = max_particles;
  let max_time         = frequency*max_particles;
  let time_to_recreate = false;
  var ele_id = "#admin-bg";
  var ele = $(ele_id);
  const data           = createCanvas()
  let tela             = data[0];
  let canvas           = data[1];


  // Enable repopolate
  setTimeout(function(){
    time_to_recreate = true;
  }.bind(this), max_time)

  // Popolate particles
  popolate(max_particles);

  class FishEgg{
    constructor(canvas){
      const random = Math.random()
      this.progress = 0;
      this.canvas = canvas;
      // Set position
      this.x = ($(ele).width()/2)  + (Math.random()*300 - Math.random()*300)
      this.y = ($(ele).height()/2) + (Math.random()*$(ele).height()/4 - Math.random()*$(ele).height()/4)
      // Get viewport size
      this.w = $(ele).width()
      this.h = $(ele).height()

      // Dimension
      this.radius = 12 + Math.random()*6
      // Color
      this.color  = "rgba(255,255,255,1)"
      // Setting
      this.fish_egg = {
        offset1: Math.random() > 0.5 ? 0.5 + Math.random()*3 :  0.5 + Math.random()*-3,
        offset2: Math.random() > 0.5 ? 0.5 + Math.random()*3 :  0.5 + Math.random()*-3,
        offset3: Math.random() > 0.5 ? 0.5 + Math.random()*3 :  0.5 + Math.random()*-3,
        radius1: 0.5 + Math.random()*5,
        radius2: 0.5 + Math.random()*5,
        radius3: 0.5 + Math.random()*5
      }
      this.variantx1 = Math.random()*100
      this.variantx2 = Math.random()*100
      this.varianty1 = Math.random()*100
      this.varianty2 = Math.random()*100
    }

    createCircle(x,y,r,c){
      this.canvas.beginPath();
      this.canvas.fillStyle = c;
      this.canvas.arc(x, y, r, 0, Math.PI*2, false);
      this.canvas.fill();
      this.canvas.closePath();
    }

    createEyes(){
      this.createCircle(this.x+this.fish_egg.offset2, this.y+this.fish_egg.offset2, this.fish_egg.radius2 + 4  , "rgba(241, 242, 244, 0.06)")
      this.createCircle(this.x+this.fish_egg.offset3, this.y+this.fish_egg.offset3, this.fish_egg.radius3 + 2  , "rgba(255, 204, 67, 0.08)")
      this.createCircle(this.x+(Math.random(this.progress/350)*this.fish_egg.offset1), this.y+(Math.random  (this.progress/350)*this.fish_egg.offset1), this.fish_egg.radius1, "rgba(152, 19, 4, 0.19)")
    }

    render(){
      // Create inside parts
      this.createEyes()

      this.canvas.beginPath();
      let c   = '130, 151, 180';
      let rad = this.canvas.createRadialGradient(this.x, this.y, this.radius, this.x, this.y, 1);
      rad.addColorStop(0, 'rgba('+c+',0.09)');
      rad.addColorStop(0.9, 'rgba('+c+',0)');
      this.canvas.lineWidth = Math.random()*2.2;
      this.canvas.fillStyle = rad;
      this.canvas.arc(this.x, this.y, this.radius, 0, Math.PI*2, false);
      this.canvas.fill();
      this.canvas.strokeStyle = "rgba(255, 255, 217, 0.05)";
      this.canvas.stroke();
      this.canvas.closePath();
    }

    move(){
      this.x += (Math.sin(this.progress/this.variantx1)*Math.cos(this.progress/this.variantx2))/8;
      this.y += (Math.sin(this.progress/this.varianty1)*Math.cos(this.progress/this.varianty2))/8;



      if(this.x < 0 || this.x > this.w - this.radius){
        return false
      }

      if(this.y < 0 || this.y > this.h - this.radius){
        return false
      }
      this.render()
      this.progress++
      return true
    }
  }

  class FishLarva{
    constructor(canvas, progress){
      const random = Math.random()
      this.progress = 0;
      this.canvas = canvas;
      this.speed = 0.5 + random*1.3

      this.x = ($(ele).width()/2)  + (Math.random()*200 - Math.random()*200)
      this.y = ($(ele).height()/2) + (Math.random()*200 - Math.random()*200)

      this.s = 0.8 + Math.random() * 0.6;
      this.a = 0

      this.w = $(ele).width()
      this.h = $(ele).height()
      this.radius = random*1.3
      this.color  = "#f69a34"

      this.variantx1 = Math.random()*1000
      this.variantx2 = Math.random()*1000
      this.varianty1 = Math.random()*1000
      this.varianty2 = Math.random()*1000
    }

    render(){
      this.canvas.beginPath();
      this.canvas.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
      this.canvas.lineWidth = 2;
      this.canvas.fillStyle = this.color;
      this.canvas.fill();
      this.canvas.closePath();
    }

    move(){
      // this.x += (Math.sin(this.progress/this.variantx1)*Math.cos(this.progress/this.variantx2))/this.speed  ;
      // this.y += (Math.sin(this.progress/this.varianty1)*Math.cos(this.progress/this.varianty2))/this.speed  ;
      this.x += Math.cos(this.a) * this.s;
      this.y += Math.sin(this.a) * this.s;
      this.a += Math.random() * 0.8 - 0.4;
      if(this.x < 0 || this.x > this.w - this.radius){
        return false
      }

      if(this.y < 0 || this.y > this.h - this.radius){
        return false
      }
      this.render()
      this.progress++
      return true
    }
  }


  class FishLarvaEgg{
    constructor(canvas, progress){
      const random = Math.random()
      this.progress = 0;
      this.canvas = canvas;
      this.speed = 0.5 + random*0.2

      this.x = ($(ele).width()/2)  + (Math.random()*200 - Math.random()*200)
      this.y = ($(ele).height()/2) + (Math.random()*200 - Math.random()*200)

      this.s = Math.random() * 1;
      this.a = 0

      this.w = $(ele).width()
      this.h = $(ele).height()
      this.radius = random*0.8
      this.color  = random > 0.8 ? "#82a0c4" : "#2E4765"

      this.variantx1 = Math.random()*100
      this.variantx2 = Math.random()*100
      this.varianty1 = Math.random()*100
      this.varianty2 = Math.random()*100
    }

    render(){
      this.canvas.beginPath();
      this.canvas.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
      this.canvas.lineWidth = 2;
      this.canvas.fillStyle = this.color;
      this.canvas.fill();
      this.canvas.closePath();
    }

    move(){
      this.x += Math.cos(this.a) * this.s;
      this.y += Math.sin(this.a) * this.s;
      this.a += Math.random() * 0.8 - 0.4;

      if(this.x < 0 || this.x > this.w - this.radius){
        return false
      }

      if(this.y < 0 || this.y > this.h - this.radius){
        return false
      }
      this.render()
      this.progress++
      return true
    }
  }

  class Paramecium{
    constructor(canvas){
      const random = Math.random()
      this.progress = 0;
      this.canvas = canvas;
      // Set position
      this.x = ($(ele).width()/2)  + (Math.random()*300 - Math.random()*300)
      this.y = ($(ele).height()/2) + (Math.random()*$(ele).height()/4 - Math.random()*$(ele).height()/4)
      // Get viewport size
      this.w = $(ele).width()
      this.h = $(ele).height()
      this.rotation = (random*180)*Math.PI/180
      // Dimension
      this.radius = 12 + Math.random()*6
      // Color
      this.color  = "rgba(255,255,255,1)"
      // Setting
      this.variantx1 = Math.random()*100
      this.variantx2 = Math.random()*100
      this.varianty1 = Math.random()*100
      this.varianty2 = Math.random()*100
    }

    createOval(x, y, w, h) {
      var kappa = .5522848,
          ox = (w / 2) * kappa, // control point offset horizontal
          oy = (h / 2) * kappa, // control point offset vertical
          xe = x + w,           // x-end
          ye = y + h,           // y-end
          xm = x + w / 2,       // x-middle
          ym = y + h / 2;       // y-middle

      this.canvas.save();

      this.canvas.translate(this.w/2, this.h/2);

      // Rotate 1 degree
      this.canvas.rotate(this.rotation);

      // Move registration point back to the top left corner of canvas
      this.canvas.translate(-this.w/2, -this.h/2);

      this.canvas.beginPath();
      this.canvas.moveTo(x, ym);
      this.canvas.quadraticCurveTo(x,y,xm,y);
      this.canvas.quadraticCurveTo(xe,y,xe,ym);
      this.canvas.quadraticCurveTo(xe,ye,xm,ye);
      this.canvas.quadraticCurveTo(x,ye,x,ym);

      this.canvas.strokeStyle = 1;
      this.canvas.fillStyle = "rgba(255,255,255,0.01)";
      this.canvas.fill();
      this.canvas.stroke();
      this.canvas.restore();
    }

    render(){
      // Create inside parts
      this.createOval(this.x, this.y, 12, 4)
    }

    move(){
      this.x += (Math.sin(this.progress/this.variantx1)*Math.cos(this.progress/this.variantx2))/4;
      this.y += (Math.sin(this.progress/this.varianty1)*Math.cos(this.progress/this.varianty2))/4;

      if(this.x < 0 || this.x > this.w - this.radius){
        return false
      }

      if(this.y < 0 || this.y > this.h - this.radius){
        return false
      }
      this.render()
      this.progress++
      return true
    }
  }


  /*
   * Function to create canvas
   */
  function createCanvas(){
    let tela = document.createElement('canvas');
        tela.width = $(document).width();
        tela.height = $(document).height();
        ele.append(tela);
    let canvas = tela.getContext('2d');
    return [tela, canvas]
  }

  /*
   * Function to clear layer canvas
   * @num:number number of particles
   */
  function popolate(num){
    for (var i = 0; i < num; i++) {
      setTimeout(
        function(x){
          return function(){
            let random = Math.random()
            // ------------------------------------
            // Set type of planktom
            let type = new FishLarva(canvas)
            if(!time_to_recreate){
              if(random > .97) type = new FishEgg(canvas);
              if(random < .1 && random > 0) type  = new Paramecium(canvas)
            }
            if(random > .1 && random < .8) type  = new FishLarvaEgg(canvas)

            // if(random < .1) this.type  = "bryozoan"
            // ------------------------------------
            // Add particle
            particles.push(type)
          };
        }(i)
        ,frequency*i);
    }
    return particles.length
  }

  /*
   * Function to clear layer canvas
   */
  function clear(){
    let grd = canvas.createRadialGradient(tela.width/2, tela.height/2, 0, tela.width/2, tela.height/2, tela  .width);
        grd.addColorStop(0,"rgba(25,25,54,0.12)");
        grd.addColorStop(1,"rgba(0,0,20,0.01)");
    // Fill with gradient
    canvas.fillStyle=grd;
    canvas.fillRect(0, 0, tela.width, tela.height);
  }

  /*
   * Function to update particles in canvas
   */
  function update(){
    clear();
    particles = particles.filter(function(p) { return p.move() })
    // Recreate particles
    if(time_to_recreate){
      if(particles.length < init_num){ popolate(1) }
    }
    requestAnimationFrame(update.bind(this))
  }
  // Update canvas
  update()
});
