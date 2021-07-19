// modified from: https://codepen.io/gskinner/pen/mmbpjv
// configurable:
var dotSize = 56, blurLevels=9, speed=0.5, particleScale=0.33;

document.getElementById('login_canvas').addEventListener("mousemove", function (e) {
    speed = e.clientX / window.innerWidth + 0.01
})
// globals:
var c = createjs, stage, t=0, count=0, w, h, max, min;
var spriteSheet, helixes=[];

var texts = [
    new c.Text("A", "bold 120px Arial", "#f79b68"),
    new c.Text("T", "bold 120px Arial", "#a0d28d"),
    new c.Text("C", "bold 120px Arial", "#8581d5"),
    new c.Text("G", "bold 120px Arial", "#5da7d6")
]


/* Objects */
function HelixParticle(spriteSheet) {
	this.Sprite_constructor(spriteSheet);
	this.t = 0;
	this.speed = 1;
	this.size = 1;
	this.altAmp = 1;
	this.altPer = 1;
}
c.extend(HelixParticle, c.Sprite);
c.promote(HelixParticle, "Sprite");


function Helix(particleCount) {
	this.Container_constructor();
	this.particleCount = particleCount||1000;
	this.set({});
	this.particles = [];
	this.createParticles();
}
var p = c.extend(Helix, c.Container);
p.set = function(o) {
	this.overscan = o.overscan==null?0.2:o.overscan;
	this.particleScale = o.particleScale||1;
	this.speed = o.speed||1;
	this.amplitude = o.amplitude==null?0.5:o.amplitude;
	this.altAmplitude = o.altAmplitude==null?0.5:o.altAmplitude;
	this.startRotation = o.startRotation||0;
	this.rotations = o.rotations==null?2:o.rotations;
}
p.createParticles = function() {
	var dots = this.particles, l=this.particleCount;
	while (l-- > 0) {
		var seed = rnd(1);
		dot = new HelixParticle(spriteSheet[Math.floor(Math.random() * 4)]);
		dot.t = rnd(Math.PI);
		dot.speed = Math.pow(seed*0.5+0.5,3);
		dot.size = 1-dot.speed;
		dot.altAmp = rnd(0.1,0.6)*rnd(0,dot.speed)*(rnd(1)<0.5?-1:1);
		dot.altPer = rnd(0.3,2);
		dot.altStart = rnd(Math.PI*2);
		dot.gotoAndStop(seed*blurLevels|0);
		dots.push(dot);
		this.addChild(dot);
	}
}
p.tick = function(delta) {
	var fov = min, dots = this.particles, a0=this.amplitude*0.5, a1=this.altAmplitude*0.5, pScale=this.particleScale*particleScale;
	var rotations = this.rotations*Math.PI*2, startRotation=this.startRotation*Math.PI*2;
	var adjW = w*(1+this.overscan*2);
	for (var i=0, l=dots.length; i<l; i++) {
		var dot = dots[i], altPer=dot.altPer*Math.PI*2;
		var t = (dot.t += delta*0.0001*this.speed*speed*dot.speed)%1;

		// base helix shape:
		if (t < 0) { t = 1+t; }
		var x = t*adjW-adjW/2;

		t = x/adjW;
		var y = Math.sin(t*rotations+startRotation)*min*a0;
		var z = Math.cos(t*rotations+startRotation)*min*a0;

		// introduce variation:
		y += Math.sin(t*altPer+dot.altStart)*min*dot.altAmp*a1;
		z += Math.cos(t*altPer+dot.altStart)*min*dot.altAmp*a1;

		var s = fov/(z+fov);
		dot.x = x*s; // disable perspective on the particle positions
		dot.y = y*s;
		dot.scaleX = dot.scaleY = Math.pow(s*(1+dot.size),2)*pScale;
		dot.alpha = s-0.6;
	}
}
p.clone = function(particleCount) {
	var o = new Helix(particleCount||this.particleCount);
	this._cloneProps(o);
	o.set(this);
	return o;
}
c.promote(Helix, "Container");


/* global methods */
setup();
function setup() {
	stage = new c.StageGL("login_canvas");
	stage.tickChildren = false;
	stage.setClearColor("#FFF");

	window.addEventListener("resize", onResize);
	onResize();

	spriteSheet = [
	    generateSpriteSheet(0),
	    generateSpriteSheet(1),
	    generateSpriteSheet(2),
	    generateSpriteSheet(3),
	];

	var helix;

	helix = stage.addChild(new Helix(500));
	helix.x = w/2;
	helix.y = h/2;
	helix.amplitude = 0.4;
	helix.particleScale = 0.2;
	helix.rotation = -40;
	helix.rotations = 2.5;
	helix.speed = 2;
	helix.startRotation = 0.33;
	helixes.push(helix);

	helix = stage.addChild(helix.clone());
	helix.startRotation = 0.83;
	helixes.push(helix);

	c.Ticker.timingMode = c.Ticker.RAF;
	c.Ticker.on("tick", tick);
}


function generateSpriteSheet(dna_index) {
	var holder = new c.Container(), shape = holder.addChild(new c.Shape()), g=shape.graphics;
	var pow = Math.ceil(Math.log(dotSize*2.2)/Math.log(2)), size2 = Math.pow(2,pow);
	var rect = new c.Rectangle(-size2/2, -size2/2, size2, size2);
	var builder = new c.SpriteSheetBuilder();

	var text = texts[dna_index];
    text.x = size2/4;
	text.y = size2/4;
	text.textAlign = "center";
    text.textBaseline = "middle";
	holder.addChild(text);

	builder.padding = 0;
	builder.maxWidth = Math.ceil(Math.sqrt(blurLevels))*size2;
    builder.addFrame(holder, rect, 1, prepFrame, 1);
	return builder.build();
}

function prepFrame(holder, i, size2) {
	holder.getChildAt(0).graphics.c()
}

function tick(evt) {
	var d = evt.delta;
	for (var i=0,l=helixes.length; i<l; i++) { helixes[i].tick(d); }
	stage.update();
}

function rnd(min, max) {
	if (max === undefined) { max=min; min=0; }
	return Math.random()*(max-min)+min;
}

function onResize() {
	w = window.innerWidth
	h = window.innerHeight;
	max = Math.max(w,h);
	min = Math.min(w,h);
	login_canvas.width = w;
	login_canvas.height = h;
	stage.updateViewport(w,h);
	for (var i=0; i<helixes.length; i++) {
		helixes[i].x = w/2;
		helixes[i].y = h/2;
	}
	particleScale = min/1000*0.3;
	stage.update();
}
