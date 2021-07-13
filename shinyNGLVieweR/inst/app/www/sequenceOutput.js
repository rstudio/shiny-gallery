//Sequence aaContainer
const sequenceContainer = div
  .attr('id', 'sequenceContainer')
  .style('width', '100%')
  .style('display', 'flex')
  .style('font-family', 'courier')
  .style('flex-direction', 'row')
  .style('flex-wrap', 'wrap')
  .style('align-items', 'flex-start')
  .style('justify-content', 'start')
  .style('user-select', 'none'); // Disable text highlighting


//Tooltip AA

//Tooltip container
const tooltip = sequenceContainer
  .append('div')
  .style("opacity", 0)
  .attr("class", "tooltip")
  .style("position", "absolute")
  .style("font-size", "14px")
  .style("font-style", "courier")
  .style('z-index', 1)
  .style('color', '#fff')
  .style("pointer-events", "none")
  .style("line-height", "1")
  .style("background-color", "rgba(0, 0, 0, 0.8)")
  .style("border-radius", "2px")
  .style("padding", "5px")

const aaMouseover = function(d,i) {
  //get offest of parent div to body
  let offset = (document.getElementById(options.parentId).getBoundingClientRect())
  let mousePos = (document.getElementById(`aa-${d.Nr}`).getBoundingClientRect())
  let boxPos = (document.getElementById(options.parentId).getBoundingClientRect())

  sequenceContainer.selectAll(`#aa-${d.Nr}`)
    .style('font-weight', 'bold')
  tooltip
    .html(() => {
        return `${d.Nr}`
    })
    .style('opacity', 1)
    .style("left", () => {
        return `${mousePos.left - offset.left + (0*mousePos.width)}px`
    })
    .style("top", `${mousePos.top - offset.top + (-1.4*mousePos.height)}px`);
}

const aaMouseout = function(d,i) {
  sequenceContainer.selectAll(`#aa-${d.Nr}`)
   .style('font-weight', 'normal')

  tooltip
  .style('opacity', 0);
}

//data
//console.log(JSON.stringify(aaBlocks))
r2d3.onRender(function(data, div, width, height, options) {

  //Place onRender for data refresh
  let aaBlocks = d3
    .nest()
    .key(d => d.block)
    .entries(data);
    //data

 //remove all seqBlocks on reRender
  div.selectAll(".seqBlocks")
    .remove()

  const seqBlocks = sequenceContainer
    .selectAll('.seqBlocks')
    .data(aaBlocks)
    .enter()
    .append('div')
    .attr('id', (d) => `seqBlock-${d.key}`)
    .attr('class', 'seqBlocks')
    .style('display', 'flex')
    .style('margin-top', '10px')
    .style('flex-direction', 'column')
    .style('align-items', 'flex-end');

  const seqBlockNr = seqBlocks
    .append('div')
    .attr('id', (d) => `seqBlockNr-${d.key}`)
    .html((d) => {
      return d.key;
    })
    .attr('class', 'seqBlockNr');

  const seqContainer = seqBlocks
    .append('div')
    .attr('id', (d) => `seqContainer-${d.key}`)
    .attr('class', 'seqContainer')
    .style('display', 'flex')
    .style('flex-direction', 'row')
    .style('align-items', 'flex-start');

let aaHover = []
let aaSel = []
let aaSelStart = null
let aaSelStop = null
let isMouseDown = false
let aaNr = data
.map(d => d.Nr);

function aaRange(start, end) {
  const isReverse = (start > end);
  const targetLength = isReverse ? (start - end) + 1 : (end - start ) + 1;
  const arr = new Array(targetLength);
  const b = Array.apply(null, arr);
  const result = b.map((discard, n) => {
    return (isReverse) ? n + end : n + start;
  });

  return (isReverse) ? result.reverse() : result;
}

const aa = seqContainer
    .selectAll('.aa')
    .data(d => d.values)
    .enter()
    .append('div')
    .style('cursor', 'pointer')
    .attr('id', (d) => `aa-${d.Nr}`)
    .text((d) => {
      return d.AA;
    })
    .attr('class', 'aaContainer')
    .style('display', 'flex')
    .style('flex-direction', 'column')
    .style('color', (d, i) => {
    if(d.unique == true){
    return("black")
  } else {
    return("red")
  }
    })
    .on('mouseenter', (d, i) => {
      aaMouseover(d, i);
        if (isMouseDown) {
          aaSelStop = d.Nr
          aaRange(aaSelStart, aaSelStop).forEach(function(i) {
            sequenceContainer.select(`#aa-${i}`)
              .style('background-color', 'rgb(255,102,153)');
          })
        }
      // let aaBackground = sequenceContainer.select(`#aa-${d.Nr}`).style('background-color')
      //   if (aaBackground == 'rgba(0, 0, 0, 0)') {
      // sequenceContainer.select(`#aa-${d.Nr}`)
      //   .style('background-color', 'red');
      // } else {
      //   sequenceContainer.select(`#aa-${d.Nr}`)
      //     .style('background-color', 'rgba(0, 0, 0, 0)');
      // }
    })
    .on('mouseout', (d,i) => {
      aaMouseout(d, i);
      if (isMouseDown) {
        aaSelStop = d.Nr
        aaRange(aaSelStart, aaSelStop).forEach(function(i) {
          sequenceContainer.select(`#aa-${i}`)
            .style('background-color', 'rgba(0, 0, 0, 0)');
        })
      }
    })
   .on('mousedown', (d,i) => {
     isMouseDown = true
     aaSelStart = d.Nr
     aaSelStop = null
    })
    .on('mouseup', (d, i) => {
          isMouseDown = false
          sequenceContainer.selectAll('.aaContainer')
            .style('background-color', 'rgba(0, 0, 0, 0)');

          if (aaSelStop == null) {
            let excists = aaSel.includes(aaSelStart)
            if (excists == false) {
              aaSel.push(aaSelStart);
            } else {
              aaSel = aaSel.filter(function(value, index, arr) {
                return value !== aaSelStart
              });
            }
          } else {
            aaRange(aaSelStart, aaSelStop).forEach(function(i) {
              let excists = aaSel.includes(i)
              if (excists == false) {
                aaSel.push(i);
              } else {
                aaSel = aaSel.filter(function(value, index, arr) {
                  return value !== i
                });
              }
            });
          }
          setTimeout(function() {

            aaSel.forEach(function(i) {
              sequenceContainer.select(`#aa-${i}`)
                .style('background-color', 'rgb(51, 255, 25)');
            })
            aaSelStop = null
            aaSelStart = null
            aaHighlight(options.seqPositions);
          }, 10);
    })
    function aaHighlight(positions) {

    //Convert to array if input has only one position
    if (!Array.isArray(positions)) {
      positions = [positions]
    }

    if (positions != null) {
      for (var i = 0; i < positions.length; i++) {
        sequenceContainer.select(`#aa-${positions[i]}`)
          .style('background-color', 'rgb(51, 255, 25)');
      }
      // positions.forEach(function(i) {
      //   sequenceContainer.select(`#aa-${i}`)
      //     .style('background-color', 'rgb(51, 255, 25)');
      // })
      Shiny.setInputValue(
        "aa_clicked",
        aaSel, {
          priority: "event"
        }
      );
    }
  }

  aaHighlight(options.seqPositions);
  });
