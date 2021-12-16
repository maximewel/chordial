<template>
   <v-sheet color="rgb(0, 0, 0, 0.2)" elevation="4" class="rounded-xl d-flex flex-column px-10 py-4 ma-3">
      <h2 class="headline secondary--text my-3">
         DHT
      </h2>

      <div class="d-flex">
         <canvas id="dht-canvas" width="700" height="700" v-on:click="onCanvasClick"></canvas> 

         <div class="d-flex flex-column justify-space-between" >
            <h3 class="headline text-center secondary--text">
               {{currentNode.name}}
            </h3>
            <h4 class="body-2 secondary--text mt-3 mb-6">
               NodeID: {{currentNode.id}}
            </h4>

            <div class="my-1">
               <h4 class="body-2 secondary--text">
                  Predecessor: {{currentNode.pred}}
               </h4>
               <h4 class="body-2 secondary--text">
                  Successor: {{currentNode.succ}}
               </h4>
            </div>

            <v-data-table
               :headers="headers"
               :items="arrays"
               :items-per-page="5"
               height="290"
               class="elevation-0 transparent"        
               :footer-props="{
                  disableItemsPerPage: true,
                  itemsPerPageOptions: [5],
                  itemsPerPageText: '',
               }"
            ></v-data-table>

            <p class="display-1 text-center mt-5 mb-0">{{ time }}</p>

            <v-btn color="accent" elevation="0" width=120 class="secondary--text align-self-center my-5" v-on:click="dt=0">
               REFRESH
            </v-btn>
         </div>
      </div>
      
   </v-sheet>
</template>

<script>
import axios from 'axios';

export default {
   name: "CanvasDHT",
   data() {
      return {
         currentNode: {
            name: "Node_1@Desktop",
            id: "6f411c4c84994e4065ddcd969d6b73af0fa0b4bf"
         },
         headers: [
            {text: 'Key', align: 'start', value: 'key'},
            {text: 'Value', value: 'value', sortable: false},
         ],
         arrays: [
            { key: 123, value: "oyo"},
            { key: 1, value: "oysado"},
            { key: 42, value: "oao"},
            { key: 93, value: "ah"},
            { key: 93, value: "ah"},
            { key: 33, value: "fafs"},
            { key: 53, value: "323"},
         ],

         dt: 0,
         timeInit: 0,
         time: "00:00",

         ring: {
            r: 220,
            color: "#616266",
         },
         node: {
            r: 18,
            color: "#717171",
         },
         dht: [
         ],
      }
   },
   computed: {
      max: function() {
         let max = "";
         for (let i = 0; i < 40; ++i)
            max += "f";
         return parseInt(max, 16);
      }
   },
   mounted () {
      this.$nextTick(function () {
         this.updateDHT();
         this.displayDHT();

         // Timer
         let _this = this;
         this.resetTimer();

         setInterval(function() {
            let now = new Date().getTime();
            _this.dt = _this.timeInit - now;
         }, 50);
      })
   },
   methods: {
      resetTimer() {
         this.timeInit = new Date();
         this.timeInit.setTime(this.timeInit.getTime() + 1000 * 10);
      },
      updateDHT() {
         axios.get('http://localhost:2938/state')
            .then(response => {
               this.dht = response.data.nodes;
            })
      },
      displayDHT() {
         let canvas = document.getElementById("dht-canvas");
         let ctx = canvas.getContext("2d");
         ctx.clearRect(0, 0, canvas.width, canvas.height);
         let c = { x: canvas.width / 2, y: canvas.height / 2 };


         // Ring
         ctx.strokeStyle = this.ring.color;
         ctx.lineWidth = 6;
         ctx.beginPath();
         ctx.arc(c.x, c.y, this.ring.r, 0, 2 * Math.PI);
         ctx.stroke();

         // Nodes
         for (let node of this.dht) {
            let v = { x: 0, y: -this.ring.r };
            let a = (parseInt(node.id, 16) / this.max) * 2 * Math.PI;
            let x = Math.cos(a) * v.x - Math.sin(a) * v.y + c.x;
            let y = Math.sin(a) * v.x + Math.cos(a) * v.y + c.y;

            console.log(node);

            node.x = x;
            node.y = y;
 
            ctx.fillStyle = this.node.color;
            ctx.beginPath();
            ctx.arc(x, y, this.node.r, 0, 2 * Math.PI);
            ctx.fill();
            
            ctx.strokeStyle = "#d0d0d0";
            ctx.lineWidth = 4;
            ctx.beginPath();
            ctx.arc(x, y, this.node.r, 0, 2 * Math.PI);
            ctx.stroke();

            ctx.fillStyle = "black";
            ctx.textBaseline = 'middle';
            ctx.textAlign = "center"; 
            ctx.font = "30px Arial";
            ctx.fillText(node.storage.length, x, y);

         }
      },
      displayDHTValues(nodeToDisplay){
         console.log(nodeToDisplay.fingers);
         const fingers = nodeToDisplay.fingers;
         this.currentNode = {
            name: nodeToDisplay.node_name,
            id: nodeToDisplay.id,
            pred: `${fingers.pred.id} at ${fingers.pred.node_name}`,
            succ: `${fingers.succ.id} at ${fingers.succ.node_name}`,
         },
         this.arrays = nodeToDisplay.storage;
      },
      onCanvasClick(e) {
         let canvas = document.getElementById("dht-canvas");
         let cRect = canvas.getBoundingClientRect();
         let x = ((e.clientX - cRect.left) / cRect.width) * canvas.width;
         let y = ((e.clientY - cRect.top) / cRect.height) * canvas.height;

         for (let node of this.dht) {
            let dx = node.x - x;
            let dy = node.y - y;
            let distance = Math.sqrt(dx*dx + dy*dy);

            if(distance <= this.node.r){
               this.displayDHTValues(node);
            }
         }
      }
   },
   watch: {
      dht: function() {
         this.displayDHT();
      },
      dt : function() {
         let pad = (num) => {
            let s = "0" + num;
            return s.substr(s.length - 2);
         }

         if (this.dt <= 0) {
            this.updateDHT();
            this.resetTimer();
         }
         else {
            let minutes = Math.floor((this.dt % (1000 * 3600)) / (1000 * 60));
            let seconds = Math.floor((this.dt % (1000 * 60)) / 1000);
            this.time = pad(minutes) + ":" + pad(seconds);
         }
      }
   }
}
</script>

<style scoped >

canvas {
   margin: auto;
   height: 500px;
}

.v-data-table >>> td {
    font-size: 0.8rem;
    background-color: transparent;
}

</style>