<template>
   <v-sheet color="rgb(0, 0, 0, 0.2)" elevation="4" class="rounded-xl d-flex flex-column px-10 py-4 ma-3">

      <h2 class="headline secondary--text my-3">
         Lookup
      </h2>

      <v-text-field v-model="key" color="accent" class="mx-5" label="Key"></v-text-field>

      <p v-show="display_error" class="warning--text body-2 my-5 text-center">{{error_message}}</p>

      <v-btn color="accent" elevation="0" width=120 class="secondary--text align-self-center my-5" v-on:click="lookup">
         SEARCH
      </v-btn>

      <v-textarea class="mt-2 mx-5" filled dense rounded readonly auto-grow rows="1" :value="result"></v-textarea>
   </v-sheet>
</template>

<script>
import axios from 'axios';

export default {
   name: 'LookupDHT',
   data() {
      return {
         result: "No result...",
         error_message: "Couldn't store this pair key value...",
         display_error: false,
      }
   },
   methods: {
      lookup() {
         const query = `?key=${this.key}`;
         axios.get(`http://localhost:2938/lookup${query}`)
            .then(response => {
               const key = this.key;
               const values = response.data.values;

               if (values.length <= 0){
                  this.result = `${key} has no value on the node`;
               } else {
                  const nodeID = response.data.node.id;
                  const nodeName = response.data.node.node_name;

                  this.result = `${key} = ${values} from node ${nodeID} at ${nodeName}`;
               }
            })
            .catch(reason => {
               this.result = "";
               this.error_message = reason;
               this.display_error = true;
               setTimeout(() => {this.display_error = false}, 3000);
            }
         );
      },
   }
}
</script>