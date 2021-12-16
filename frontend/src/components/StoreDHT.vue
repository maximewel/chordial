<template>
   <v-sheet color="rgb(0, 0, 0, 0.2)" elevation="4" class="rounded-xl d-flex flex-column px-10 py-4 ma-3">
      
      <h2 class="headline secondary--text my-3">
         Save data
      </h2>

      <v-text-field v-model="key" color="accent" class="mx-5" label="Key"></v-text-field>
      <v-text-field v-model="value" color="accent" class="mx-5" label="Value"></v-text-field>

      <p v-show="display_error" class="warning--text body-2 my-5 text-center">
         {{error_message}}
      </p>

      <p v-show="display_message" class="success--text body-2 my-5 text-center">
         {{message}}
      </p>

      <v-btn color="accent" elevation="0" width=120 class="secondary--text align-self-center my-5" v-on:click="save">
         STORE
      </v-btn>
   </v-sheet>
</template>

<script>
import axios from 'axios';

export default {
   name: 'StoreDHT',
   data() {
      return {
         error_message: "Couldn't store this pair key value...",
         display_error: false,
         message: "Couldn't store this pair key value...",
         display_message: false,
      }
   },
   methods: {
      save() {
         const query = `?key=${this.key}&value=${this.value}`;
         axios.get(`http://localhost:2938/store${query}`)
            .then(response => {
               this.message = response.data;
               this.display_message = true;
               setTimeout(() => {this.display_error = false}, 3000);
            })
            .catch(reason => {
               this.display_message = false;
               this.error_message = reason;
               this.display_error = true;
               setTimeout(() => {this.display_error = false}, 3000);
            }
         );
      },
   }
}
</script>