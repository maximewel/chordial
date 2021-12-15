<template>
   <v-sheet color="rgb(0, 0, 0, 0.2)" elevation="4" class="rounded-xl d-flex flex-column px-10 py-4 ma-3">
      
      <h2 class="headline secondary--text my-3">
         Save data
      </h2>

      <v-text-field color="accent" class="mx-5" label="Key"></v-text-field>
      <v-text-field color="accent" class="mx-5" label="Value"></v-text-field>

      <p v-show="display_error" class="warning--text body-2 my-5 text-center">
         {{error_message}}
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
      }
   },
   methods: {
      save() {
         // TODO: axios request
         axios.get('http://localhost:2938/')
            .then(response => (this.DHT = response.data))
      },
   }
}
</script>