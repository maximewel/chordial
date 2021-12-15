<template>
   <v-sheet color="rgb(0, 0, 0, 0.2)" elevation="4" class="rounded-xl d-flex flex-column px-10 py-4 ma-3">

      <h2 class="headline secondary--text my-3">
         Lookup
      </h2>

      <v-text-field color="accent" class="mx-5" label="Key"></v-text-field>

      <p v-show="display_error" class="warning--text body-2 my-5 text-center">{{error_message}}</p>

      <v-btn color="accent" elevation="0" width=120 class="secondary--text align-self-center my-5" v-on:click="save">
         SEARCH
      </v-btn>
      
      <v-text-field class="mt-2 mx-5" filled dense rounded readonly :value="result"></v-text-field>
   </v-sheet>
</template>

<script>
import axios from 'axios';

export default {
   name: 'LookupDHT',
   data() {
      return {
         result: "No result...",
         error_message: "Couldn't access to this key...",
         display_error: false,
      }
   },
   methods: {
      // TODO: axios request
      lookup() {
         axios.get('http://localhost:2938/')
            .then(response => (this.DHT = response.data))
      },
   }
}
</script>