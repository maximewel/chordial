<template>
  <v-app>   
    <TheHeader />
    <v-main>
      <Home />
    </v-main>
  </v-app>
</template>

<script>
import Home from "./views/Home.vue";
import TheHeader from "./components/TheHeader.vue"

export default {
  name: 'App',
  components: {
    Home,
    TheHeader,
  }
};
</script>
<style>

#app {
  background-image: linear-gradient(to bottom right, #423A61, #2C3756);
}

</style>