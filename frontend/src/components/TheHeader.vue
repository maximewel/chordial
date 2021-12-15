<template>
  <div class="px-16 py-6 d-flex align-center justify-space-between">
     <div class="d-flex align-center">
         <span class="logo"></span>
         <h1 class="display-1 secondary--text ml-5">
            Chordial.
         </h1>
     </div>
     
      <v-btn fab elevation="0" color="transparent" href="https://github.com/maximewel/chordial">
         <v-icon x-large>
            mdi-github
         </v-icon>
      </v-btn>
  </div>
</template>

<script>
export default {

}
</script>

<style>

.logo {
   display: block;
   position: relative;
   height: 40px;
   width: 40px;
   background-color: white;
   border-radius: 50%;
}

.logo::after {
   content: "";
   position: absolute;
   display: block;
   left: -10px;
   top: -4px;
   width: 48px;
   height: 48px;
   border-radius: 50%;
   box-shadow: -12px 0 0 0 #F35959;
}

</style>