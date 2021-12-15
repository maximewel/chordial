<template>
   <div class="d-flex justify-center" >
      <CanvasDHT />
      <div class="d-flex flex-column justify-space-between">
         <StoreDHT />
         <LookupDHT />
      </div>
   </div>
</template>

<script>
import CanvasDHT from '@/components/CanvasDHT.vue';
import LookupDHT from '@/components/LookupDHT.vue'
import StoreDHT from '@/components/StoreDHT.vue'

export default {
   name: 'Home',
   components: {
      CanvasDHT,
      LookupDHT,
      StoreDHT,
   },
}
</script>