<template>
    <p>
      Init : <span>{{ message }}</span>
      Init2 : <span>{{ message2 }}</span>
    </p>
</template>

<script>
import axios from 'axios';

export default {
    name: 'Mycomp',
    data() {
        return {
            message: "VIDE",
            message2: "VIDE",
        }
    },
  mounted () {
    console.log("Heloo");
    axios
        .get('http://localhost:2938/')
        .then(response => (this.message = response.data))
    axios
        .get('http://localhost:2938/store?key=gregoire&value=lemeilleur')
        .then(response => (this.message2 = response.data))
  },
}
</script>

<style>

</style>