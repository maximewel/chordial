import Vue from 'vue';
import Vuetify from 'vuetify/lib/framework';
import colors from 'vuetify/lib/util/colors'

Vue.use(Vuetify);

export default new Vuetify({
   theme: {
		dark: true,
		options: { customProperties: true },
		themes: {
			dark: {
				primary: "#242832",
				secondary: colors.grey.lighten4,
				accent: "#7764FD",
			},
		},
	},
});
