const { defineConfig } = require("cypress");

module.exports = defineConfig({
  env: {
    CYPRESS_BASE_URL: "http://localhost:4200/ailabar",
    CYPRESS_USERNAME: "Juanjo",
    CYPRESS_PASSWORD: "pass123"
  },

  production: {
    CYPRESS_BASE_URL: "https://www.example.com",
    CYPRESS_USERNAME: "production_user",
    CYPRESS_PASSWORD: "tu_contraseña_produccion",
  },
  e2e: {
    setupNodeEvents(on, config) {
      // implement node event listeners here
    },
  },
});
