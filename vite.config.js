import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  base: "/BLE-Micro-Pro-WebConfigurator/",
  build: {
    outDir: "dist",
  },
});
