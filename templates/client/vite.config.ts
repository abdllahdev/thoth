import { defineConfig } from "vite";
import path from "path";
import react from "@vitejs/plugin-react-swc";
import WindiCSS from "vite-plugin-windicss";
import Pages from "vite-plugin-pages";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), WindiCSS(), Pages()],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
});
