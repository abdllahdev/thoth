import app from './app';
import { appConfig } from './configs';

const start = async () => {
  try {
    // start server
    app.listen(appConfig.PORT, () =>
      console.log(
        `Server running on port ${appConfig.PORT} - Environment: ${process.env.NODE_ENV}`,
      ),
    );
  } catch (error) {
    console.error(error);
    process.exit(1);
  }
};

start();
