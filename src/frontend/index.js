import "./static/css/index.css";
import "./static/css/spinner.css";

import("./output/Main")
  .then((m) => m.main())
  .catch((err) => console.log(err));
