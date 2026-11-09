/* Entry point for the bundle's JavaScript islands. Each island is an
 * independent progressive enhancement over the contract in HTML_CONTRACT.md;
 * pages must remain fully readable with scripting disabled. */

import { initPopovers } from "./popover";
import { initSearch } from "./search";

initPopovers();
initSearch();
