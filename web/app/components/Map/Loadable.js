/**
 *
 * Asynchronously loads the component for Map
 *
 */

import loadable from 'utils/loadable';

export default loadable(() => import('./index'));
