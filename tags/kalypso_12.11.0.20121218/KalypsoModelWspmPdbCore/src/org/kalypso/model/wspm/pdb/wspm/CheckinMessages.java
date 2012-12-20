/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.wspm;

import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public interface CheckinMessages
{
  String STR_STATE_WILL_BE_OVERWRITTEN = Messages.getString("CheckinMessages_0"); //$NON-NLS-1$

  String STR_UPLOAD_IMPOSSIBLE = Messages.getString("CheckinMessages_1"); //$NON-NLS-1$

  String STR_STATE_ISZERO = Messages.getString("CheckinMessages_2") + STR_UPLOAD_IMPOSSIBLE; //$NON-NLS-1$

  String STR_SISTER_STATE_EXISTS = Messages.getString("CheckinMessages_3") + STR_UPLOAD_IMPOSSIBLE; //$NON-NLS-1$

  String STR_STATE_EXISTS_IN_DIFFERENT_WATER = Messages.getString("CheckinMessages_4") + STR_UPLOAD_IMPOSSIBLE; //$NON-NLS-1$
}