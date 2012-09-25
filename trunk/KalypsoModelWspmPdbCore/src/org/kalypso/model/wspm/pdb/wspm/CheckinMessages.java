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

/**
 * @author Gernot Belger
 */
public interface CheckinMessages
{
  String STR_STATE_WILL_BE_OVERWRITTEN = "Der Zustand mit gleichem Namen existiert bereits und wird aktualisiert.";

  String STR_UPLOAD_IMPOSSIBLE = " Bitte geben Sie einen anderen Namen ein, um den Strang als neuen Zustand hochzuladen.";

  String STR_STATE_ISZERO = "Ein Zustand mit diesem Namen existiert bereits, ist aber schreibgeschützt (Ur-Zustand).\n" + STR_UPLOAD_IMPOSSIBLE;

  String STR_SISTER_STATE_EXISTS = "Es existiert bereits ein Zustand innerhalb des Gewässers mit diesem Namen.\n" + STR_UPLOAD_IMPOSSIBLE;

  String STR_STATE_EXISTS_IN_DIFFERENT_WATER = "Es existiert bereits ein Zustand mit diesem Namen in einem anderen Gewässer.\n" + STR_UPLOAD_IMPOSSIBLE;
}