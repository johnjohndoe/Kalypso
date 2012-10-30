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
package org.kalypso.model.wspm.pdb.internal.waterlevel2d;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.math.simplify.DouglasPeuckerLineSimplifierEx;
import org.kalypso.commons.math.simplify.ISegmentDistance;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.ProfileObjectHelper;

/**
 * Simplifies a projected waterlevel with douglas peucker.
 * 
 * @author Gernot Belger
 */
class SimplifyProjectedWaterlevelWorker
{
  private final IProfileObject m_waterlevel;

  private IProfileObject m_result;

  private final double m_maxDistance;

  public SimplifyProjectedWaterlevelWorker( final IProfileObject originalWaterlevel, final double maxDistance )
  {
    m_waterlevel = originalWaterlevel;
    m_maxDistance = maxDistance;
  }

  public IProfileObject getResult( )
  {
    return m_result;
  }

  public IStatus execute( )
  {
    /* simplify records */
    final IProfileObjectRecord[] records = m_waterlevel.getRecords().getAll();

    final ISegmentDistance<IProfileObjectRecord> distanceFunction = new ProfileObjectRecordDistance( m_maxDistance );

    final IProfileObjectRecord[] simplifiedRecords = DouglasPeuckerLineSimplifierEx.simplify( records, distanceFunction );

    /* build new object */
    m_result = ProfileObjectHelper.cloneProfileObject( m_waterlevel, null, simplifiedRecords );

    final String message = String.format( Messages.getString("SimplifyProjectedWaterlevelWorker_0"), records.length - simplifiedRecords.length ); //$NON-NLS-1$
    return new Status( IStatus.OK, WspmPdbCorePlugin.PLUGIN_ID, message );
  }
}