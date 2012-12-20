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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.ProfileObjectHelper;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Removes all points from the original waterlevel points that are lower than the lowest profile point.
 * 
 * @author Gernot Belger
 */
public class CleanProjectedWaterlevelWorker
{
  private final IProfileObject m_originalWaterlevel;

  private final MLineString m_profileLine;

  private IProfileObject m_result;

  public CleanProjectedWaterlevelWorker( final MLineString profileLine, final IProfileObject originalWaterlevel )
  {
    m_profileLine = profileLine;
    m_originalWaterlevel = originalWaterlevel;
  }

  public IProfileObject getResult( )
  {
    return m_result;
  }

  public IStatus execute( )
  {
    /* simplify records */
    final IProfileObjectRecord[] records = m_originalWaterlevel.getRecords().getAll();

    final double lowestValue = findLowestProfileValue();

    final IProfileObjectRecord[] cleanRecords = clean( records, lowestValue );

    /* build new object */
    m_result = ProfileObjectHelper.cloneProfileObject( m_originalWaterlevel, null, cleanRecords );

    final String message = String.format( Messages.getString( "SimplifyProjectedWaterlevelWorker_0" ), records.length - cleanRecords.length ); //$NON-NLS-1$
    return new Status( IStatus.OK, WspmPdbCorePlugin.PLUGIN_ID, message );
  }

  private double findLowestProfileValue( )
  {
    double minZ = Double.MAX_VALUE;

    final Coordinate[] coordinates = m_profileLine.getCoordinates();
    for( final Coordinate coordinate : coordinates )
      minZ = Math.min( coordinate.z, minZ );

    return minZ;
  }

  /**
   * Prunes all records with a height lower than the lowest point
   */
  private IProfileObjectRecord[] clean( final IProfileObjectRecord[] records, final double lowestValue )
  {
    final Collection<IProfileObjectRecord> cleanRecords = new ArrayList<>( records.length );

    for( final IProfileObjectRecord record : records )
    {
      final Double hoehe = record.getHoehe();
      if( hoehe != null && hoehe > lowestValue )
        cleanRecords.add( record );
    }

    return cleanRecords.toArray( new IProfileObjectRecord[cleanRecords.size()] );
  }
}