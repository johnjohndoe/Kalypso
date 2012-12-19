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

import java.math.BigDecimal;
import java.util.Arrays;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.ProfileObjectRecordWidthComparator;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.ProfileObjectHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * @author Gernot Belger
 */
class ProjectOriginalWaterlevelWorker
{
  private final String m_eventName;

  private final ProjectedWaterlevel[] m_waterlevels;

  private IProfileObject m_result;

  public ProjectOriginalWaterlevelWorker( final String eventName, final ProjectedWaterlevel[] waterlevels )
  {
    m_eventName = eventName;
    m_waterlevels = waterlevels;
  }

  public IProfileObject getResult( )
  {
    return m_result;
  }

  public IStatus execute( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    final IStatusCollector log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

    final GenericProfileHorizon waterlevel2D = new GenericProfileHorizon( IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS );

    /* set general data */
    // TODO: important, that name is unique within the cross section, how can we force this here?
    waterlevel2D.setValue( IGafConstants.PART_NAME, m_eventName );

    /* set global discharge value */
    final AggregatedWaterlevel aggregator = new AggregatedWaterlevel( m_waterlevels, null );
    final BigDecimal discharge = aggregator.getDischarge();
    if( discharge != null )
      waterlevel2D.setValue( IGafConstants.METADATA_WATERLEVEL_DISCHARGE, discharge.toString() );

    /* convert to points */
    final IProfileObjectRecords records = waterlevel2D.getRecords();

    for( final ProjectedWaterlevel waterlevel : m_waterlevels )
    {
      try
      {
        final IStatus status = waterlevel.createOriginalRecord( records );
        if( !status.isOK() )
          log.add( status );
      }
      catch( final MismatchedDimensionException e )
      {
        e.printStackTrace();
        log.add( IStatus.ERROR, e.toString() );
      }
      catch( final FactoryException e )
      {
        e.printStackTrace();
        log.add( IStatus.ERROR, e.toString() );
      }
      catch( final TransformException e )
      {
        e.printStackTrace();
        log.add( IStatus.ERROR, e.toString() );
      }
    }

    /* sort records by width */
    m_result = sortBywidth( waterlevel2D );

    return log.asMultiStatusOrOK( Messages.getString("ProjectOriginalWaterlevelWorker_0") ); //$NON-NLS-1$
  }

  private IProfileObject sortBywidth( final GenericProfileHorizon waterlevel2d )
  {
    /* sort */
    final IProfileObjectRecord[] allRecords = waterlevel2d.getRecords().getAll();
    Arrays.sort( allRecords, new ProfileObjectRecordWidthComparator() );

    /* clone to change order */
    return ProfileObjectHelper.cloneProfileObject( waterlevel2d, null, allRecords );
  }
}