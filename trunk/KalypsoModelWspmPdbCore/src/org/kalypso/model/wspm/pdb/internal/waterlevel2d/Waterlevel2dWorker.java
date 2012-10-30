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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.ISectionProvider;
import org.kalypso.model.wspm.pdb.wspm.WaterlevelsForStation;

/**
 * @author Gernot Belger
 */
public class Waterlevel2dWorker
{
  private final IStatusCollector m_log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final ISectionProvider[] m_sections;

  private final Collection<WaterlevelFixation> m_waterlevels;

  private final String m_eventName;

  private final Collection<WaterlevelsForStation> m_waterlevels2d = new ArrayList<>();

  public Waterlevel2dWorker( final String eventName, final Collection<WaterlevelFixation> waterlevels, final ISectionProvider[] sections )
  {
    m_eventName = eventName;
    m_waterlevels = waterlevels;
    m_sections = sections;
  }

  public WaterlevelsForStation[] getWaterlevels2D( )
  {
    return m_waterlevels2d.toArray( new WaterlevelsForStation[m_waterlevels2d.size()] );
  }

  public IStatus execute( final IProgressMonitor monitor )
  {
    /* hash by station */
    final Map<BigDecimal, Collection<WaterlevelFixation>> waterlevelsByStation = hashWaterlevelsByStation();

    monitor.beginTask( Messages.getString("Waterlevel2dWorker_0"), m_sections.length ); //$NON-NLS-1$

    for( final ISectionProvider sectionProvider : m_sections )
    {
      final BigDecimal station = sectionProvider.getStation();

      monitor.subTask( station.toString() );

      final Collection<WaterlevelFixation> waterlevels = waterlevelsByStation.get( station );
      if( !CollectionUtils.isEmpty( waterlevels ) )
      {
        final IStatus status = createWaterlevel( sectionProvider, waterlevels );
        m_log.add( status );
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    monitor.done();

    return m_log.asMultiStatusOrOK( Messages.getString("Waterlevel2dWorker_1") ); //$NON-NLS-1$
  }

  private IStatus createWaterlevel( final ISectionProvider sectionProvider, final Collection<WaterlevelFixation> waterlevels )
  {
    final WaterlevelsForStation waterlevelsForStation = new WaterlevelsForStation( sectionProvider.getStation() );

    for( final WaterlevelFixation waterlevel : waterlevels )
      waterlevelsForStation.addWaterlevel( waterlevel, Status.OK_STATUS );

    m_waterlevels2d.add( waterlevelsForStation );

    return waterlevelsForStation.create2DWaterlevels( m_eventName, sectionProvider );
  }

  private Map<BigDecimal, Collection<WaterlevelFixation>> hashWaterlevelsByStation( )
  {
    final Map<BigDecimal, Collection<WaterlevelFixation>> hash = new TreeMap<>();

    for( final WaterlevelFixation waterlevel : m_waterlevels )
    {
      final BigDecimal station = waterlevel.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<WaterlevelFixation>() );

      hash.get( station ).add( waterlevel );
    }

    return hash;
  }
}