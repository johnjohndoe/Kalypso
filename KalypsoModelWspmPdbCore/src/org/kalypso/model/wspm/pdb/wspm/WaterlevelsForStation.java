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

import java.math.BigDecimal;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.waterlevel2d.ProjectedWaterlevels;

/**
 * Collection of {@link org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation} for one profile station.
 * 
 * @author Gernot Belger
 */
public class WaterlevelsForStation implements Comparable<WaterlevelsForStation>
{
  public static final String PROPERTY_STATION = "station"; //$NON-NLS-1$

  public static final String PROPERTY_WATERLEVEL_COUNT = "waterlevelCount"; //$NON-NLS-1$

  private final BigDecimal m_station;

  private final IStatusCollector m_levelsLog = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  // REMARK: using fatser concurrent map here because we get MANY entries sometimes
  private final Map<WaterlevelFixation, IStatus> m_waterlevels = new ConcurrentHashMap<>();

  private IStatus m_waterlevel2dStatus;

  private IProfileObject[] m_waterlevels2DObjecs;

  private ISectionProvider m_section;

  public WaterlevelsForStation( final BigDecimal station )
  {
    m_station = station;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public int getWaterlevelCount( )
  {
    return m_waterlevels.size();
  }

  public void addWaterlevel( final WaterlevelFixation waterlevel, final IStatus status )
  {
    m_waterlevels.put( waterlevel, status );

    if( !status.isOK() )
      m_levelsLog.add( status );
  }

  @Override
  public int compareTo( final WaterlevelsForStation other )
  {
    final BigDecimal s1 = m_station;
    final BigDecimal s2 = other.m_station;

    return s1.compareTo( s2 );
  }

  public boolean isValid( )
  {
    return !validate().matches( IStatus.WARNING | Status.ERROR );
  }

  public IStatus validate( )
  {
    final IStatusCollector status = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

    /* raw stati of levels */
    final IStatus levelsStatus = m_levelsLog.asMultiStatus( "Read waterlevels" );
    status.add( levelsStatus );

    /* section present? */
    if( m_waterlevel2dStatus != null )
      status.add( m_waterlevel2dStatus );

    return status.asMultiStatus( m_station.toString() );
  }

  public IStatus create2DWaterlevels( final String eventName, final ISectionProvider section )
  {
    m_waterlevel2dStatus = update2dWaterlevels( eventName, section );
    return m_waterlevel2dStatus;
  }

  private IStatus update2dWaterlevels( final String eventName, final ISectionProvider section )
  {
    m_section = section;
    m_waterlevels2DObjecs = null;

    if( section == null )
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "No cross section with same station" );

    final IStatusCollector log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

    try
    {
      final WaterlevelFixation[] waterlevels = m_waterlevels.keySet().toArray( new WaterlevelFixation[m_waterlevels.keySet().size()] );

      final MLineString profileLine = section.getProfileLine();

      if( profileLine != null )
      {
        log.add( IStatus.OK, "Valid cross section found" );

        final ProjectedWaterlevels projected = new ProjectedWaterlevels( eventName, m_station, profileLine, waterlevels );

        // TODO: douglas peucker

        m_waterlevels2DObjecs = projected.createParts();

        // TODO fetch simplified fixations and add to event
        log.add( projected.getStatus() );
      }
      else
        log.add( IStatus.WARNING, "Cross section lacks valid geometry" );

      return log.asMultiStatus( "Create 2D-waterlevels" );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "Failed to create 2D-waterlevels", e );
    }
  }

  public IProfileObject[] getWaterlevelObjects( )
  {
    return m_waterlevels2DObjecs;
  }

  public ISectionProvider getSection( )
  {
    return m_section;
  }
}