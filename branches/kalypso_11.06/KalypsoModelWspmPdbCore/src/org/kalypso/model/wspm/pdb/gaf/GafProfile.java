/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.gaf;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.GafPart;
import org.kalypso.model.wspm.pdb.internal.gaf.GafPoint;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Represent point of a gaf file with the same station.
 * 
 * @author Gernot Belger
 */
public class GafProfile implements IGafConstants
{
  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  /** Linked in order to preserve original order from graf file */
  private final Map<String, GafPart> m_parts = new LinkedHashMap<String, GafPart>();

  private final BigDecimal m_station;

  private String m_lastKind = null;

  private final GeometryFactory m_geometryFactory;

  private IStatus m_status;

  public GafProfile( final BigDecimal station, final GeometryFactory geometryFactory )
  {
    m_station = station;
    m_geometryFactory = geometryFactory;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public void addPoint( final GafPoint point, final GafCode code )
  {
    final BigDecimal station = point.getStation();

    Assert.isTrue( station.equals( m_station ) );

    final String kind = code.getKind();

    checkDiscontinuosPart( kind );

    final GafPart part = getOrCreatePart( kind );

    part.add( point );

    m_lastKind = kind;
  }

  /* Check, if we have discontinues parts */
  private void checkDiscontinuosPart( final String kind )
  {
    if( !kind.equals( m_lastKind ) )
    {
      if( m_parts.containsKey( kind ) )
      {
        /* Ignore water levels can occur anywhere, thats normal */
        if( KZ_CATEGORY_WATERLEVEL.equals( m_lastKind ) || kind.equals( KZ_CATEGORY_WATERLEVEL ) )
          return;

        final String message = String.format( "Part '%s': gaps between points of this part", kind );
        m_stati.add( IStatus.WARNING, message );
      }
    }
  }

  private GafPart getOrCreatePart( final String kind )
  {
    /* Make sure this kind of parts exists */
    if( !m_parts.containsKey( kind ) )
      m_parts.put( kind, new GafPart( kind, m_geometryFactory ) );

    return m_parts.get( kind );
  }

  public Geometry createLine( final String dbType ) throws Exception
  {
    /* Normally, the line of the cross section is the line of the profile */
    final GafPart profilePart = m_parts.get( KZ_CATEGORY_PROFILE );
    if( profilePart != null )
      return profilePart.getLine( dbType );

    /* Else, if it is only one single part, we use it */
    if( m_parts.size() == 1 )
      return m_parts.values().iterator().next().getLine( dbType );

    /* Don't known what to do now */
    return null;
  }

  public GafPart[] getParts( )
  {
    return m_parts.values().toArray( new GafPart[m_parts.size()] );
  }

  public void addStatus( final int severity, final String message )
  {
    m_stati.add( severity, message );
  }

  public IStatus getStatus( )
  {
    if( m_status == null )
    {
      final Collection<GafPart> values = m_parts.values();
      for( final GafPart part : values )
        m_stati.add( part.getStatus() );

      final String okMessage = String.format( "Cross Section '%s': OK", getStation() );
      final String message = String.format( "Cross Section '%s': Warnings/Errors", getStation() );
      return m_stati.asMultiStatusOrOK( message, okMessage );
    }

    return m_status;
  }

  /**
   * Check the profile if everything is legal. Check, if the profile intersects with the riverline
   */
  public void check( final LineString riverline )
  {
    /* Find PP part */
    if( m_parts.size() == 0 )
    {
      m_stati.add( IStatus.ERROR, "Cross sections does not contain any parts" );
      return;
    }

    /* Check if PP part intersects with riverline */
    final GafPart ppPart = m_parts.get( IGafConstants.KZ_CATEGORY_PROFILE );
    if( ppPart == null )
    {
      m_stati.add( IStatus.ERROR, "No profile points (PP) in this cross section." );
      return;
    }

    try
    {
      final Geometry line = ppPart.getLine( null );
      if( line == null )
      {
        m_stati.add( IStatus.WARNING, "Cross section has no line geometry" );
      }
      else if( riverline != null && !line.intersects( riverline ) )
      {
        final double distance = line.distance( riverline );
        final String msg = String.format( "Cross section does not intersect with riverline. Distance is %.1f [km]", distance / 1000.0 );
        m_stati.add( IStatus.WARNING, msg );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_stati.add( IStatus.ERROR, "Inalid geometry for part 'PP'" );
    }

    // TODO More checks?
  }
}