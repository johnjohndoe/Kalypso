/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Represent point of a gaf file with the same station.
 * 
 * @author Gernot Belger
 */
public class GafProfile
{
  private static final String KIND_PROFILE = "P"; //$NON-NLS-1$

  private static final String KIND_WATERLEVEL = "W"; //$NON-NLS-1$

  /** Linked in order to preserve original order from graf file */
  private final Map<String, GafPart> m_parts = new LinkedHashMap<String, GafPart>();

  private final BigDecimal m_station;

  private String m_lastKind = null;

  private final GafLogger m_logger;

  private final GeometryFactory m_geometryFactory;

  public GafProfile( final BigDecimal station, final GafLogger logger, final GeometryFactory geometryFactory )
  {
    m_station = station;
    m_logger = logger;
    m_geometryFactory = geometryFactory;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public void addPoint( final GafPoint point )
  {
    final BigDecimal station = point.getStation();

    Assert.isTrue( station.equals( m_station ) );

    final GafCode codeKZ = point.getCodeKZ();
    Assert.isNotNull( codeKZ );

    final String kind = codeKZ.getKind();

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
        if( KIND_WATERLEVEL.equals( m_lastKind ) || kind.equals( KIND_WATERLEVEL ) )
          return;

        final String message = String.format( "discontinuos part of kind '%s'", kind );
        m_logger.log( IStatus.WARNING, message );
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

  public LineString createLine( )
  {
    /* Normally, the line of the cross section is the line of the profile */
    final GafPart profilePart = m_parts.get( KIND_PROFILE );
    if( profilePart != null )
      return profilePart.getLine();

    /* Else, if it is only one single part, we use it */
    if( m_parts.size() == 1 )
      return m_parts.values().iterator().next().getLine();

    /* Don't known what to do now */
    return null;
  }

  public GafPart[] getParts( )
  {
    return m_parts.values().toArray( new GafPart[m_parts.size()] );
  }
}