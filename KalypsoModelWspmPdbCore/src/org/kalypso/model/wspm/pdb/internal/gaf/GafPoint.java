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
package org.kalypso.model.wspm.pdb.internal.gaf;

import java.math.BigDecimal;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.GafPointCheck;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.transformation.transformer.JTSTransformer;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Represents a parsed profile point of a gaf file
 * 
 * @author Gernot Belger
 */
public class GafPoint
{
  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final BigDecimal m_station;

  private final String m_pointId;

  private final BigDecimal m_width;

  private final BigDecimal m_height;

  private final GafCode m_code;

  private final Roughness m_roughnessClass;

  private final Vegetation m_vegetationClass;

  private final GafCode m_hyk;

  private final Point m_point;

  private final GafPointCheck m_checker;

  public GafPoint( final GafLine line, final GafPointCheck checker, final GeometryFactory geometryFactory, final JTSTransformer transformer )
  {
    m_checker = checker;
    m_station = line.getStation();
    m_pointId = line.getPointId();
    m_width = line.getWidth();
    m_height = line.getHeight();

    final Coordinate position = createPosition( line );
    m_point = createPoint( position, geometryFactory, transformer );

    m_code = m_checker.translateCode( line.getCode() );

    m_roughnessClass = m_checker.translateRoughness( line.getRoughnessClass() );
    m_vegetationClass = m_checker.translateVegetation( line.getVegetationClass() );
    m_hyk = m_checker.translateHyk( line.getHyk() );
  }

  private Coordinate createPosition( final GafLine line )
  {
    final BigDecimal rw = line.getRw();
    final BigDecimal hw = line.getHw();
    final BigDecimal height = line.getHeight();

    final double x = rw == null ? Coordinate.NULL_ORDINATE : rw.doubleValue();
    final double y = hw == null ? Coordinate.NULL_ORDINATE : hw.doubleValue();
    final double z = height == null ? Coordinate.NULL_ORDINATE : height.doubleValue();

    return new Coordinate( x, y, z );
  }

  private Point createPoint( final Coordinate position, final GeometryFactory geometryFactory, final JTSTransformer transformer )
  {
    if( !checkPosition( position ) )
      return null;

    try
    {
      final Coordinate transformedCrd = transformer.transform( position );
      return geometryFactory.createPoint( transformedCrd );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_stati.add( IStatus.ERROR, Messages.getString( "GafPoint.0" ), e ); //$NON-NLS-1$
      return null;
    }
  }

  private boolean checkPosition( final Coordinate position )
  {
    if( Double.isNaN( position.x ) || Double.isNaN( position.y ) )
    {
      m_stati.add( IStatus.WARNING, Messages.getString( "GafPoint.1" ) ); //$NON-NLS-1$
      return false;
    }

    if( Double.isNaN( position.z ) || Double.isNaN( position.y ) )
      m_stati.add( IStatus.WARNING, Messages.getString( "GafPoint.2" ) ); //$NON-NLS-1$

    return true;
  }

  public String getPointId( )
  {
    return m_pointId;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public GafCode getCode( )
  {
    return m_code;
  }

  public BigDecimal getWidth( )
  {
    return m_width;
  }

  public BigDecimal getHeight( )
  {
    return m_height;
  }

  public GafCode getHyk( )
  {
    return m_hyk;
  }

  public Roughness getRoughnessClass( )
  {
    return m_roughnessClass;
  }

  public Vegetation getVegetationClass( )
  {
    return m_vegetationClass;
  }

  public Point getPoint( )
  {
    return m_point;
  }
}