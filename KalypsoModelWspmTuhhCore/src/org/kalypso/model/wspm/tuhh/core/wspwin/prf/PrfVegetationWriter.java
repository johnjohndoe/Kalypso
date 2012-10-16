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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

/**
 * @author Dirk Kuch
 */
public class PrfVegetationWriter
{
  private final boolean m_preferClasses;

  private final IProfile m_profile;

  private final DataBlockWriter m_dbWriter;

  public PrfVegetationWriter( final DataBlockWriter dbWriter, final IProfile profile, final boolean preferClasses )
  {
    m_dbWriter = dbWriter;
    m_profile = profile;
    m_preferClasses = preferClasses;
  }

  public void writeBewuchs( )
  {
    if( !hasBewuchs() )
      return;

    final DataBlockHeader dbhx = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    writeCoords( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX, dbx, 0.0 );
    m_dbWriter.addDataBlock( dbx );

    final DataBlockHeader dbhy = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    m_dbWriter.addDataBlock( dby );
    writeCoords( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY, dby, 0.0 );

    final DataBlockHeader dbhp = PrfHeaders.createHeader( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP, dbp, 0.0 );
    m_dbWriter.addDataBlock( dbp );
  }

  /**
   * Returns <code>true</code>, if any of the Bewuchs components is present.
   */
  private boolean hasBewuchs( )
  {
    if( m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS ) != null )
      return true;

    if( m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) != null )
      return true;

    if( m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ) != null )
      return true;

    if( m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ) != null )
      return true;

    return false;
  }

  void writeCoords( final String valueComponentId, final CoordDataBlock db, final Double nullValue )
  {
    final IProfileRecord[] points = m_profile.getPoints();

    final List<Double> arrX = new ArrayList<>( points.length );
    final List<Double> arrY = new ArrayList<>( points.length );

    final int indexWidth = m_profile.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );

    for( final IProfileRecord point : points )
    {
      final Double width = (Double)point.getValue( indexWidth );

      final BigDecimal value = WspmClassifications.getVegetationValue( point, valueComponentId, m_preferClasses );
      final Number fixedValue = Objects.firstNonNull( value, nullValue );

      if( value != null && fixedValue != null )
      {
        arrX.add( width );
        arrY.add( fixedValue.doubleValue() );
      }
    }

    final Double[] xArray = arrX.toArray( new Double[arrX.size()] );
    final Double[] yArray = arrY.toArray( new Double[arrY.size()] );
    db.setCoords( xArray, yArray );
  }
}