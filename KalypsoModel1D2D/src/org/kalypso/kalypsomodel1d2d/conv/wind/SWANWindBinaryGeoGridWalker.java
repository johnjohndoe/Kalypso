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
package org.kalypso.kalypsomodel1d2d.conv.wind;

import java.util.Locale;

import org.deegree.framework.util.Pair;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.core.wind.BinaryGeoGridWrapperForPairsModel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This grid walker will write the values inside of given envelope into the given {@link StringBuffer}s
 * the content of thus buffers will be flushed to the file in according order.
 * 
 * @author ig
 * 
 */
public class SWANWindBinaryGeoGridWalker extends AbstractWindGeoGridWalker
{

  private static final double DOUBLE_DEFAULT_IGNORE_VALUE = 0;

  private int m_intLastY = -1;
  
  private StringBuffer[] m_stringBuffers;
  /**
   * The constructor.
   * 
   * @param pGMEnvelope {@link GM_Envelope}
   *          This envelope will be walked.
   * @param coordinateSystem
   *          The coordinate system.
   * @param grid descriptor {@link RectifiedGridDomain} of grid to visit 
   */
  public SWANWindBinaryGeoGridWalker( final GM_Envelope pGMEnvelope, final StringBuffer[] pStringBuffers, final RectifiedGridDomain pGridDescriptorOrig )
  {
    super( pGMEnvelope, pGridDescriptorOrig );
    m_stringBuffers = pStringBuffers; 
    m_intLastY = -1;
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   *  this operation based on line by line walking strategy according to implementation
   *  in  {@link org.kalypso.kalypsosimulationmodel.core.wind.SimpleWindDataGridWalkingStrategy} 
   */
  @Override
  public void operate( int x, int y, Coordinate c ) throws GeoGridException
  {
    if( m_intLastY != -1 && m_intLastY != y ){
      m_stringBuffers[ 0 ].append( String.format( "\n" ) );
      m_stringBuffers[ 1 ].append( String.format( "\n" ) );
    }
    Pair<Double, Double> lPairVectorNativeWindValues = ((BinaryGeoGridWrapperForPairsModel) getGrid()).getPairValue( x, y );
    if( lPairVectorNativeWindValues == null || Double.isNaN( lPairVectorNativeWindValues.first ) || Double.isNaN( lPairVectorNativeWindValues.second ) )
    {
      m_stringBuffers[ 0 ].append( String.format( Locale.US, "%.2f ", DOUBLE_DEFAULT_IGNORE_VALUE ) );
      m_stringBuffers[ 1 ].append( String.format( Locale.US, "%.2f ", DOUBLE_DEFAULT_IGNORE_VALUE ) );
    }
    else
    {
      m_stringBuffers[ 0 ].append( String.format( Locale.US, "%.2f ", lPairVectorNativeWindValues.first ) );
      m_stringBuffers[ 1 ].append( String.format( Locale.US, "%.2f ", lPairVectorNativeWindValues.second ) );
    }
    m_intLastY = y;
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#start(org.kalypso.grid.IGeoGrid)
   */
  @Override
  public void start( IGeoGrid grid )
  {
    setGrid( grid );
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#finish()
   */
  @Override
  public Object finish( ) 
  {
    return null;
  }

  protected final StringBuffer[] getStringBuffers( )
  {
    return m_stringBuffers;
  }

}
