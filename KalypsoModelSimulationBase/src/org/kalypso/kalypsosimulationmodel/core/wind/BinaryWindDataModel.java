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
package org.kalypso.kalypsosimulationmodel.core.wind;

import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.deegree.framework.util.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.GMRectanglesClip;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.IPlainGridVisitable;
import org.kalypsodeegree.model.geometry.IPlainGridVisitor;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author ig
 *
 */
public class BinaryWindDataModel implements IWindDataProvider, IPlainGridVisitable<GM_Curve>
{
  private static final int INT_CONST_SCALE_VECTOR_LEN_PRESENTATION = 20;

  private final double m_doubleEps = 0.01;

  private final IGeoGrid m_geoGrid;

  private final RectifiedGridDomain m_gridDescriptor;

  private GM_Envelope m_gmEnvelope = null;

  private String m_strCRS = ""; //$NON-NLS-1$

  private final URL m_urlFile;

  private final boolean m_boolIsRegular = true;

  private double m_cellSizeX = 0;

  private double m_cellSizeY = 0;

  private GM_Point m_gmPointOrigin = null;

  private final Date m_date = null;

  private int m_intScale = 1;

  private final boolean m_boolDoInterpolate = true;

  public final static int MIN_VECTOR_SCALE = 1;

  public final static int MAX_VECTOR_SCALE = 11;

  public BinaryWindDataModel( final URL pUrlFile, final RectifiedGridDomain pGridDescriptor ) throws Exception
  {
    m_gridDescriptor = pGridDescriptor;
    m_urlFile = pUrlFile;
    m_strCRS = m_gridDescriptor.getCoordinateSystem();
    final Coordinate lCoordOrigin = new Coordinate( m_gridDescriptor.getOrigin( m_strCRS ).getX(), m_gridDescriptor.getOrigin( m_strCRS ).getY() );
    try
    {
      m_cellSizeX = m_gridDescriptor.getOffsetX( m_strCRS );
      m_cellSizeY = m_gridDescriptor.getOffsetY( m_strCRS );
      m_gmPointOrigin = m_gridDescriptor.getOrigin( m_strCRS );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    final Coordinate lOffsetX = new Coordinate( m_gridDescriptor.getOffsetX().getGeoX(), m_gridDescriptor.getOffsetX().getGeoY() );
    final Coordinate lOffsetY = new Coordinate( m_gridDescriptor.getOffsetY().getGeoX(), m_gridDescriptor.getOffsetY().getGeoY() );
    m_geoGrid = BinaryGeoGridWrapperForPairsModel.openGrid( pUrlFile, lCoordOrigin, lOffsetX, lOffsetY, m_strCRS, false );
    final GM_Position lGMPositionMAX = GeometryFactory.createGM_Position( m_gmPointOrigin.getX() + m_gridDescriptor.getNumColumns() * m_cellSizeX, m_gmPointOrigin.getY() + m_gridDescriptor.getNumRows()
        * m_cellSizeY );
    m_gmEnvelope = GeometryFactory.createGM_Envelope( m_gmPointOrigin.getPosition(), lGMPositionMAX, m_strCRS );

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    return m_gmEnvelope;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return m_strCRS;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataAsGrid()
   */
  @Override
  public IGeoGrid getDataAsGrid( )
  {
    return m_geoGrid;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( )
  {
    return m_gridDescriptor;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsSpeedAndDirection(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsSpeedAndDirection( final GM_Point location )
  {
    return NativeWindDataModelHelper.convertVectorWindToSpeedAndDirection( getWindAsVector( location ) );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsVector(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsVector( final GM_Point location )
  {
    if( !m_boolDoInterpolate || isPositionExactGridNode( location ) )
    {
      return getWindValuesFromGrid( location );
    }
    else
    {
      final List<GM_Point> lListNeiboursPositions = geNeiboursPositions( location );
      final Map<GM_Point, Pair<Double, Double>> lMapNeiboursValues = getNeiboursValues( lListNeiboursPositions );
      return NativeWindDataModelHelper.getInterpolatedPair( location, lMapNeiboursValues );//, lListNeiboursPositions );
    }
  }

  private List<GM_Point> geNeiboursPositions( final GM_Point location )
  {
    final List<GM_Point> lListRes = new ArrayList<>();

    final int col = (int) Math.floor( (location.getX() - m_gmEnvelope.getMin().getX()) / m_cellSizeX );
    int row = (int) Math.floor( (location.getY() - m_gmEnvelope.getMin().getY()) / m_cellSizeY );
    if( row < 0 )
      row = 0;
    if( col + 1 <= m_gridDescriptor.getNumColumns() && row + 1 <= m_gridDescriptor.getNumRows() && col >= 0 && row >= 0 )
    {
      lListRes.add( GeometryFactory.createGM_Point( m_gmPointOrigin.getX() + m_cellSizeX * col,  m_gmPointOrigin.getY() + m_cellSizeY * row, m_strCRS ) );
      lListRes.add( GeometryFactory.createGM_Point( m_gmPointOrigin.getX() + m_cellSizeX * ( col + 1 ),  m_gmPointOrigin.getY() + m_cellSizeY * row, m_strCRS ) );
      lListRes.add( GeometryFactory.createGM_Point( m_gmPointOrigin.getX() + m_cellSizeX * ( col + 1 ),  m_gmPointOrigin.getY() + m_cellSizeY * ( row + 1 ), m_strCRS ) );
      lListRes.add( GeometryFactory.createGM_Point( m_gmPointOrigin.getX() + m_cellSizeX * ( col ),  m_gmPointOrigin.getY() + m_cellSizeY * ( row + 1 ), m_strCRS ) );
    }
    else{
      return null;
    }
//    else if( ( col + 1 ) == m_gridDescriptor.getNumColumns() || ( row + 1 ) == m_gridDescriptor.getNumRows() || col == 0 || row == 0 )
//    {
//    }
    return lListRes;

  }

  private Pair<Double, Double> getWindValuesFromGrid( final GM_Point location )
  {
    if( m_gmEnvelope != null && location != null )
    {
      final int col = (int) Math.floor( (location.getX() - m_gmEnvelope.getMin().getX()) / m_cellSizeX );
      int row = (int) Math.floor( (location.getY() - m_gmEnvelope.getMin().getY()) / m_cellSizeY );
      if( row < 0 )
        row = 0;
      if( col < m_gridDescriptor.getNumColumns() && row < m_gridDescriptor.getNumRows() && col >= 0 && row >= 0 )
      {
        try
        {
          final Pair<Double, Double> lPairData = ((BinaryGeoGridWrapperForPairsModel) m_geoGrid).getPairValue( col, row );
          return lPairData;
        }
        catch( final GeoGridException e )
        {
          e.printStackTrace();
        }
      }
    }
    return null;
  }

  private Map<GM_Point, Pair<Double, Double>> getNeiboursValues( final List<GM_Point> listNeiboursPositions )
  {
    if( listNeiboursPositions == null || listNeiboursPositions.size() != 4 )
    {
      return null;
    }

    final Map<GM_Point, Pair<Double, Double>> lMapRes = new HashMap<>();
    for( final GM_Point lPoint : listNeiboursPositions )
    {
      lMapRes.put( lPoint, getWindValuesFromGrid( lPoint ) );
    }
    return lMapRes;
  }

  private boolean isPositionExactGridNode( final GM_Point location )
  {
    try
    {
      if( ((location.getX() - m_gmPointOrigin.getX()) / m_cellSizeX - (int) Math.floor( (location.getX() - m_gmPointOrigin.getX()) / m_cellSizeX )) <= m_doubleEps
          && ((location.getY() - m_gmPointOrigin.getY()) / m_cellSizeY - (int) Math.floor( (location.getY() - m_gmPointOrigin.getY()) / m_cellSizeY )) <= m_doubleEps )
        return true;
    }
    catch( final Exception e )
    {
    }
    return false;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#isRegularGrid()
   */
  @Override
  public boolean isRegularGrid( )
  {
    return m_boolIsRegular;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    m_strCRS = coordinateSystem;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataFile()
   */
  @Override
  public URL getDataFileURL( )
  {
    return m_urlFile;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.IPlainGridVisitable#acceptVisits(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.IPlainGridVisitor, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void acceptVisits( final GM_Envelope envToVisit, final IPlainGridVisitor<GM_Curve> gridVisitor, final IProgressMonitor monitor )
  {
    if( envToVisit == null || envToVisit.getMin().equals( envToVisit.getMax() ) )
    {
      return;
    }
    final GM_Envelope env = GMRectanglesClip.getIntersectionEnv( m_gmEnvelope, envToVisit );
    final double xmin = env.getMin().getX();
    final int col = (int) Math.floor( (xmin - m_gmPointOrigin.getX()) / m_cellSizeX );
    final double ymin = env.getMin().getY();
    int row = (int) Math.floor( (ymin - m_gmPointOrigin.getY()) / m_cellSizeY );
    if( row < 0 )
      row = 0;
    final double lDoubleActScale = Math.min( envToVisit.getHeight(), envToVisit.getWidth() ) / Math.min( m_cellSizeX, m_cellSizeY );

    m_intScale = (int) (1 + lDoubleActScale / INT_CONST_SCALE_VECTOR_LEN_PRESENTATION);

    if( m_intScale < MIN_VECTOR_SCALE )
    {
      m_intScale = MIN_VECTOR_SCALE;
    }
    else if( m_intScale > MAX_VECTOR_SCALE )
    {
      m_intScale = MAX_VECTOR_SCALE;
    }

    if( col <= m_gridDescriptor.getNumColumns() && row <= m_gridDescriptor.getNumRows() && col >= 0 && row >= 0 )
    {
      monitor.beginTask( "Painting Wind Data", row ); //$NON-NLS-1$

      final int N_COL_ENV = Math.min( (int) Math.round( env.getWidth() / m_cellSizeX ) + col + 1, m_gridDescriptor.getNumColumns() );
      final int N_ROW_ENV = Math.min( (int) Math.round( env.getHeight() / m_cellSizeY ) + row + 1, m_gridDescriptor.getNumRows() );
      for( int i = row; i < N_ROW_ENV; i += m_intScale )
      {
        for( int j = col; j < N_COL_ENV; j += m_intScale )
        {
          try
          {
            // the values are saved in binary grid per default in vector representation of wind.
            // it is also needed as vector for painting of wind layer
            final Pair<Double, Double> lPairWindValues = ((BinaryGeoGridWrapperForPairsModel) m_geoGrid).getPairValue( j, i );
            if( Double.isNaN( lPairWindValues.first ) || Double.isNaN( lPairWindValues.second ) )
            {
              continue;
            }
            final GM_Curve lMeanCurve = createMeanVectorPartCurveForWind( i, j, lPairWindValues, m_intScale );

            gridVisitor.visit( lMeanCurve );
          }
          catch( final GeoGridException e )
          {
            e.printStackTrace();
          }
        }
        monitor.worked( i );

      }
      monitor.done();
    }
  }

  private GM_Curve createMeanVectorPartCurveForWind( final int j, final int i, final Pair<Double, Double> pPairVectorWindValues, final int pIntScale )
  {
    final int intScale = pIntScale * INT_CONST_SCALE_VECTOR_LEN_PRESENTATION;
    final GM_Position[] lRes = new GM_Position[2];

    lRes[0] = GeometryFactory.createGM_Position( m_gmPointOrigin.getPosition().getX() + (i) * m_cellSizeX, m_gmPointOrigin.getPosition().getY() + (j) * m_cellSizeY );
    lRes[1] = GeometryFactory.createGM_Position( (m_gmPointOrigin.getPosition().getX() + (i) * m_cellSizeX + pPairVectorWindValues.first * (intScale + 1.1)), (m_gmPointOrigin.getPosition().getY()
        + (j) * m_cellSizeY + pPairVectorWindValues.second * (intScale + 1.1)) );

    try
    {
      return GeometryFactory.createGM_Curve( lRes, m_strCRS );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDateStep()
   */
  @Override
  public Date getDateStep( )
  {
    return m_date;
  }

  public final int getIntScale( )
  {
    return m_intScale;
  }

  public final void setIntScale( final int intScale )
  {
    m_intScale = intScale;
  }

}
