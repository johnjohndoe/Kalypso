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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 */
public class GMLNodeResult extends AbstractFeatureBinder implements INodeResult
{
  public static final QName QNAME_PROP_CALCID = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "calcId" );

  public static final QName QNAME_PROP_LOCATION = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "location" );

  /*
   * the virtual depth is calculated by the calculation core RMA10 and can differ from the true depth defined by water
   * level minus node elevation! (Marsh-Algorithm).
   *
   * for that reason the true depth is computed separately.
   */
  private static final QName QNAME_PROP_VIRTUALDEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "virtualdepth" );

  private static final QName QNAME_PROP_WATERLEVEL = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "waterlevel" );

  private static final QName QNAME_PROP_DEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "depth" );

  private static final QName QNAME_PROP_VELOCITY = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "velocity" );

  private static final QName QNAME_PROP_MIDSIDE = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "midside" );

  private static final QName QNAME_PROP_DRY = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "dry" );

  public final List<ArcResult> m_arcs = new LinkedList<ArcResult>();

  public final List<Double> m_lambdas = new LinkedList<Double>();

  private boolean m_nodeAssigned;

  public List<ArcResult> getArcs( )
  {
    return m_arcs;
  }

  public void setArc( final ArcResult arc )
  {
    m_arcs.add( arc );
  }

  public GMLNodeResult( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public void setCalcId( final int id )
  {
    getFeature().setProperty( QNAME_PROP_CALCID, new Integer( id ) );
  }

  public void setDry( final int dry )
  {
    getFeature().setProperty( QNAME_PROP_DRY, new Integer( dry ) );
  }

  public void setDepth( final double depth )
  {
    getFeature().setProperty( QNAME_PROP_DEPTH, new Double( depth ) );
  }

  public void setWaterlevel( final double waterlevel )
  {
    getFeature().setProperty( QNAME_PROP_WATERLEVEL, new Double( waterlevel ) );
  }

  public void setLocation( final double x, final double y, final double z, final CS_CoordinateSystem crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    final GM_Point point = GeometryFactory.createGM_Point( position, crs );

    getFeature().setProperty( QNAME_PROP_LOCATION, point );
  }

  public void setResultValues( final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    getFeature().setProperty( QNAME_PROP_VIRTUALDEPTH, virtualDepth );
    getFeature().setProperty( QNAME_PROP_WATERLEVEL, waterlevel );

    final List<Double> veloList = new ArrayList<Double>();
    veloList.clear();
    veloList.add( vx );
    veloList.add( vy );
    getFeature().setProperty( QNAME_PROP_VELOCITY, veloList );

    /* check the real depth by comparing water level with terrain elevation */
    double depth = getDepth();
    getFeature().setProperty( QNAME_PROP_DEPTH, depth );
  }

  public void setMidSide( final boolean isMidSide )
  {
    getFeature().setProperty( QNAME_PROP_MIDSIDE, isMidSide );
  }

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( GMLNodeResult.QNAME_PROP_LOCATION );
  }

  @SuppressWarnings("unchecked")
  public List<Double> getVelocity( )
  {
    return (List<Double>) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VELOCITY );
  }

  public double getVirtualDepth( )
  {
    return (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VIRTUALDEPTH );
  }

  public double getDepth( )
  {
    Object property = getFeature().getProperty( GMLNodeResult.QNAME_PROP_WATERLEVEL );
    final double waterlevel;// = (Double) property;
    if( property != null && property instanceof Double )
      waterlevel = ((Double) property).doubleValue();
    else
      waterlevel = Double.NaN;
    final GM_Point point = (GM_Point) getFeature().getProperty( GMLNodeResult.QNAME_PROP_LOCATION );
    final double z = point.getZ();

    final double depth = waterlevel - z;
    if( depth <= 0 )
      return 0.0;
    else
      return depth;
  }

  public double getWaterlevel( )
  {
    return FeatureHelper.getAsDouble( getFeature(), GMLNodeResult.QNAME_PROP_WATERLEVEL, Double.NaN );
  }

  public int getDry( )
  {
    return (Integer) getFeature().getProperty( GMLNodeResult.QNAME_PROP_DRY );
  }

  public int getNodeID( )
  {
    return (Integer) getFeature().getProperty( GMLNodeResult.QNAME_PROP_CALCID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setVelocity(java.util.List)
   */
  public void setVelocity( final List<Double> velocity )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#isWet()
   */
  public boolean isWet( )
  {
    if( getDepth() <= 0 )
      return false;
    else
      return true;
  }

  public boolean isAssigned( )
  {
    if( m_nodeAssigned == true )
      return true;
    else
      return false;
  }

  public void setAssigned( final boolean assign )
  {
    m_nodeAssigned = assign;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getAbsoluteVelocity()
   */
  public double getAbsoluteVelocity( )
  {
    final List<Double> velocity = getVelocity();
    if( velocity == null )
      return Double.NaN;

    return Math.sqrt( velocity.get( 0 ) * velocity.get( 0 ) + velocity.get( 1 ) * velocity.get( 1 ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#addLambda()
   */
  public void addLambda( final double lambda )
  {
    m_lambdas.add( lambda );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getAveragedLambda()
   */
  public double getAveragedLambda( )
  {
    if( m_lambdas.size() > 0 )
    {
      double sum = 0;
      for( double lambda : m_lambdas )
      {
        sum += lambda;
      }
      return sum / m_lambdas.size();
    }
    else
      return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setVirtualDepth(double)
   */
  public void setVirtualDepth( double virtualDepth )
  {
    getFeature().setProperty( QNAME_PROP_VIRTUALDEPTH, new Double( virtualDepth ) );

  }
}
