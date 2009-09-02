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

import org.kalypso.kalypsomodel1d2d.conv.results.ArcResult;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class GMLNodeResult extends AbstractFeatureBinder implements INodeResult
{
  public static final QName QNAME_PROP_CALCID = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "calcId" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_LOCATION = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "location" ); //$NON-NLS-1$

  /*
   * the virtual depth is calculated by the calculation core RMA∑Kalypso and can differ from the true depth defined by
   * water level minus node elevation! (Marsh-Algorithm).
   * 
   * for that reason the true depth is computed separately.
   */
  private static final QName QNAME_PROP_VIRTUALDEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "virtualdepth" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VIRTDEPPREVSTEP = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "virtDepPrevStep" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VIRTDEPOVERTIME = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "virtDepOverTime" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "virtDepOverTimePrevStep" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_WATERLEVEL = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "waterlevel" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_DEPTH = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "depth" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VELOCITY = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "velocity" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VELPREVSTEP = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "velPrevStep" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VELOVERTIME = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "velOverTime" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_VELOVERTIMEPREVSTEP = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "velOverTimePrevStep" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_DISCHARGE = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "discharge" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_MIDSIDE = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "midside" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_DRY = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "dry" ); //$NON-NLS-1$

  public final List<ArcResult> m_arcs = new LinkedList<ArcResult>();

  public final List<Double> m_lambdas = new LinkedList<Double>();

  private boolean m_nodeAssigned;

  public GMLNodeResult( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public List<ArcResult> getArcs( )
  {
    return m_arcs;
  }

  public void setArc( final ArcResult arc )
  {
    m_arcs.add( arc );
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
    // depth is a function property, it will not be set anyway
    // TODO: remove this method
    // getFeature().setProperty( QNAME_PROP_DEPTH, new Double( depth ) );
  }

  public void setWaterlevel( final double waterlevel )
  {
    getFeature().setProperty( QNAME_PROP_WATERLEVEL, new Double( waterlevel ) );
  }

  public void setLocation( final double x, final double y, final double z, final String crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    final GM_Point point = GeometryFactory.createGM_Point( position, crs );

    getFeature().setProperty( QNAME_PROP_LOCATION, point );
  }

  public void setResultValues( final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    setVirtualDepth( virtualDepth );
    setWaterlevel( waterlevel );

    final List<Double> veloList = new ArrayList<Double>();
    veloList.clear();
    veloList.add( vx );
    veloList.add( vy );
    setVelocity( veloList );
  }

  public void setTimeDerivativeValues( final double vxWRTt, final double vyWRTt, final double virtDepWRTt )
  // WRT means with respect to
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPOVERTIME, virtDepWRTt );

    final List<Double> veloList = new ArrayList<Double>();
    veloList.clear();
    veloList.add( vxWRTt );
    veloList.add( vyWRTt );
    getFeature().setProperty( QNAME_PROP_VELOVERTIME, veloList );

  }

  public void setResultPrevStepValues( final double vxPrevStep, final double vyPrevStep, final double virtDepPrevStep )
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPPREVSTEP, virtDepPrevStep );

    final List<Double> velPrevStepList = new ArrayList<Double>();
    velPrevStepList.clear();
    velPrevStepList.add( vxPrevStep );
    velPrevStepList.add( vyPrevStep );
    getFeature().setProperty( QNAME_PROP_VELPREVSTEP, velPrevStepList );
  }

  public void setTimeDerivativeValuesPrevStep( final double vxWRTtPrevStep, final double vyWRTtPrevStep, final double virtDepWRTtPrevStep )
  // WRT means with respect to
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP, virtDepWRTtPrevStep );

    final List<Double> veloList = new ArrayList<Double>();
    veloList.clear();
    veloList.add( vxWRTtPrevStep );
    veloList.add( vyWRTtPrevStep );
    getFeature().setProperty( QNAME_PROP_VELOVERTIMEPREVSTEP, veloList );

    /* check the real depth by comparing water level with terrain elevation */
    // double depth = getDepth();
    // getFeature().setProperty( QNAME_PROP_DEPTH, depth );
  }

  public void setMidSide( final boolean isMidSide )
  {
    getFeature().setProperty( QNAME_PROP_MIDSIDE, isMidSide );
  }

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( GMLNodeResult.QNAME_PROP_LOCATION );
  }

  public List<Double> getVelocity( )
  {
    // return velocity only for wet nodes
    if( isWet() )
      return getVirtualVelocity();
    else
    {
      final List<Double> veloList = new ArrayList<Double>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
      return veloList;
    }
  }

  @SuppressWarnings("unchecked")
  public List<Double> getVirtualVelocity( )
  {
    return (List<Double>) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VELOCITY );
  }

  public double getVirtualDepth( )
  {
    return (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VIRTUALDEPTH );
  }

  public double getDepth( )
  {
    // this is a function property that returns the water level minus the elevation
    return (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_DEPTH );
  }

  public double getWaterlevel( )
  {
    return (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_WATERLEVEL );
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
    getFeature().setProperty( QNAME_PROP_VELOCITY, velocity );
  }

  public void setVelPrevStep( final List<Double> velPrevStep )
  {
    getFeature().setProperty( QNAME_PROP_VELPREVSTEP, velPrevStep );
  }

  public void setVelOverTime( final List<Double> velOverTime )
  {
    getFeature().setProperty( QNAME_PROP_VELOVERTIME, velOverTime );
  }

  public void setVelOverTimePrevStep( final List<Double> velOverTimePrevStep )
  {
    getFeature().setProperty( QNAME_PROP_VELOVERTIMEPREVSTEP, velOverTimePrevStep );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#isWet()
   */
  public boolean isWet( )
  {
    return getDepth() > 0;
  }

  public boolean isAssigned( )
  {
    return m_nodeAssigned;
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
      for( final double lambda : m_lambdas )
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
  public void setVirtualDepth( final double virtualDepth )
  {
    getFeature().setProperty( QNAME_PROP_VIRTUALDEPTH, new Double( virtualDepth ) );
  }

  public void setVirtDepPrevStep( final double virtDepthPrevStep )
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPPREVSTEP, new Double( virtDepthPrevStep ) );
  }

  public void setVirtDepOverTime( final double virtDepOverTime )
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPOVERTIME, new Double( virtDepOverTime ) );
  }

  public void setVirtDepOverTimePrevStep( final double virtDepOverTimePrevStep )
  {
    getFeature().setProperty( QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP, new Double( virtDepOverTimePrevStep ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getDischarge()
   */
  public Double getDischarge( )
  {
    return (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_DISCHARGE );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setDischarge(double)
   */
  public void setDischarge( double discharge )
  {
    getFeature().setProperty( GMLNodeResult.QNAME_PROP_DISCHARGE, new Double( discharge ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVelOverTime()
   */
  public List<Double> getVelOverTime( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = (List<Double>) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VELOVERTIME );

    if( veloList == null )
    {
      veloList = new ArrayList<Double>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }

    return veloList;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVelOverTimePrevStep()
   */
  public List<Double> getVelOverTimePrevStep( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = (List<Double>) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VELOVERTIMEPREVSTEP );

    if( veloList == null )
    {
      veloList = new ArrayList<Double>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }

    return veloList;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVelPrevStep()
   */
  public List<Double> getVelPrevStep( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = (List<Double>) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VELPREVSTEP );

    if( veloList == null )
    {
      veloList = new ArrayList<Double>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }

    return veloList;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepOverTime()
   */
  public double getVirtDepOverTime( )
  {
    Double VirtualDepth = (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPOVERTIME );
    if( VirtualDepth == null )
      VirtualDepth = 0.0;
    return VirtualDepth;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepOverTimePrevStep()
   */
  public double getVirtDepOverTimePrevStep( )
  {
    Double VirtDepOverTimePrevStep = (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP );
    if( VirtDepOverTimePrevStep == null )
      VirtDepOverTimePrevStep = 0.0;
    return VirtDepOverTimePrevStep;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepPrevStep()
   */
  public double getVirtDepPrevStep( )
  {
    Double VirtDepPrevStep = (Double) getFeature().getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPPREVSTEP );
    if( VirtDepPrevStep == null )
      VirtDepPrevStep = 0.0;
    return VirtDepPrevStep;
  }
}
