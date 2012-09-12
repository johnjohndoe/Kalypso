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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.conv.results.ArcResult;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class GMLNodeResult extends Feature_Impl implements INodeResult
{
  public GMLNodeResult( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

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

  private static final QName QNAME_PROP_WAVE_HSIG = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "waveHsig" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_WAVE_DIR = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "waveDir" ); //$NON-NLS-1$

  private static final QName QNAME_PROP_WAVE_PER = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "wavePer" ); //$NON-NLS-1$

  public final List<ArcResult> m_arcs = new LinkedList<>();

  public final List<Double> m_lambdas = new LinkedList<>();

  private boolean m_nodeAssigned;

  @Override
  public List<ArcResult> getArcs( )
  {
    return m_arcs;
  }

  @Override
  public void setArc( final ArcResult arc )
  {
    m_arcs.add( arc );
  }

  @Override
  public void setCalcId( final int id )
  {
    setProperty( QNAME_PROP_CALCID, new Integer( id ) );
  }

  @Override
  public void setDry( final int dry )
  {
    setProperty( QNAME_PROP_DRY, new Integer( dry ) );
  }

  @Override
  public void setDepth( final double depth )
  {
    // depth is a function property, it will not be set anyway
    // TODO: remove this method
    // setProperty( QNAME_PROP_DEPTH, new Double( depth ) );
  }

  @Override
  public void setWaterlevel( final double waterlevel )
  {
    setProperty( QNAME_PROP_WATERLEVEL, new Double( waterlevel ) );
  }

  @Override
  public void setLocation( final double x, final double y, final double z, final String crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    final GM_Point point = GeometryFactory.createGM_Point( position, crs );

    setProperty( QNAME_PROP_LOCATION, point );
  }

  @Override
  public void setResultValues( final double vx, final double vy, final double virtualDepth, final double waterlevel )
  {
    setVirtualDepth( virtualDepth );
    setWaterlevel( waterlevel );

    final List<Double> veloList = new ArrayList<>();
    veloList.clear();
    veloList.add( vx );
    veloList.add( vy );
    setVelocity( veloList );
  }

  @Override
  public void setTimeDerivativeValues( final double vxWRTt, final double vyWRTt, final double virtDepWRTt )
  // WRT means with respect to
  {
    setProperty( QNAME_PROP_VIRTDEPOVERTIME, virtDepWRTt );

    final List<Double> veloList = new ArrayList<>();
    veloList.clear();
    veloList.add( vxWRTt );
    veloList.add( vyWRTt );
    setProperty( QNAME_PROP_VELOVERTIME, veloList );

  }

  @Override
  public void setResultPrevStepValues( final double vxPrevStep, final double vyPrevStep, final double virtDepPrevStep )
  {
    setProperty( QNAME_PROP_VIRTDEPPREVSTEP, virtDepPrevStep );

    final List<Double> velPrevStepList = new ArrayList<>();
    velPrevStepList.clear();
    velPrevStepList.add( vxPrevStep );
    velPrevStepList.add( vyPrevStep );
    setProperty( QNAME_PROP_VELPREVSTEP, velPrevStepList );
  }

  @Override
  public void setTimeDerivativeValuesPrevStep( final double vxWRTtPrevStep, final double vyWRTtPrevStep, final double virtDepWRTtPrevStep )
  // WRT means with respect to
  {
    setProperty( QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP, virtDepWRTtPrevStep );

    final List<Double> veloList = new ArrayList<>();
    veloList.clear();
    veloList.add( vxWRTtPrevStep );
    veloList.add( vyWRTtPrevStep );
    setProperty( QNAME_PROP_VELOVERTIMEPREVSTEP, veloList );

    /* check the real depth by comparing water level with terrain elevation */
    // double depth = getDepth();
    // setProperty( QNAME_PROP_DEPTH, depth );
  }

  @Override
  public void setMidSide( final boolean isMidSide )
  {
    setProperty( QNAME_PROP_MIDSIDE, isMidSide );
  }

  @Override
  public GM_Point getPoint( )
  {
    return (GM_Point) getProperty( GMLNodeResult.QNAME_PROP_LOCATION );
  }

  @Override
  public List<Double> getVelocity( )
  {
    // return velocity only for wet nodes
    if( isWet() )
      return getVirtualVelocity();
    else
    {
      final List<Double> veloList = new ArrayList<>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
      return veloList;
    }
  }

  @Override
  public List<Double> getVirtualVelocity( )
  {
    return (List<Double>) getProperty( GMLNodeResult.QNAME_PROP_VELOCITY );
  }

  @Override
  public double getVirtualDepth( )
  {
    final Double virtDepth = (Double) getProperty( GMLNodeResult.QNAME_PROP_VIRTUALDEPTH );
    if( virtDepth == null )
      return 0.0;
    return virtDepth;
  }

  @Override
  public double getDepth( )
  {
    // this is a function property that returns the water level minus the elevation
    return (Double) getProperty( GMLNodeResult.QNAME_PROP_DEPTH );
  }

  @Override
  public double getWaterlevel( )
  {
    final Double waterlevel = (Double) getProperty( GMLNodeResult.QNAME_PROP_WATERLEVEL );
    if( waterlevel == null )
      return Double.NaN;
    return waterlevel;
  }

  @Override
  public int getDry( )
  {
    return (Integer) getProperty( GMLNodeResult.QNAME_PROP_DRY );
  }

  @Override
  public int getNodeID( )
  {
    return (Integer) getProperty( GMLNodeResult.QNAME_PROP_CALCID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setVelocity(java.util.List)
   */
  @Override
  public void setVelocity( final List<Double> velocity )
  {
    setProperty( QNAME_PROP_VELOCITY, velocity );
  }

  @Override
  public void setVelPrevStep( final List<Double> velPrevStep )
  {
    setProperty( QNAME_PROP_VELPREVSTEP, velPrevStep );
  }

  @Override
  public void setVelOverTime( final List<Double> velOverTime )
  {
    setProperty( QNAME_PROP_VELOVERTIME, velOverTime );
  }

  @Override
  public void setVelOverTimePrevStep( final List<Double> velOverTimePrevStep )
  {
    setProperty( QNAME_PROP_VELOVERTIMEPREVSTEP, velOverTimePrevStep );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#isWet()
   */
  @Override
  public boolean isWet( )
  {
    return getDepth() > 0;
  }

  @Override
  public boolean isAssigned( )
  {
    return m_nodeAssigned;
  }

  @Override
  public void setAssigned( final boolean assign )
  {
    m_nodeAssigned = assign;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getAbsoluteVelocity()
   */
  @Override
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
  @Override
  public void addLambda( final double lambda )
  {
    m_lambdas.add( lambda );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getAveragedLambda()
   */
  @Override
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
  @Override
  public void setVirtualDepth( final double virtualDepth )
  {
    setProperty( QNAME_PROP_VIRTUALDEPTH, new Double( virtualDepth ) );
  }

  @Override
  public void setVirtDepPrevStep( final double virtDepthPrevStep )
  {
    setProperty( QNAME_PROP_VIRTDEPPREVSTEP, new Double( virtDepthPrevStep ) );
  }

  @Override
  public void setVirtDepOverTime( final double virtDepOverTime )
  {
    setProperty( QNAME_PROP_VIRTDEPOVERTIME, new Double( virtDepOverTime ) );
  }

  @Override
  public void setVirtDepOverTimePrevStep( final double virtDepOverTimePrevStep )
  {
    setProperty( QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP, new Double( virtDepOverTimePrevStep ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getDischarge()
   */
  @Override
  public Double getDischarge( )
  {
    return (Double) getProperty( GMLNodeResult.QNAME_PROP_DISCHARGE );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setDischarge(double)
   */
  @Override
  public void setDischarge( final double discharge )
  {
    setProperty( GMLNodeResult.QNAME_PROP_DISCHARGE, new Double( discharge ) );
  }

  @Override
  public List<Double> getVelOverTime( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = null;
    try{
      veloList = (List<Double>) getProperty( GMLNodeResult.QNAME_PROP_VELOVERTIME );
    }
    catch (final Exception e) {
    }

    if( veloList == null )
    {
      veloList = new ArrayList<>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }
    else if( veloList.size() == 0 ){
      while( veloList.size() < 2 )
        veloList.add( 0.0 );
    }

    return veloList;
  }

  @Override
  public List<Double> getVelOverTimePrevStep( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = null;
    try{
      veloList = (List<Double>) getProperty( GMLNodeResult.QNAME_PROP_VELOVERTIMEPREVSTEP );
    }
    catch (final Exception e) {
    }

    if( veloList == null )
    {
      veloList = new ArrayList<>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }
    else if( veloList.size() == 0 ){
      while( veloList.size() < 2 )
        veloList.add( 0.0 );
    }

    return veloList;
  }

  @Override
  public List<Double> getVelPrevStep( )
  {
    // Try to get the velocity over time derivative
    List<Double> veloList = null;
    try{
      veloList = (List<Double>) getProperty( GMLNodeResult.QNAME_PROP_VELPREVSTEP );
    }
    catch (final Exception e) {
    }

    if( veloList == null )
    {
      veloList = new ArrayList<>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
    }
    else if( veloList.size() == 0 ){
      while( veloList.size() < 2 )
        veloList.add( 0.0 );
    }

    return veloList;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepOverTime()
   */
  @Override
  public double getVirtDepOverTime( )
  {
    Double VirtualDepth = (Double) getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPOVERTIME );
    if( VirtualDepth == null )
      VirtualDepth = 0.0;
    return VirtualDepth;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepOverTimePrevStep()
   */
  @Override
  public double getVirtDepOverTimePrevStep( )
  {
    Double VirtDepOverTimePrevStep = (Double) getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPOVERTIMEPREVSTEP );
    if( VirtDepOverTimePrevStep == null )
      VirtDepOverTimePrevStep = 0.0;
    return VirtDepOverTimePrevStep;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtDepPrevStep()
   */
  @Override
  public double getVirtDepPrevStep( )
  {
    Double VirtDepPrevStep = (Double) getProperty( GMLNodeResult.QNAME_PROP_VIRTDEPPREVSTEP );
    if( VirtDepPrevStep == null )
      VirtDepPrevStep = 0.0;
    return VirtDepPrevStep;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getWaveDirection()
   */
  @Override
  public double getWaveDirection( )
  {
    final Double waveDir = (Double) getProperty( GMLNodeResult.QNAME_PROP_WAVE_DIR );
    if( waveDir == null )
      return Double.NaN;
    return waveDir;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getWaveHsig()
   */
  @Override
  public double getWaveHsig( )
  {
    final Double waveHsig = (Double) getProperty( GMLNodeResult.QNAME_PROP_WAVE_HSIG );
    if( waveHsig == null )
      return Double.NaN;
    return waveHsig;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getWavePeriod()
   */
  @Override
  public double getWavePeriod( )
  {
    final Double wavePeriod = (Double) getProperty( GMLNodeResult.QNAME_PROP_WAVE_PER );
    if( wavePeriod == null )
      return Double.NaN;
    return wavePeriod;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setWaveDirection(double)
   */
  @Override
  public void setWaveDirection( final double direction )
  {
    setProperty( QNAME_PROP_WAVE_DIR, direction );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setWaveHsig(double)
   */
  @Override
  public void setWaveHsig( final double hsig )
  {
    setProperty( QNAME_PROP_WAVE_HSIG, hsig );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setWavePeriod(double)
   */
  @Override
  public void setWavePeriod( final double period )
  {
    setProperty( QNAME_PROP_WAVE_PER, period );
  }
}
