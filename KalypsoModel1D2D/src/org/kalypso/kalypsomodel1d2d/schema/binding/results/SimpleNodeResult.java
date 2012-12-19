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

import org.kalypso.kalypsomodel1d2d.conv.results.ArcResult;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.AbstractEmptyFeature;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class SimpleNodeResult extends AbstractEmptyFeature implements INodeResult
{
  private GM_Point m_point;

  private double m_vx;

  private double m_vy;

  private double m_vxPrevStep;

  private double m_vyPrevStep;

  private double m_dvxdt;

  private double m_dvydt;

  private double m_dvxdtPrevStep;

  private double m_dvydtPrevStep;

  private double m_waveHsig;

  private double m_waveDir;

  private double m_wavePer;

  private double m_depth;

  private double m_depthPrevStep;

  private double m_dDepthDt;

  private double m_dDepthDtPrevStep;

  private double m_waterlevel;

  private List<Double> m_velocity;

  private List<Double> m_velocityPrevStep;

  private List<Double> m_dVelDt;

  private List<Double> m_dVelDtPrevStep;

  public final List<Double> m_lambdas = new LinkedList<>();

  public final List<ArcResult> m_arcs = new LinkedList<>();

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
  public GM_Point getPoint( )
  {
    return m_point;
  }

  @Override
  public double getWaterlevel( )
  {
    return m_waterlevel;
  }

  @Override
  public void setCalcId( final int id )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void setLocation( final double x, final double y, final double z, final String crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    m_point = GeometryFactory.createGM_Point( position, crs );
  }

  @Override
  public void setMidSide( final boolean isMidSide )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void setResultValues( final double vx, final double vy, final double depth, final double waterlevel )
  {
    m_vx = vx;
    m_vy = vy;
    m_depth = depth;
    m_waterlevel = waterlevel;
  }

  @Override
  public void setResultPrevStepValues( final double vxPrevStep, final double vyPrevStep, final double virtDepPrevStep )
  {
    m_vxPrevStep = vxPrevStep;
    m_vyPrevStep = vyPrevStep;
    m_depthPrevStep = virtDepPrevStep;

  }

  @Override
  public void setTimeDerivativeValues( final double vxWRTt, final double vyWRTt, final double virtDepWRTt )
  {
    m_dvxdt = vxWRTt;
    m_dvydt = vyWRTt;
    m_dDepthDt = virtDepWRTt;

  }

  @Override
  public void setTimeDerivativeValuesPrevStep( final double vxWRTtPrevStep, final double vyWRTtPrevStep, final double virtDepWRTtPrevStep )
  {
    m_dvxdtPrevStep = vxWRTtPrevStep;
    m_dvydtPrevStep = vyWRTtPrevStep;
    m_dDepthDtPrevStep = virtDepWRTtPrevStep;

  }

  @Override
  public void setDepth( final double depth )
  {
    m_depth = depth;
  }

  @Override
  public void setWaterlevel( final double waterlevel )
  {
    m_waterlevel = waterlevel;
  }

  @Override
  public double getDepth( )
  {
    return getWaterlevel() - getPoint().getZ();
  }

  @Override
  public void setVelocity( final List<Double> velocity )
  {
    m_velocity = velocity;
    m_vx = velocity.get( 0 );
    m_vy = velocity.get( 1 );
  }

  @Override
  public List<Double> getVelocity( )
  {
    final double depth = getDepth();

    if( depth > 0 )
      return m_velocity;
    else
    {
      final List<Double> veloList = new ArrayList<>();
      veloList.add( 0.0 );
      veloList.add( 0.0 );
      return veloList;
    }
  }

  @Override
  public boolean isWet( )
  {
    if( m_depth <= 0 )
      return false;
    else
      return true;
  }

  @Override
  public double getAbsoluteVelocity( )
  {
    return Math.sqrt( m_vx * m_vx + m_vy * m_vy );
  }

  @Override
  public void addLambda( final double lambda )
  {
    m_lambdas.add( lambda );
  }

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

  @Override
  public double getVirtualDepth( )
  {
    return m_depth;
  }

  @Override
  public void setVirtualDepth( final double virtualDepth )
  {
    m_depth = virtualDepth;
  }

  @Override
  public int getDry( )
  {
    return 0;
  }

  @Override
  public void setDry( final int dry )
  {
  }

  @Override
  public Double getDischarge( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public void setDischarge( final double discharge )
  {
    // TODO Auto-generated method stub
  }

  @Override
  public int getNodeID( )
  {
    return -1;
  }

  @Override
  public boolean isAssigned( )
  {
    if( m_nodeAssigned == true )
      return true;
    else
      return false;
  }

  @Override
  public void setAssigned( final boolean assign )
  {
    m_nodeAssigned = assign;
  }

  @Override
  public List<Double> getVirtualVelocity( )
  {
    return m_velocity;
  }

  // Why are the following functions needed at all?

  @Override
  public void setVelOverTime( final List<Double> velOverTime )
  {
    m_dVelDt = velOverTime;
    m_dvxdt = velOverTime.get( 0 );
    m_dvydt = velOverTime.get( 1 );

  }

  @Override
  public void setVelOverTimePrevStep( final List<Double> velOverTimePrevStep )
  {
    m_dVelDtPrevStep = velOverTimePrevStep;
    m_dvxdtPrevStep = velOverTimePrevStep.get( 0 );
    m_dvydtPrevStep = velOverTimePrevStep.get( 1 );
  }

  @Override
  public void setVelPrevStep( final List<Double> velPrevStep )
  {
    m_velocityPrevStep = velPrevStep;
    m_vxPrevStep = velPrevStep.get( 0 );
    m_vyPrevStep = velPrevStep.get( 1 );
  }

  @Override
  public void setVirtDepOverTime( final double virtDepOverTime )
  {
    m_dDepthDt = virtDepOverTime;
  }

  @Override
  public void setVirtDepOverTimePrevStep( final double virtDepOverTimePrevStep )
  {
    m_dDepthDtPrevStep = virtDepOverTimePrevStep;
  }

  @Override
  public void setVirtDepPrevStep( final double virtDepthPrevStep )
  {
    m_depthPrevStep = virtDepthPrevStep;
  }

  @Override
  public List<Double> getVelOverTime( )
  {
    return m_dVelDt;
  }

  @Override
  public List<Double> getVelOverTimePrevStep( )
  {
    return m_dVelDtPrevStep;
  }

  @Override
  public List<Double> getVelPrevStep( )
  {
    return m_velocityPrevStep;
  }

  @Override
  public double getVirtDepOverTime( )
  {
    return m_dDepthDt;
  }

  @Override
  public double getVirtDepOverTimePrevStep( )
  {
    return m_dDepthDtPrevStep;
  }

  @Override
  public double getVirtDepPrevStep( )
  {
    return m_depthPrevStep;
  }

  @Override
  public double getWaveDirection( )
  {
    return m_waveDir;
  }

  @Override
  public double getWaveHsig( )
  {
    return m_waveHsig;
  }

  @Override
  public double getWavePeriod( )
  {
    return m_wavePer;
  }

  @Override
  public void setWaveDirection( final double direction )
  {
    m_waveDir = direction;
  }

  @Override
  public void setWaveHsig( final double hsig )
  {
    m_waveHsig = hsig;
  }

  @Override
  public void setWavePeriod( final double period )
  {
    m_wavePer = period;
  }

  public final double getVxPrevStep( )
  {
    return m_vxPrevStep;
  }

  public final void setVxPrevStep( final double vxPrevStep )
  {
    m_vxPrevStep = vxPrevStep;
  }

  public final double getVyPrevStep( )
  {
    return m_vyPrevStep;
  }

  public final void setVyPrevStep( final double vyPrevStep )
  {
    m_vyPrevStep = vyPrevStep;
  }

  public final double getDvxdt( )
  {
    return m_dvxdt;
  }

  public final void setDvxdt( final double dvxdt )
  {
    m_dvxdt = dvxdt;
  }

  public final double getDvydt( )
  {
    return m_dvydt;
  }

  public final void setDvydt( final double dvydt )
  {
    m_dvydt = dvydt;
  }

  public final double getDvxdtPrevStep( )
  {
    return m_dvxdtPrevStep;
  }

  public final void setDvxdtPrevStep( final double dvxdtPrevStep )
  {
    m_dvxdtPrevStep = dvxdtPrevStep;
  }

  public final double getDvydtPrevStep( )
  {
    return m_dvydtPrevStep;
  }

  public final void setDvydtPrevStep( final double dvydtPrevStep )
  {
    m_dvydtPrevStep = dvydtPrevStep;
  }
}
