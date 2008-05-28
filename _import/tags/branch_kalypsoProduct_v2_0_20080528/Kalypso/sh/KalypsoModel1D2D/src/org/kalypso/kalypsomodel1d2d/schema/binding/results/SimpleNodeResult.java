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

import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.conv.results.ArcResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class SimpleNodeResult implements INodeResult
{
  private GM_Point m_point;

  private double m_vx;

  private double m_vy;

  private double m_depth;

  private double m_waterlevel;

  private List<Double> m_velocity;

  public final List<Double> m_lambdas = new LinkedList<Double>();

  public final List<ArcResult> m_arcs = new LinkedList<ArcResult>();

  private boolean m_nodeAssigned;

  public List<ArcResult> getArcs( )
  {
    return m_arcs;
  }

  public void setArc( final ArcResult arc )
  {
    m_arcs.add( arc );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getPoint()
   */
  public GM_Point getPoint( )
  {
    return m_point;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getWaterlevel()
   */
  public double getWaterlevel( )
  {
    return m_waterlevel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setCalcId(int)
   */
  public void setCalcId( final int id )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setLocation(double, double, double,
   *      java.lang.String)
   */
  public void setLocation( final double x, final double y, final double z, final String crs )
  {
    final GM_Position position = GeometryFactory.createGM_Position( x, y, z );
    m_point = GeometryFactory.createGM_Point( position, crs );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setMidSide(boolean)
   */
  public void setMidSide( final boolean isMidSide )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setResultValues(double, double, double,
   *      double)
   */
  public void setResultValues( final double vx, final double vy, final double depth, final double waterlevel )
  {
    m_vx = vx;
    m_vy = vy;
    m_depth = depth;
    m_waterlevel = waterlevel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setDepth(double)
   */
  public void setDepth( final double depth )
  {
    m_depth = depth;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setWaterlevel(double)
   */
  public void setWaterlevel( final double waterlevel )
  {
    m_waterlevel = waterlevel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getDepth()
   */
  public double getDepth( )
  {
    return m_depth;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setVelocity(java.util.List)
   */
  public void setVelocity( final List<Double> velocity )
  {
    m_velocity = velocity;
    m_vx = velocity.get( 0 );
    m_vy = velocity.get( 1 );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVelocity()
   */
  public List<Double> getVelocity( )
  {
    return m_velocity;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#isWet()
   */
  public boolean isWet( )
  {
    if( m_depth <= 0 )
      return false;
    else
      return true;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getAbsoluteVelocity()
   */
  public double getAbsoluteVelocity( )
  {
    return Math.sqrt( m_vx * m_vx + m_vy * m_vy );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getGmlID()
   */
  public String getGmlID( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getFeature()
   */
  public Feature getFeature( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#addLambda(double)
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
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getVirtualDepth()
   */
  public double getVirtualDepth( )
  {
    // no virtual depth in simple nodes
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setVirtualDepth(double)
   */
  public void setVirtualDepth( final double virtualDepth )
  {
    // no virtual depth in simple nodes
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getDry()
   */
  public int getDry( )
  {
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setDry(int)
   */
  public void setDry( final int dry )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getDischarge()
   */
  public Double getDischarge( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#setDischarge(double)
   */
  public void setDischarge( final double discharge )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult#getNodeID()
   */
  public int getNodeID( )
  {
    return -1;
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

}
