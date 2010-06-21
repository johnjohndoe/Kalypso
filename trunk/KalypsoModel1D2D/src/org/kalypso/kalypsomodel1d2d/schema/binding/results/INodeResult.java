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

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.conv.results.ArcResult;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author jung
 * 
 */
public interface INodeResult extends IFeatureWrapper2
{
  public final static QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResult" ); //$NON-NLS-1$

  public abstract void setCalcId( final int id );

  public abstract void setLocation( final double x, final double y, final double z, final String crs );

  public abstract void setResultValues( final double vx, final double vy, final double virtualDepth, final double waterlevel );

  public abstract void setMidSide( final boolean isMidSide );

  public abstract GM_Point getPoint( );

  public abstract void setWaterlevel( final double waterlevel );

  public abstract void setDepth( final double depth );

  public abstract void setVirtualDepth( final double virtualDepth );

  public double getDepth( );

  public double getVirtualDepth( );

  public double getWaterlevel( );
  
  public abstract void setWaveHsig( final double hsig );
  
  public abstract void setWavePeriod( final double period );
  
  public abstract void setWaveDirection( final double direction );
  
  public double getWaveHsig( );
  
  public double getWavePeriod( );
  
  public double getWaveDirection( );

  public boolean isWet( );

  public abstract void setVelocity( List<Double> velocity );

  /**
   * returns a velocity value even if depth > 0, else (0.0,0.0)
   */
  public abstract List<Double> getVelocity( );

  /**
   * could return a velocity value even if depth < 0
   */
  public abstract List<Double> getVirtualVelocity( );

  public abstract double getAbsoluteVelocity( );

  public abstract void addLambda( final double lambda );

  public abstract double getAveragedLambda( );

  public abstract void setDry( final int dry );

  public int getDry( );

  public abstract Double getDischarge( );

  public abstract void setDischarge( final double discharge );

  List<ArcResult> getArcs( );

  void setArc( ArcResult arc );

  int getNodeID( );

  boolean isAssigned( );

  void setAssigned( boolean assign );

  // setter-methods for time dependent variables at unsteady restart
  public abstract void setTimeDerivativeValues( final double vxWRTt, final double vyWRTt, final double virtDepWRTt );

  public abstract void setResultPrevStepValues( final double vxPrevStep, final double vyPrevStep, final double virtDepPrevStep );

  public abstract void setTimeDerivativeValuesPrevStep( final double vxWRTtPrevStep, final double vyWRTtPrevStep, final double virtDepWRTtPrevStep );

  public abstract void setVirtDepPrevStep( final double virtDepthPrevStep );

  public abstract void setVirtDepOverTime( final double virtDepOverTime );

  public abstract void setVirtDepOverTimePrevStep( final double virtDepOverTimePrevStep );

  public abstract void setVelPrevStep( List<Double> velPrevStep );

  public abstract void setVelOverTime( List<Double> velOverTime );

  public abstract void setVelOverTimePrevStep( List<Double> velOverTimePrevStep );

  // getter-methods for time dependent variables at unsteady restart
  public double getVirtDepPrevStep( );

  public double getVirtDepOverTime( );

  public double getVirtDepOverTimePrevStep( );

  public List<Double> getVelPrevStep( );

  public List<Double> getVelOverTime( );

  public List<Double> getVelOverTimePrevStep( );

}