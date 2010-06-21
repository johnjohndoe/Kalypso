/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.sim;

import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;

/**
 * @author Thomas Jung
 * 
 */
public class NodeResultMinMaxCatcher
{
  double m_minDepth = Double.POSITIVE_INFINITY;

  double m_maxDepth = Double.NEGATIVE_INFINITY;

  double m_minVelocityAbs = Double.POSITIVE_INFINITY;

  double m_maxVelocityAbs = Double.NEGATIVE_INFINITY;

  double m_minWaterlevel = Double.POSITIVE_INFINITY;

  double m_maxWaterlevel = Double.NEGATIVE_INFINITY;

  private double m_minShearStress = Double.POSITIVE_INFINITY;

  private double m_maxShearStress = Double.NEGATIVE_INFINITY;

  private double m_minTerrain = Double.POSITIVE_INFINITY;

  private double m_maxTerrain = Double.NEGATIVE_INFINITY;

  double m_minWaveHsig = Double.POSITIVE_INFINITY;

  double m_maxWaveHsig = Double.NEGATIVE_INFINITY;

  double m_minWavePer = Double.POSITIVE_INFINITY;

  double m_maxWavePer = Double.NEGATIVE_INFINITY;

  public void addNodeResult( final INodeResult nodeResult )
  {
    try
    {
      final double velocity = nodeResult.getAbsoluteVelocity();
      final double depth = nodeResult.getDepth();
      final double waterlevel = nodeResult.getWaterlevel();
      final double Terrain = nodeResult.getPoint().getZ();
      final double waveHsig = nodeResult.getWaveHsig();
      final double wavePer = nodeResult.getWavePeriod();

      // TODO: implement the shear stress
      final double shearstress = 0;

      if( velocity < m_minVelocityAbs )
        m_minVelocityAbs = velocity;
      if( velocity > m_maxVelocityAbs )
        m_maxVelocityAbs = velocity;

      if( depth < m_minDepth )
        m_minDepth = depth;
      if( depth > m_maxDepth )
        m_maxDepth = depth;

      if( waterlevel < m_minWaterlevel )
        m_minWaterlevel = waterlevel;
      if( waterlevel > m_maxWaterlevel )
        m_maxWaterlevel = waterlevel;

      if( shearstress < m_minShearStress )
        m_minShearStress = shearstress;
      if( shearstress > m_maxShearStress )
        m_maxShearStress = shearstress;

      if( Terrain < m_minTerrain )
        m_minTerrain = Terrain;
      if( Terrain > m_maxTerrain )
        m_maxTerrain = Terrain;

      if( waveHsig > m_maxWaveHsig )
        m_maxWaveHsig = waveHsig;
      if( waveHsig < m_minWaveHsig )
        m_minWaveHsig = waveHsig;

      if( wavePer > m_maxWavePer )
        m_maxWavePer = wavePer;
      if( wavePer < m_minWavePer )
        m_minWavePer = wavePer;
    }
    catch( Exception e )
    {
    }

  }

  public void addNodeResultMinMaxCatcher( NodeResultMinMaxCatcher minMaxCatcher )
  {
    try
    {
      if( minMaxCatcher.getMaxDepth() > m_maxDepth )
        m_maxDepth = minMaxCatcher.getMaxDepth();
      if( minMaxCatcher.getMinDepth() < m_minDepth )
        m_minDepth = minMaxCatcher.getMinDepth();

      if( minMaxCatcher.getMaxWaterlevel() > m_maxWaterlevel )
        m_maxWaterlevel = minMaxCatcher.getMaxWaterlevel();
      if( minMaxCatcher.getMinWaterlevel() < m_minWaterlevel )
        m_minWaterlevel = minMaxCatcher.getMinWaterlevel();

      if( minMaxCatcher.getMaxVelocityAbs() > m_maxVelocityAbs )
        m_maxVelocityAbs = minMaxCatcher.getMaxVelocityAbs();
      if( minMaxCatcher.getMinVelocityAbs() < m_minVelocityAbs )
        m_minVelocityAbs = minMaxCatcher.getMinVelocityAbs();

      if( minMaxCatcher.getMaxVelocityAbs() > m_maxVelocityAbs )
        m_maxVelocityAbs = minMaxCatcher.getMaxVelocityAbs();
      if( minMaxCatcher.getMinVelocityAbs() < m_minVelocityAbs )
        m_minVelocityAbs = minMaxCatcher.getMinVelocityAbs();

      if( minMaxCatcher.getMaxShearStress() > m_maxShearStress )
        m_maxShearStress = minMaxCatcher.getMaxShearStress();
      if( minMaxCatcher.getMinShearStress() < m_minShearStress )
        m_minShearStress = minMaxCatcher.getMinShearStress();

      if( minMaxCatcher.getMaxTerrain() > m_maxTerrain )
        m_maxTerrain = minMaxCatcher.getMaxTerrain();
      if( minMaxCatcher.getMinTerrain() > m_minTerrain )
        m_minTerrain = minMaxCatcher.getMinTerrain();

      if( minMaxCatcher.getMaxWaveHsig() > m_maxWaveHsig )
        m_maxWaveHsig = minMaxCatcher.getMaxWaveHsig();
      if( minMaxCatcher.getMinWaveHsig() < m_minWaveHsig )
        m_minWaveHsig = minMaxCatcher.getMinWaveHsig();

      if( minMaxCatcher.getMaxWavePer() > m_maxWavePer )
        m_maxWavePer = minMaxCatcher.getMaxWavePer();
      if( minMaxCatcher.getMinWavePer() < m_minWavePer )
        m_minWavePer = minMaxCatcher.getMinWavePer();
    }
    catch( Exception e )
    {
    }
  }

  public double getMinTerrain( )
  {
    return m_minTerrain;
  }

  public double getMaxTerrain( )
  {
    return m_maxTerrain;
  }

  public double getMinDepth( )
  {
    return m_minDepth;
  }

  public double getMaxDepth( )
  {
    return m_maxDepth;
  }

  public double getMinVelocityAbs( )
  {
    return m_minVelocityAbs;
  }

  public double getMaxVelocityAbs( )
  {
    return m_maxVelocityAbs;
  }

  public double getMinWaterlevel( )
  {
    return m_minWaterlevel;
  }

  public double getMaxWaterlevel( )
  {
    return m_maxWaterlevel;
  }

  public double getMinShearStress( )
  {
    return m_minShearStress;
  }

  public double getMaxShearStress( )
  {
    return m_maxShearStress;
  }

  public final double getMinWaveHsig( )
  {
    return m_minWaveHsig;
  }

  public final double getMinWavePer( )
  {
    return m_minWavePer;
  }

  public final double getMaxWaveHsig( )
  {
    return m_maxWaveHsig;
  }

  public final double getMaxWavePer( )
  {
    return m_maxWavePer;
  }

}
