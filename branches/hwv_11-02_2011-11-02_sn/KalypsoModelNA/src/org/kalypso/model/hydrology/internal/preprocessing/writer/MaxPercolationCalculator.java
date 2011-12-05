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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypso.model.hydrology.binding.suds.ISwaleInfiltrationDitch;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class MaxPercolationCalculator
{
  private final Map<ISwaleInfiltrationDitch, List<Double>> m_suds2HydrotopMaxPercRateMap = new HashMap<ISwaleInfiltrationDitch, List<Double>>();

  private final NAHydrotop m_naHydrotope;

  public MaxPercolationCalculator( final NAHydrotop naHydrotope )
  {
    m_naHydrotope = naHydrotope;

    buildData();
  }

  private void buildData( )
  {
    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_naHydrotope.getHydrotopes();
    for( final IHydrotope hydrotop : hydrotopes )
    {
      final IFeatureBindingCollection<ISuds> sudCollection = hydrotop.getSudCollection();
      for( final ISuds suds : sudCollection )
      {
        if( suds instanceof ISwaleInfiltrationDitch )
          addSuds( hydrotop, (ISwaleInfiltrationDitch) suds );
      }
    }
  }

  private void addSuds( final IHydrotope hydrotop, final ISwaleInfiltrationDitch suds )
  {
    final double maxPerkolationRate = hydrotop.getMaxPerkolationRate();
    if( Double.isNaN( maxPerkolationRate ) )
      return;

    final List<Double> list = getList( suds );
    list.add( maxPerkolationRate );
  }

  private List<Double> getList( final ISwaleInfiltrationDitch suds )
  {
    final List<Double> list = m_suds2HydrotopMaxPercRateMap.get( suds );
    if( list != null )
      return list;

    final List<Double> newlist = new ArrayList<Double>();
    m_suds2HydrotopMaxPercRateMap.put( suds, newlist );
    return newlist;
  }

  private double calculateAverageMaxPercRate( final ISuds suds )
  {
    final List<Double> list = m_suds2HydrotopMaxPercRateMap.get( suds );
    if( list == null || list.size() == 0 )
      return Double.NaN;

    double average = 0.0;
    for( final double value : list )
      average += value;
    return average / list.size();
  }

  public double getSudsAverageMaxPercRate( final ISwaleInfiltrationDitch ditch )
  {
    final double maxPercRate = calculateAverageMaxPercRate( ditch );
    if( Double.isNaN( maxPercRate ) )
      return ditch.getMaxPercRate();

    return maxPercRate;
  }

}
