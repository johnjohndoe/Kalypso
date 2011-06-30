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

import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author ig
 */
public class WindModel extends VersionedModel implements IWindModel
{
  private final IFeatureWrapperCollection< IWindDataModelSystem > m_windSystems;
  
  public WindModel( final Feature featureToBind )
  {
    super( featureToBind, QNAME_WIND_MODEL );
    m_windSystems = 
      new FeatureWrapperCollection<IWindDataModelSystem>( featureToBind, IWindDataModelSystem.class, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_WIND_ELE_SYS );
  }

  public IFeatureWrapperCollection<IWindDataModelSystem> getWindDataModelSystems( )
  {
    return m_windSystems;
//    final Feature feature = (Feature) getFeature().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_WIND_ELE_SYS );
    
//    final Object[] features = getFeature().getProperties();
//
//    if( features == null )
//    {
//      return null;
//    }
//    else
//    {
//      IFeatureWrapperCollection< IWindDataModelSystem > lWindSystems = new FeatureWrapperCollection<IWindDataModelSystem>();
//      return (IFeatureWrapperCollection<IWindDataModelSystem>) feature.getAdapter( IWindDataModelSystem.class );
//    }
  }

}
