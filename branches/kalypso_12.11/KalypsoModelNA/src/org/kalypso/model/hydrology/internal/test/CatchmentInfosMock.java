/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.hydrology.internal.test;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentIndex;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Simple implementation for testing; does not dissolve hydrotopes.
 * 
 * @author Gernot Belger
 */
class CatchmentInfosMock implements ICatchmentInfos
{
  private final Map<Catchment, CatchmentInfo> m_infos = new HashMap<>();

  private final Catchment[] m_catchments;

  public CatchmentInfosMock( final ParameterHash landuseHash, final NaModell model, final HydrotopeCollection naHydrotop ) throws NAPreprocessorException
  {
    m_catchments = addHydrotopes( landuseHash, model, naHydrotop );
  }

  private Catchment[] addHydrotopes( final ParameterHash landuseHash, final NaModell model, final HydrotopeCollection hydrotopeCollection ) throws NAPreprocessorException
  {
    /* index for catchments */
    final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
    final Catchment[] originalCatchments = catchments.toArray( new Catchment[catchments.size()] );

    final CatchmentIndex catchmentIndex = new CatchmentIndex( originalCatchments );

    final IFeatureBindingCollection<IHydrotope> hydrotopes = hydrotopeCollection.getHydrotopes();
    for( final IHydrotope hydrotope : hydrotopes )
    {
      final Catchment originalCatchment = catchmentIndex.findCatchment( hydrotope );

      /* create and validate hydrotope info */
      final HydrotopeInfo hydrotopeInfo = new HydrotopeInfo( hydrotope, landuseHash );
      hydrotopeInfo.validateAttributes();

      final CatchmentInfo info = getOrCreateInfo( originalCatchment );
      info.add( hydrotopeInfo );
    }

    return originalCatchments;
  }

  private CatchmentInfo getOrCreateInfo( final Catchment catchment )
  {
    final CatchmentInfo info = m_infos.get( catchment );
    if( info != null )
      return info;

    final String label = catchment.getName();
    final String lzsID = catchment.getId();
    final CatchmentInfo newInfo = new CatchmentInfo( catchment, label, lzsID );
    m_infos.put( catchment, newInfo );

    return newInfo;
  }

  @Override
  public Catchment[] getCatchments( )
  {
    return m_catchments;
  }

  @Override
  public CatchmentInfo getInfo( final Catchment catchment )
  {
    return m_infos.get( catchment );
  }
}