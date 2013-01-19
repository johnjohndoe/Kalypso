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
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Holds information about the catchments derived from the hydrotopes.
 * 
 * @author Gernot Belger
 */
public class NaCatchmentData
{
  private final Map<Catchment, CatchmentInfo> m_catchmentInfos = new HashMap<>();

  private final ParameterHash m_landuseHash;

  public NaCatchmentData( final ParameterHash landuseHash )
  {
    m_landuseHash = landuseHash;
  }

  public IStatus addHydrotopes( final NaModell naModel, final HydrotopeCollection hydrotopeCollection, final boolean doAttributeDissolve ) throws NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    /* index for catchments */
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    final Catchment[] originalCatchments = catchments.toArray( new Catchment[catchments.size()] );

    final CatchmentIndex catchmentIndex = new CatchmentIndex( originalCatchments );

    final IFeatureBindingCollection<IHydrotope> hydrotopes = hydrotopeCollection.getHydrotopes();
    for( final IHydrotope hydrotope : hydrotopes )
    {
      final Catchment originalCatchment = catchmentIndex.findCatchment( hydrotope );
      if( originalCatchment == null )
      {
        final String message = String.format( Messages.getString( "NaCatchmentData_0" ) ); //$NON-NLS-1$
        log.add( IStatus.WARNING, message );
      }

      /* create and validate hydrotope info */
      final HydrotopeInfo hydrotopeInfo = new HydrotopeInfo( hydrotope, m_landuseHash );
      hydrotopeInfo.validateAttributes();

      // FIXME: split by drwbms

      final CatchmentInfo info = getOrCreateInfo( originalCatchment, doAttributeDissolve );
      info.add( hydrotopeInfo );
    }

    return log.asMultiStatus( Messages.getString( "NaCatchmentData_1" ) ); //$NON-NLS-1$
  }

  private CatchmentInfo getOrCreateInfo( final Catchment catchment, final boolean doAttributeDissolve )
  {
    final CatchmentInfo info = m_catchmentInfos.get( catchment );
    if( info != null )
      return info;

    final CatchmentInfo newInfo = new CatchmentInfo( catchment, doAttributeDissolve );
    m_catchmentInfos.put( catchment, newInfo );
    return newInfo;
  }

  public CatchmentInfo getInfo( final Catchment catchment )
  {
    return m_catchmentInfos.get( catchment );
  }

  // FIXME: collect all catchments and 'virtual' catchments
  public Catchment[] getCatchments( )
  {
    return m_catchmentInfos.keySet().toArray( new Catchment[m_catchmentInfos.size()] );
  }

  public List<HydrotopeInfo> getHydrotops( final Catchment catchment )
  {
    final CatchmentInfo catchmentInfo = getInfo( catchment );
    return catchmentInfo.getHydrotops();
  }

  public String getHydroFeatureId( final Catchment catchment, final int pos )
  {
    final CatchmentInfo catchmentInfo = getInfo( catchment );
    return catchmentInfo.getHydroFeatureId( pos );
  }

  public void addInfo( final CatchmentInfo info )
  {
    m_catchmentInfos.put( info.getCatchment(), info );
  }
}