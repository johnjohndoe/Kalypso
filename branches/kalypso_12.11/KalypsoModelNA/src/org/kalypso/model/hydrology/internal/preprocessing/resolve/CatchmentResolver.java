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
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;

/**
 * Resolves catchments that contain drwbm soil types. For each distinguished dwwbm soil type, a new catchment is created.
 * 
 * @author Gernot Belger
 */
class CatchmentResolver
{
  private final NaModell m_model;

  private final ParameterHash m_landuseHash;

  private final HydrotopeCollection m_hydrotopes;

  private ICatchmentInfos m_resolvedCatchmentData;

  public CatchmentResolver( final NaModell model, final ParameterHash landuseHash, final HydrotopeCollection hydrotopes )
  {
    m_model = model;
    m_landuseHash = landuseHash;
    m_hydrotopes = hydrotopes;
  }

  public ICatchmentInfos getResolvedCatchments( )
  {
    return m_resolvedCatchmentData;
  }

  public IStatus execute( ) throws NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    m_resolvedCatchmentData = buildCatchmentData( log );

    return log.asMultiStatus( "Resolving catchments" ); //$NON-NLS-1$
  }

  private ICatchmentInfos buildCatchmentData( final IStatusCollector log ) throws NAPreprocessorException
  {
    /* first, dissolve hydrotopes including drwbm */
    final CatchmentDissolver dissolver = new CatchmentDissolver( m_landuseHash, m_model );
    final IStatus status = dissolver.addHydrotopes( m_hydrotopes );
    if( !status.isOK() )
      log.add( status );

    /* helper for finding incoming groundwater releations */
    final CatchmentGroundwaterFinder relationFinder = new CatchmentGroundwaterFinder( m_model );

    /* create new catchments and also create info objects */
    final CatchmentInfos infos = new CatchmentInfos();

    final Catchment[] catchments = dissolver.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final CatchmentResolverWorker worker = new CatchmentResolverWorker( m_model, catchment, dissolver, relationFinder, infos );
      worker.execute();
    }

    return infos;
  }
}