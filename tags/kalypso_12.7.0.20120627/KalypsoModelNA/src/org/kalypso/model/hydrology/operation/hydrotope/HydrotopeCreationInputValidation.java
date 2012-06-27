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
package org.kalypso.model.hydrology.operation.hydrotope;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

/**
 * @author Gernot Belger
 */
class HydrotopeCreationInputValidation implements ICoreRunnableWithProgress
{
  private final IHydrotopeInput[] m_input;

  private final String m_logLabel;

  public HydrotopeCreationInputValidation( final IHydrotopeInput[] input, final String logLabel )
  {
    m_input = input;
    m_logLabel = logLabel;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String taskName = Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.1" ); //$NON-NLS-1$

    final SubMonitor progress = SubMonitor.convert( monitor, taskName, 100 );

    for( int i = 0; i < m_input.length; i++ )
    {
      final IHydrotopeInput input = m_input[i];

      final String subTask = String.format( Messages.getString("HydrotopeCreationInputValidation.0"), i + 1, m_input.length, input.getLabel() ); //$NON-NLS-1$
      progress.subTask( subTask );

      final IStatus status = validateIndex( input, subTask, progress.newChild( 1 ) );
      log.add( status );
    }

    return log.asMultiStatus( m_logLabel );
  }

  private static IStatus validateIndex( final IHydrotopeInput input, final String logLabel, final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    /* run checks */
    final SpatialIndexExt index = input.getIndex();
    final String label = input.getLabel();
    final HydrotopeCreationGeometryValidator validator = new HydrotopeCreationGeometryValidator( label, index );

    /* General geometric validation */
    log.add( validator.checkGeometryCorrectness( progress.newChild( 33 ) ) );

    log.add( validator.checkSelfIntersection( progress.newChild( 33 ) ) );

    /* special type dependent validation */
    log.add( input.validateInput() );
    ProgressUtilities.worked( monitor, 34 );

    return log.asMultiStatus( logLabel );
  }
}