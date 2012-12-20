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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.valid.IsValidOp;
import com.vividsolutions.jts.operation.valid.TopologyValidationError;

/**
 * @author Gernot Belger
 */
class Validate2dImportOperation implements ICoreRunnableWithProgress
{
  private final IPolygonWithName[] m_elements;

  public Validate2dImportOperation( final IPolygonWithName[] elements )
  {
    m_elements = elements;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "Validate2dImportOperation_0" ), m_elements.length ); //$NON-NLS-1$

    for( final IPolygonWithName element : m_elements )
    {
      final IStatus status = validateElement( element );
      element.setStatus( status );

      ProgressUtilities.worked( monitor, 1 );
    }

    monitor.done();

    return Status.OK_STATUS;
  }

  private IStatus validateElement( final IPolygonWithName element )
  {
    final IStatusCollector log = new StatusCollector( Kalypso1d2dProjectPlugin.PLUGIN_ID );

    final Polygon polygon = element.getPolygon();

    /* jts valid ? */
    final IsValidOp isValidOp = new IsValidOp( polygon );
    if( !isValidOp.isValid() )
    {
      final TopologyValidationError error = isValidOp.getValidationError();
      final String jtsMessage = error.getMessage();

      final String message = String.format( Messages.getString( "Validate2dImportOperation_1" ), jtsMessage ); //$NON-NLS-1$

      log.add( IStatus.ERROR, message );
    }

    /* interior rings? */
    if( polygon.getNumInteriorRing() > 0 )
      log.add( IStatus.WARNING, Messages.getString( "Validate2dImportOperation_2" ) ); //$NON-NLS-1$

    /* number of vertices */
    if( polygon.getNumPoints() > 5 )
      log.add( IStatus.WARNING, Messages.getString( "Validate2dImportOperation_3" ) ); //$NON-NLS-1$

    if( log.size() == 1 )
      return log.getAllStati()[0];

    return log.asMultiStatusOrOK( Messages.getString( "Validate2dImportOperation_4" ) ); //$NON-NLS-1$
  }
}
