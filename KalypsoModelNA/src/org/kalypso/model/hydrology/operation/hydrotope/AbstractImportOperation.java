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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * @author Gernot Belger
 */
abstract class AbstractImportOperation implements ICoreRunnableWithProgress
{
  static final String STR_IMPORT_SUCCESSFULLY_TERMINATED = Messages.getString( "AbstractImportOperation.0" ); //$NON-NLS-1$

  static final String STR_PROBLEMS_DURING_IMPORT = Messages.getString( "AbstractImportOperation.1" ); //$NON-NLS-1$

  private final IStatusCollector m_log = new StatusCollector( ModelNA.PLUGIN_ID );

  private final InputDescriptor m_inputDescriptor;

  interface InputDescriptor
  {
    /** Number of elements contained in this descriptor. All other methods allow for indices in the range 0..size-1 */
    int size( ) throws CoreException;

    String getName( int index );

    GM_MultiSurface getGeometry( int index ) throws CoreException;
  }

  public AbstractImportOperation( final InputDescriptor inputDescriptor )
  {
    m_inputDescriptor = inputDescriptor;
  }

  @Override
  public final IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final int size = m_inputDescriptor.size();
    final SubMonitor progess = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.0" ), size + 10 ); //$NON-NLS-1$

    init();
    ProgressUtilities.worked( progess, 10 );

    // traverse input workspace and import all single input soilTypes, if the soilType class exists
    for( int i = 0; i < size; i++ )
    {
      try
      {
        final String label = m_inputDescriptor.getName( i );
        final GM_MultiSurface geometry = m_inputDescriptor.getGeometry( i );
        checkGeometry( geometry, label );

        importRow( i, label, geometry, m_log );
      }
      catch( final CoreException e )
      {
        m_log.add( e.getStatus() );
      }
      // FIXME: should handle IOException differently
      catch( final Exception e )
      {
        final String message = String.format( Messages.getString( "AbstractImportOperation.2" ), i + 1 ); //$NON-NLS-1$
        final Status status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message, e );
        m_log.add( status );
      }
      catch( final OutOfMemoryError e )
      {
        final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString( "AbstractImportOperation.3" ), e ); //$NON-NLS-1$
        throw new CoreException( status );
      }

      ProgressUtilities.worked( progess, 1 );
    }

    return m_log.asMultiStatusOrOK( STR_PROBLEMS_DURING_IMPORT, STR_IMPORT_SUCCESSFULLY_TERMINATED );
  }

  private void checkGeometry( final GM_MultiSurface geometry, final String label )
  {
    if( geometry == null )
    {
      final String message = Messages.getString( "org.kalypso.convert.namodel.hydrotope.PedologyImportOperation.3", label ); //$NON-NLS-1$
      m_log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
    }
    else
    {
      final IStatus isValidTop = TopologyChecker.checkTopology( geometry, label );
      if( !isValidTop.isOK() )
        m_log.add( isValidTop );
    }
  }

  protected abstract void init( );

  protected abstract void importRow( int i, String label, GM_MultiSurface geometry, IStatusCollector log ) throws CoreException;
}
