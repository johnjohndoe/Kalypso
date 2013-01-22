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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class SobekExportOperation implements ICoreRunnableWithProgress
{
  private final ISobekProfileExportOperation[] m_operations;

  public SobekExportOperation( final ISobekProfileExportOperation[] operations )
  {
    m_operations = operations;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final Collection<IStatus> problems = new ArrayList<>( m_operations.length );

    monitor.beginTask( Messages.getString( "SobekExportProfilesWizard_0" ), m_operations.length ); //$NON-NLS-1$

    for( final ISobekProfileExportOperation operation : m_operations )
    {
      monitor.subTask( operation.getLabel() );

      final IStatus execute = operation.execute( new SubProgressMonitor( monitor, 1 ) );
      if( !execute.isOK() )
      {
        problems.add( execute );
      }
    }

    final IStatus[] problemChildren = problems.toArray( new IStatus[problems.size()] );
    if( problemChildren.length > 0 )
    {
      final String message = Messages.getString( "SobekExportProfilesWizard_1" ); //$NON-NLS-1$
      final IStatus status = new MultiStatus( KalypsoModelWspmTuhhUIPlugin.getID(), 0, problemChildren, message, null );
      throw new CoreException( status );
    }

    return Status.OK_STATUS;
  }
}