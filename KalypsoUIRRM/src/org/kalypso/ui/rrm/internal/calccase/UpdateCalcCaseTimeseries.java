/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.calccase;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.runtime.HandleDoneJobChangeAdapter;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.simulation.ui.actions.CalcCaseHelper;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class UpdateCalcCaseTimeseries extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    final ISelection selection = window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    // Rechenfälle raussuchen
    final String title = Messages.getString( "org.kalypso.simulation.ui.actions.UpdateCalcCaseTimeseries_0" ); //$NON-NLS-1$
    final String message = Messages.getString( "org.kalypso.simulation.ui.actions.UpdateCalcCaseTimeseries_1" );//$NON-NLS-1$
    final IFolder[] calcCases = CalcCaseHelper.chooseCalcCases( shell, selection, title, message, INaCalcCaseConstants.CALCULATION_GML_PATH );

    if( calcCases == null )
      return null;

    // die Rechenfälle sollen nacheinander aktualisiert werden
    // parallelität macht hier keinen Sinn
    // final MutexRule mutexRule = new MutexRule();

    // changed by doemming: jobrule is now calcCase folder, otherwise the job conflicts with the project rule.
    // jobs can now run parallel

    // alle Rechenfälle aktualisieren
    for( final IFolder calcCase : calcCases )
    {
      final Job job = new Job( Messages.getString( "org.kalypso.simulation.ui.actions.UpdateCalcCaseTimeseries_2" ) + calcCase.getName() ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final ModelNature nature = (ModelNature) calcCase.getProject().getNature( ModelNature.ID );

            final IStatus status = nature.updateCalcCase( calcCase, monitor );

            return status;
          }
          catch( final CoreException e )
          {
            e.printStackTrace();

            return e.getStatus();
          }
        }
      };

      // TODO see if autoRemoveListener (argument of HandleDoneJobChangeAdapter) should be true?
      job.addJobChangeListener( new HandleDoneJobChangeAdapter( shell, title, Messages.getString( "org.kalypso.simulation.ui.actions.UpdateCalcCaseTimeseries_4" ), false, IStatus.ERROR, true ) ); //$NON-NLS-1$ //$NON-NLS-2$
      job.setUser( true );
      job.setRule( calcCase.getProject() );
      job.schedule();
    }

    return null;
  }
}
