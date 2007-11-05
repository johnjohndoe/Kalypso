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
package org.kalypso.model.flood.handlers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.core.FloodModelProcess;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class ProcessFloodModelHandler extends AbstractHandler implements IHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      /* Get context */
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
      final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );

      final IFloodModel model = dataProvider.getModel( IFloodModel.class );
      final IFeatureWrapperCollection<IRunoffEvent> events = model.getEvents();

      /* ask user which events to process? */
      final Object[] eventsToProcess = askUserForEvents( shell, events );

      // check prerequisites
      // - event has at least 1 tin
      // - at least one grid present

      final IStatus processResult = runCalculation( model, eventsToProcess, dataProvider );
      ErrorDialog.openError( shell, "Flood-Modeller", "Flieﬂtiefen erzeugen", processResult );
      if( processResult.isOK() )
      {
        // handle results if job is successful
      }

      return null;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw new ExecutionException( e.getLocalizedMessage(), e );
    }
  }

  private Object[] askUserForEvents( final Shell shell, final IFeatureWrapperCollection<IRunoffEvent> events )
  {
    final LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        final IRunoffEvent event = (IRunoffEvent) element;
        return event.getName();
      }
    };

    final ListSelectionDialog dialog = new ListSelectionDialog( shell, events, new ArrayContentProvider(), labelProvider, "Welche Ereignisse sollen verarbeitet werden?" );
    dialog.setTitle( "Flood-Modeller" );

    if( dialog.open() == Window.OK )
      return dialog.getResult();

    return null;
  }

  private IStatus runCalculation( final IFloodModel model, final Object[] eventsToProcess, final SzenarioDataProvider dataProvider )
  {
    final IRunoffEvent[] events = new IRunoffEvent[eventsToProcess.length];
    for( int i = 0; i < events.length; i++ )
      events[i] = (IRunoffEvent) eventsToProcess[i];

    final FloodModelProcess process = new FloodModelProcess( model, events );

    // TODO: clear existing results!

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws CoreException, InterruptedException, InvocationTargetException
      {
        final IStatus result = process.process( monitor );

        dataProvider.saveModel( IFloodModel.class, monitor );

        return result;
      }
    };

    return ProgressUtilities.busyCursorWhile( operation, "Fehler beim Berechnen der Flieﬂtiefen" );
  }

}
