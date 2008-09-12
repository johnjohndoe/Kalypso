/*
 * --------------- Kalypso-Header --------------------------------------------
 *
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 *
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 *
 * and
 *
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Contact:
 *
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 *
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.view.actions;

import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

/**
 * DumpStructureAction
 * 
 * @author schlienger (16.06.2005)
 */
public class DumpStructureHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    final ObservationChooser chooser = (ObservationChooser) part.getAdapter( ObservationChooser.class );

    final IRepository rep = chooser.isRepository( chooser.getSelection() );
    if( rep == null )
      return Status.OK_STATUS;

    final StringWriter writer = new StringWriter();

    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( final IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
      {
        monitor.beginTask( Messages.getString("org.kalypso.ogc.sensor.view.DumpStructureHandler.0"), 1000 ); //$NON-NLS-1$

        try
        {
          rep.dumpStructure( writer, monitor );
          writer.close();
        }
        catch( final IOException e )
        {
          throw new InvocationTargetException( e );
        }
        catch( final RepositoryException e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          IOUtils.closeQuietly( writer );

          monitor.done();
        }
      }
    };

    try
    {
      PlatformUI.getWorkbench().getProgressService().busyCursorWhile( runnable );

      final Clipboard clipboard = new Clipboard( shell.getDisplay() );
      clipboard.setContents( new Object[] { writer.toString() }, new Transfer[] { TextTransfer.getInstance() } );
      clipboard.dispose();
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      MessageDialog.openWarning( shell, Messages.getString("org.kalypso.ogc.sensor.view.DumpStructureHandler.1"), e.getLocalizedMessage() ); //$NON-NLS-1$
    }
    catch( final InterruptedException ignored )
    {
      // empty
    }

    return Status.OK_STATUS;
  }

}
