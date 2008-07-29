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

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.io.FileUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.repository.RepositoryDumper;

/**
 * This action starts the dump of the structure of a repository. It will ask the user for a directory name and puts the
 * data there.
 * 
 * @author Holger Albert
 */
public class DumpExtendedHandler extends AbstractHandler
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

    /* Get the repository. */
    final IRepository rep = chooser.isRepository( chooser.getSelection() );
    if( rep == null )
      return null;

    /* Ask the user for a directory. */
    DirectoryDialog dialog = new DirectoryDialog( shell );
    String directoryPath = dialog.open();
    if( directoryPath == null )
      return null;

    final File directory = new File( directoryPath );
    if( !directory.exists() )
    {
      /* Should not happen, but secure is secure. */
      return null;
    }

    /* If the 'structure.txt' exists already, ask the user, if he want to delete it. */
    File structureFile = new File( directory, "structure.txt" );
    if( structureFile.exists() )
    {
      boolean ok = MessageDialog.openConfirm( shell, "Bestätigung", "Sie haben einen Pfad ausgewählt, in dem bereits ein Export liegt. Wenn Sie fortfahren, wird der komplette Inhalt des Verzeichnisses gelöscht, bevor der neue Export durchgeführt wird. Sind Sie damit einverstanden?" );

      if( !ok )
        return null;

      /* Deletes the complete directory and its content. */
      FileUtilities.deleteRecursive( directory );

      /* Create the directory again. */
      if( !directory.mkdir() )
      {
        MessageDialog.openError( shell, "Fehler", "Konnte das Verzeichnis für den Export nicht wieder neu anlegen ..." );
        return null;
      }
    }

    /* Dump the structure. */
    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "Struktur exportieren", IProgressMonitor.UNKNOWN );

        try
        {
          /* Do the dump. This may take a while. */
          RepositoryDumper.dumpExtended( directory, rep, monitor );
          return Status.OK_STATUS;
        }
        catch( Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( runnable );
    ErrorDialog.openError( shell, Messages.getString("org.kalypso.ogc.sensor.view.DumpStructureHandler.1"), "Struktur konnte nicht exportiert werden", status ); //$NON-NLS-1$
    
    return null;
  }

  // TODO: ui-handler
//  /**
//   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
//   */
//  public void selectionChanged( SelectionChangedEvent event )
//  {
//    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
//  }
}