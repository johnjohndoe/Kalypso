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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.contribs.java.io.FileUtilities;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.RepositoryDumper;

/**
 * This action starts the dump of the structure of a repository. It will ask the user for a directory name and puts the
 * data there.
 * 
 * @author Holger Albert
 */
public class DumpExtendedAction extends AbstractObservationChooserAction implements ISelectionChangedListener
{
  /**
   * The constructor.
   * 
   * @param explorer
   *          The observation chooser.
   */
  public DumpExtendedAction( ObservationChooser explorer )
  {
    super( explorer, "Struktur/ZMLs exportieren", ImageProvider.IMAGE_ZML_REPOSITORY,
        "Exportiert die Gesamtstruktur in das Dateisystem" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    /* Get the repository. */
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    Shell shell = Workbench.getInstance().getDisplay().getActiveShell();

    /* Ask the user for a directory. */
    DirectoryDialog dialog = new DirectoryDialog( shell );
    String directoryPath = dialog.open();
    if( directoryPath == null )
      return;

    final File directory = new File( directoryPath );
    if( !directory.exists() )
    {
      /* Should not happen, but secure is secure. */
      return;
    }

    /* If the 'structure.txt' exists already, ask the user, if he want to delete it. */
    File structureFile = new File( directory, "structure.txt" );
    if( structureFile.exists() )
    {
      boolean ok = MessageDialog
          .openConfirm(
              shell,
              "Bestätigung",
              "Sie haben einen Pfad ausgewählt, in dem bereits ein Export liegt. Wenn Sie fortfahren, wird der komplette Inhalt des Verzeichnisses gelöscht, bevor der neue Export durchgeführt wird. Sind Sie damit einverstanden?" );

      if( !ok )
        return;

      /* Deletes the complete directory and its content. */
      FileUtilities.deleteRecursive( directory );

      /* Create the directory again. */
      if( !directory.mkdir() )
      {
        MessageDialog.openError( shell, "Fehler", "Konnte das Verzeichnis für den Export nicht wieder neu anlegen ..." );
        return;
      }
    }

    /* Dump the structure. */
    IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      /**
       * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
       */
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "Struktur exportieren", 1000 );

        try
        {
          /* Do the dump. This may take a while. */
          RepositoryDumper.dumpExtended( directory, rep, monitor );
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

    try
    {
      Workbench.getInstance().getProgressService().busyCursorWhile( runnable );
    }
    catch( InvocationTargetException e )
    {
      /* For debug purposes. */
      e.printStackTrace();

      /* Tell the user, that something went wrong. */
      MessageDialog.openWarning( getShell(), "Struktur exportieren", e.getLocalizedMessage() );
    }
    catch( InterruptedException ignored )
    {
      /* Will be ignored. */
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }
}