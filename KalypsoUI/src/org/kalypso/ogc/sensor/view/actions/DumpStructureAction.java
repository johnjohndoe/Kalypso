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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.sensor.view.ObservationChooser;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.ui.ImageProvider;

/**
 * DumpStructureAction
 * 
 * @author schlienger (16.06.2005)
 */
public class DumpStructureAction extends AbstractObservationChooserAction implements ISelectionChangedListener
{
  public DumpStructureAction( final ObservationChooser explorer )
  {
    super( explorer, "Struktur exportieren", ImageProvider.IMAGE_ZML_REPOSITORY, "Exportiert die Gesamtstruktur in der Zwischenablage" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  public void dispose( )
  {
    getExplorer().removeSelectionChangedListener( this );
  }

  @Override
  public void run( )
  {
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    final StringWriter writer = new StringWriter();

    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( final IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
      {
        monitor.beginTask( "Struktur exportieren", 1000 );

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

      final Clipboard clipboard = new Clipboard( getShell().getDisplay() );
      clipboard.setContents( new Object[] { writer.toString() }, new Transfer[] { TextTransfer.getInstance() } );
      clipboard.dispose();
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      MessageDialog.openWarning( getShell(), "Struktur exportieren", e.getLocalizedMessage() );
    }
    catch( final InterruptedException ignored )
    {
      // empty
    }
  }

  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }
}
