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
package org.kalypso.model.wspm.tuhh.ui.export;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfWriter;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.wspwin.core.Plotter;

/**
 * Action which exports the selected profile as .prf files.
 * <p>
 * TODO: better use a wizard and let the user choose <br>
 * 1) the profiles to export<br>
 * 2) a name pattern for the generated files<br>
 * 3) If to directly print or show the plot
 * 
 * @author Gernot Belger
 */
public class PlotterExportHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ISelection selection = HandlerUtil.getCurrentSelection( event );
    final ProfileSelection profileSelection = new ProfileSelection( selection );
    final IProfileFeature[] profiles = profileSelection.getSelectedProfiles();

    if( !Plotter.checkPlotterExe( shell ) )
      return null;

    final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );

    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Export to Plotter", profiles.length );

        for( final IProfileFeature feature : profiles )
        {
          final WspmWaterBody water = feature.getWater();

          final IProfil profile = feature.getProfil();

          final String filename = PrfExporter.createWspWinFileName( profile );
//          String filename = "exportTmp" + i + ".prf"; //$NON-NLS-1$ //$NON-NLS-2$
          final File file = new File( System.getProperty( "java.io.tmpdir" ), filename );//$NON-NLS-1$
          try
          {
            final PrfWriter prfWriter = new PrfWriter( profile );

            PrfExporter.configurePrfWriterWithMetadata( water, profile, prfWriter );

            prfWriter.write( file );

            Plotter.openPrf( file );

            Thread.sleep( 500 );
          }
          catch( final IOException e )
          {
            e.printStackTrace();
            return StatusUtilities.statusFromThrowable( e, "an error occured starting the plotter" );
          }
          catch( final InterruptedException e )
          {
            e.printStackTrace();
            return StatusUtilities.statusFromThrowable( e, "an error occured starting the plotter" );
          }

          monitor.worked( 1 );
          if( monitor.isCanceled() )
            return new Status( IStatus.CANCEL, id, 1, "program abortion by user", null );
        }

        return new Status( IStatus.OK, id, "" );
      }
    };

    ProgressUtilities.busyCursorWhile( op, "could not export profile" );
    return null;
  }

}
