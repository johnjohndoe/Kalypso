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
package org.kalypso.ogc.gml.map.handlers;

import java.awt.image.BufferedImage;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.awt.ImageConverter;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * @author Gernot Belger
 */
public class PrintMapHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IMapPanel mapPanel = MapHandlerUtils.getMapPanel( context );
    final IMapModell mapModell = MapHandlerUtils.getMapModell( context );
    final String mapName = mapModell.getName().getValue();

    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final PrintDialog printDialog = new PrintDialog( shell );
    final PrinterData printerData = printDialog.open();
    if( printerData == null )
      return null;

    final Printer printer = new Printer( printerData );

    final Job job = new Job( "'" + mapName + "' wird gedruckt" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {

        printer.startJob( mapName );
        printer.startPage();

        final GC gc = new GC( printer );
        gc.setAntialias( SWT.ON );
        gc.setTextAntialias( SWT.ON );

        final Rectangle clientArea = printer.getClientArea();

        try
        {
          // TRY ONE: printing directly on the printer: works, but all sizes of styles are in pixel, resulting
          // in too much fine lines/symbols on the map
          // final BufferedImage awtImage = MapModellHelper.createWellFormedImageFromModel( mapPanel, clientArea.width,
          // clientArea.height );

          // TRY TWO: scale the current map image on the printer: works, but scaling problems, as expected
          final BufferedImage awtImage = mapPanel.getMapImage();

          // scale onto map: centre, fit in (keep ratio), insets
          final Rectangle sourceRect = new Rectangle( 0, 0, awtImage.getWidth(), awtImage.getHeight() );
          final Point insets = new Point( 20, 20 ); // insets in x and y direction
          final Rectangle targetRect = scaleIntoClient( sourceRect, clientArea, insets );

          // Convert to SWT and print onto map
          final ImageData imgData = ImageConverter.convertToSWT( awtImage );
          final Image swtImage = new Image( printer, imgData );
          gc.drawImage( swtImage, sourceRect.x, sourceRect.y, sourceRect.width, sourceRect.height, targetRect.x, targetRect.y, targetRect.width, targetRect.height );

          // a black border
          gc.setForeground( printer.getSystemColor( SWT.COLOR_BLACK ) );
          gc.setLineWidth( 5 );
          gc.drawRectangle( targetRect.x, targetRect.y, targetRect.width, targetRect.height );
          // testing: client area
          // gc.drawRectangle( clientArea.x, clientArea.y, clientArea.width - 3, clientArea.height - 3 );
        }
        catch( final Throwable e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
        finally
        {
          gc.dispose();
          printer.endPage();
          printer.endJob();
        }

        return StatusUtilities.createStatus( IStatus.INFO, "Karte erfolgreich gedruckt", null );
      }
    };

    job.addJobChangeListener( new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent evt )
      {
        printer.dispose();
      }
    } );

    job.setUser( true );
    job.schedule();

    return null;
  }

  public static Rectangle scaleIntoClient( final Rectangle sourceRect, final Rectangle destRect, final Point insets )
  {
    final Rectangle targetRect = new Rectangle( destRect.x + insets.x, destRect.y + insets.y, destRect.width - (2 * insets.x), destRect.height - (2 * insets.y) );

    final double scaleX = (double) targetRect.width / (double) sourceRect.width;
    final double scaleY = (double) targetRect.height / (double) sourceRect.height;

    // Scale: scale both with same ratio, so both fit into the client
    if( scaleX < scaleY )
    {
      targetRect.width = (int) (sourceRect.width * scaleX); // should equal clientRect.width
      targetRect.height = (int) (sourceRect.height * scaleX);
    }
    else
    {
      targetRect.width = (int) (sourceRect.width * scaleY);
      targetRect.height = (int) (sourceRect.height * scaleY); // should equal clientArea.height
    }

    // Centre
    targetRect.x += (destRect.width - targetRect.width) / 2 - insets.x;
    targetRect.y += (destRect.height - targetRect.height) / 2 - insets.y;

    return targetRect;
  }
}
