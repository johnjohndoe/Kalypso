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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.ExportableMap;
import org.kalypso.ui.preferences.KalypsoScreenshotPreferencePage;

/**
 * TWEAKING of MapScreenShotHandler:<br>
 * <br>
 * final ICommandService service = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );<br>
 * final Command command = service.getCommand( "org.kalypso.ogc.gml.map.Screenshot" );<br>
 * <br>
 * command.addExecutionListener( new IExecutionListener() {<br>
 * <br>
 * public void postExecuteSuccess( final String commandId, final Object returnValue )<br> {<br>
 * if( !(returnValue instanceof URL) )<br>
 * return;<br>
 * ... <br>
 * public void preExecute( final String commandId, final ExecutionEvent event ) {<br>
 * final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();<br>
 * context.addVariable( MapScreenShotHandler.CONST_TARGET_DIR_URL, folder.getLocationURI().toURL() );<br> ..<br>
 * 
 * @author kuch
 */
public class MapScreenShotHandler extends AbstractHandler
{
  public static final String CONST_SHOULD_EXECUTE_BOOLEAN = "shouldExecute"; // ICommand.executionListener can stop

  // processing

  public static final String CONST_TARGET_DIR_URL = "targetDir"; // can be overwritten by an commandListener

  public static final String CONST_EXPORTED_IMAGE_URL = "exportedImage";

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    try
    {
      final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
      final IPreferenceStore preferences = KalypsoScreenshotPreferencePage.getPreferences();

      final Object objShouldExecute = context.getVariable( MapScreenShotHandler.CONST_SHOULD_EXECUTE_BOOLEAN );
      if( (objShouldExecute != null) && ((Boolean) objShouldExecute == false) )
        return null;

      final int width = preferences.getInt( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_WIDTH );
      final int height = preferences.getInt( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_HEIGHT );
      final String format = preferences.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT );

      final URL targetURL = (URL) context.getVariable( MapScreenShotHandler.CONST_TARGET_DIR_URL );

      /* if targetURL is overwritten by ICommand.Executionlistener - take listener targetDir */
      File targetDir;
      if( targetURL != null )
        targetDir = new File( targetURL.toURI() );
      else
        targetDir = new File( preferences.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET ) );

      /* if targetDir is instance of file (!dir) -> set targetImage = targetDir */
      final File imgTarget;
      if( targetDir.isFile() )
        imgTarget = targetDir;
      else if( targetDir.isDirectory() )
        imgTarget = getTargetImageFile( targetDir, format );
      else
        throw (new NotImplementedException( "targetDir must be file or directory" ));

      final BufferedOutputStream os = new BufferedOutputStream( new FileOutputStream( imgTarget ) );

      final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
      final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );

      final ExportableMap export = new ExportableMap( mapPanel, width, height, format );
      export.exportObject( os, new NullProgressMonitor(), null );

      return imgTarget.toURL();
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw (new ExecutionException( e.getMessage() ));
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      throw (new ExecutionException( e.getMessage() ));
    }
    catch( final URISyntaxException e )
    {
      e.printStackTrace();
      throw (new ExecutionException( e.getMessage() ));
    }
  }

  private File getTargetImageFile( final File targetDir, final String format )
  {
    if( !targetDir.exists() || !targetDir.isDirectory() || (format == null) )
      throw (new IllegalStateException());

    int count = 0;
    while( true )
    {
      final File file = new File( targetDir, "kalypso_map_" + count + "." + format );
      if( !file.exists() )
        return file;

      count++;
    }
  }

}
