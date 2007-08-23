package org.kalypso.ogc.gml.map.handlers.listener;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.map.handlers.MapScreenShotHandler;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.KalypsoScreenshotPreferencePage;

/**
 * This is a listener which is notified, when the screenshot command changes its state.
 * 
 * @author Holger Albert
 */
public final class MapScreenshotExecuteListener implements IExecutionListener
{
  /**
   * The constructor.
   */
  public MapScreenshotExecuteListener( )
  {
  }

  /**
   * @see org.eclipse.core.commands.IExecutionListener#notHandled(java.lang.String,
   *      org.eclipse.core.commands.NotHandledException)
   */
  public void notHandled( String commandId, NotHandledException exception )
  {
    if( "org.kalypso.ogc.gml.map.Screenshot".equals( commandId ) == false )
      return;
  }

  /**
   * @see org.eclipse.core.commands.IExecutionListener#postExecuteFailure(java.lang.String,
   *      org.eclipse.core.commands.ExecutionException)
   */
  public void postExecuteFailure( String commandId, ExecutionException exception )
  {
    if( "org.kalypso.ogc.gml.map.Screenshot".equals( commandId ) == false )
      return;

    /* Log the error message. */
    KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( exception ) );

    /* Show an error dialog. */
    ErrorDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Fehler", "Konnte die das Bild nicht exportieren.", StatusUtilities.statusFromThrowable( exception ) );
  }

  /**
   * @see org.eclipse.core.commands.IExecutionListener#postExecuteSuccess(java.lang.String, java.lang.Object)
   */
  public void postExecuteSuccess( String commandId, Object returnValue )
  {
    if( "org.kalypso.ogc.gml.map.Screenshot".equals( commandId ) == false )
      return;

    if( !(returnValue instanceof File) )
      return;

    /* Cast to file. */
    File file = (File) returnValue;

    /* Show the user a success dialog. */
    MessageDialog.openInformation( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Nachricht", "Der Export-Vorgang war erfolgreich: " + file.toString() );
  }

  /**
   * @see org.eclipse.core.commands.IExecutionListener#preExecute(java.lang.String,
   *      org.eclipse.core.commands.ExecutionEvent)
   */
  public void preExecute( String commandId, ExecutionEvent event )
  {
    if( "org.kalypso.ogc.gml.map.Screenshot".equals( commandId ) == false )
      return;

    IPreferenceStore preferences = KalypsoScreenshotPreferencePage.getPreferences();
    String extension = preferences.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_FORMAT );
    String dir = preferences.getString( KalypsoScreenshotPreferencePage.KEY_SCREENSHOT_TARGET );

    File file = null;
    if( dir != null && !dir.equals( "" ) )
      file = new File( dir );

    /* Create the file dialog. */
    FileDialog dialog = new FileDialog( PlatformUI.getWorkbench().getDisplay().getActiveShell(), SWT.NONE );

    /* Set some dialog information. */
    dialog.setText( "Kartenausschnitt exportieren" );

    /* Set the initialize path, if available. */
    if( file != null )
      dialog.setFilterPath( file.getAbsolutePath() );

    /* Initialize with some settings. */
    dialog.setFilterExtensions( new String[] { "*." + extension } );
    dialog.setFilterNames( new String[] { extension.toUpperCase() + " - Images" } );

    /* Show the dialog. */
    String result = dialog.open();

    /* If the user has canceled the dialog, do not execute the command. */
    if( result == null )
    {
      stopIt( event );
      return;
    }

    /* Create the target file. */
    File target = new File( result );

    /* Add extension. */
    if( !target.getName().endsWith( "." + extension ) )
      target = new File( target.getParentFile(), target.getName() + "." + extension );

    /* If the target exists already, give a warning and do only execute, if the user has confirmed it. */
    if( target.exists() )
    {
      /* Ask the user. */
      boolean confirmed = MessageDialog.openConfirm( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Nachfrage", "Die ausgewählte Datei existiert bereits, möchten Sie sie überschreiben?" );

      /* If he has not confirmed, do not execute the command. */
      if( !confirmed )
      {
        stopIt( event );
        return;
      }
    }

    try
    {
      /* Create the empty file. */
      target.createNewFile();
    }
    catch( IOException e )
    {
      /* Show an error dialog. */
      ErrorDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), "Fehler", "Konnte die Datei '" + target.getName() + "' nicht anlegen.", StatusUtilities.statusFromThrowable( e ) );

      /* Stop executing ... */
      stopIt( event );

      /* ... and leave. */
      return;
    }

    /* If everything is okay, set the target. */
    IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    context.addVariable( MapScreenShotHandler.CONST_TARGET_FILE, target );
  }

  /**
   * This function stops the execution of the command.
   */
  private void stopIt( ExecutionEvent event )
  {
    /* Say the command handler, he shoud not execute the command. */
    IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    context.addVariable( MapScreenShotHandler.CONST_SHOULD_EXECUTE_BOOLEAN, Boolean.FALSE );
  }
}