package org.kalypso.editor;

import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;

final class EditorPartJob extends Job
{
  private final ICommand myCommand;

  private final AbstractEditorPart myEditor;

  private final ICommandManager myCommandManager;

  private static final Logger LOGGER = Logger.getLogger( EditorPartJob.class.getName() );

  private final Runnable myRunnable;

  public EditorPartJob( final AbstractEditorPart editor, final ICommand command,
      final ICommandManager commandManager, final Runnable runnable )
  {
    super( "Job: " + command.getDescription() );

    setRule( editor.getShedulingRule() );
    setUser( true );
    setPriority( Job.SHORT );

    myCommand = command;
    myEditor = editor;
    myCommandManager = commandManager;
    myRunnable = runnable;
  }

  protected IStatus run( final IProgressMonitor monitor )
  {
    myEditor.setDirty( true );

    LOGGER.info( "Executing command: " + myCommand.getDescription() );

    try
    {
      myCommandManager.postCommand( myCommand, null );
      if( myRunnable != null )
        myRunnable.run();
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR,
          KalypsoGisPlugin.getDefault().getBundle().getSymbolicName(), 0,
          "Fehler beim Ausführen eines Kommandos", e );
    }

    return Status.OK_STATUS;
  }
}