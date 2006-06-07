package org.kalypso.model.wspm.ui.profil.operation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.changes.IllegalChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


public final class ProfilOperation extends AbstractOperation
{
  private final List<IProfilChange> m_undoChanges = new ArrayList<IProfilChange>();

  private final IProfilEventManager m_pem;

  private final List<IProfilChange> m_changes = new ArrayList<IProfilChange>();

  private final boolean m_rollbackAll;

  private boolean m_canUndo = true;

  public ProfilOperation( final String label, final IProfilEventManager pem, boolean rollbackAll )
  {
    this( label, pem, new IProfilChange[] {}, rollbackAll );
  }

  public ProfilOperation( final String label, final IProfilEventManager pem, final IProfilChange change, boolean rollbackAll )
  {
    this( label, pem, new IProfilChange[] { change }, rollbackAll );
  }

  public ProfilOperation( final String label, final IProfilEventManager pem, final IProfilChange[] changes, boolean rollbackAll )
  {
    super( label );

    addContext( new ProfilUndoContext( pem.getProfil() ) );

    m_changes.addAll( Arrays.asList( changes ) );
    m_pem = pem;
    m_rollbackAll = rollbackAll;
  }

  public void addChange( final IProfilChange change )
  {
    m_changes.add( change );
  }

  protected final IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }

  @Override
  public IStatus redo( final IProgressMonitor monitor, final IAdaptable info )
  {
    return doit( monitor, info, new ArrayList<IProfilChange>(), m_changes );
  }

  @Override
  public IStatus undo( final IProgressMonitor monitor, final IAdaptable info )
  {
    return doit( monitor, info, new ArrayList<IProfilChange>(), m_undoChanges );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor, final IAdaptable info )
  {
    return doit( monitor, info, m_undoChanges, m_changes );
  }

  private IStatus doit( final IProgressMonitor monitor, final IAdaptable info, final List<IProfilChange> undoChanges, List<IProfilChange> changes )
  {
    monitor.beginTask( "Profil wird geändert", changes.size() );
    final ProfilChangeHint hint = new ProfilChangeHint();
    final ArrayList<IProfilChange> doneChanges = new ArrayList<IProfilChange>();
    try
    {
      for( final IProfilChange change : changes )
      {
        try
        {
          final IProfilChange undoChange = change.doChange( hint );
          if( undoChange instanceof IllegalChange )
          {
            handleIllegalChanges( info, doneChanges, (String) undoChange.getObject() );
          }
          else
          {
            doneChanges.add( change );
            undoChanges.add( 0, undoChange );
            monitor.worked( 1 );
          }
        }
        catch( final ProfilDataException e )
        {
          rollback( undoChanges );
          doneChanges.clear();
          return new Status( IStatus.ERROR, KalypsoModelWspmUIPlugin.ID, 0, "Fehler beim Ändern des Profils", e );
        }
      }
    }
    finally
    {
      // auf jeden Fall monitor beenden und
      // einen fire auf allen changes absetzen (zuviel ist nicht schlimm)
      monitor.done();
      m_pem.fireProfilChanged( hint, doneChanges.toArray( new IProfilChange[doneChanges.size()] ) );
    }
    return Status.OK_STATUS;
  }

  private void rollback( final List<IProfilChange> changes )
  {
    final ProfilChangeHint hint = new ProfilChangeHint();
    for( IProfilChange undo : changes )
    {
      try
      {
        undo.doChange( hint );
      }
      catch( ProfilDataException e )
      {
        // should never happen
        e.printStackTrace();
      }
    }
  }

  private void handleIllegalChanges( final IAdaptable info, final List<IProfilChange> undoChanges, final String msg )
  {
    if( m_rollbackAll )
    {
      rollback( undoChanges );
      undoChanges.clear();
    }
    else
    {
      if( info != null )
      {
        final Shell shell = (Shell) info.getAdapter( Shell.class );
        shell.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            MessageDialog.openWarning( shell, "Unvollständige Ausführung", msg );
          }
        } );
      }
      else
      {
        Logger.getLogger( getClass().getName() ).warning( "Message-Dialog konnte nicht geöffnet werden. Ürsprungliche Nachricht: " + msg );
      }
    }
    m_canUndo = undoChanges.size() > 0;
  }

  /**
   * @see org.eclipse.core.commands.operations.AbstractOperation#canUndo()
   */
  @Override
  public boolean canUndo( )
  {
    return m_canUndo;
  }
}
