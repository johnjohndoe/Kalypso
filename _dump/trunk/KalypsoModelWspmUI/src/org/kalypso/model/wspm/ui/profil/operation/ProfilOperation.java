/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.profil.operation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IllegalProfileOperationException;
import org.kalypso.model.wspm.core.profil.changes.IllegalChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;

public final class ProfilOperation extends AbstractOperation
{
  private final List<IProfilChange> m_undoChanges = new ArrayList<IProfilChange>();

  private final IProfil m_profile;

  private final List<IProfilChange> m_changes = new ArrayList<IProfilChange>();

  private final boolean m_rollbackAll;

  private boolean m_canUndo = true;

  public ProfilOperation( final String label, final IProfil profile, final boolean rollbackAll )
  {
    this( label, profile, new IProfilChange[] {}, rollbackAll );
  }

  public ProfilOperation( final String label, final IProfil profile, final IProfilChange change, final boolean rollbackAll )
  {
    this( label, profile, new IProfilChange[] { change }, rollbackAll );
  }

  public ProfilOperation( final String label, final IProfil profile, final IProfilChange[] changes, final boolean rollbackAll )
  {
    super( label );

    addContext( new ProfilUndoContext( profile ) );

    m_changes.addAll( Arrays.asList( changes ) );
    m_profile = profile;
    m_rollbackAll = rollbackAll;
  }

  public void addChange( final IProfilChange change )
  {
    m_changes.add( change );
  }

  protected final IProfil getProfil( )
  {
    return m_profile;
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

  private IStatus doit( final IProgressMonitor monitor, @SuppressWarnings("unused")
  final IAdaptable info, final List<IProfilChange> undoChanges, final List<IProfilChange> changes )
  {
    monitor.beginTask( "Profil wird geändert", changes.size() );
    final ProfilChangeHint hint = new ProfilChangeHint();
    final ArrayList<IProfilChange> doneChanges = new ArrayList<IProfilChange>();
    try
    {
      for( final IProfilChange change : changes )
      {
        final IProfilChange undoChange = change == null ? null : change.doChange( hint );
        if( undoChange != null && undoChange instanceof IllegalChange )
        {
          throw new IllegalProfileOperationException( undoChange.getInfo(), change );
        }
        doneChanges.add( change );
        undoChanges.add( 0, undoChange );
        monitor.worked( 1 );
      }
    }
    catch( final IllegalProfileOperationException e )
    {
      if( m_rollbackAll )
      {
        rollback( undoChanges );
        doneChanges.clear();
      }
      final Display d = PlatformUI.getWorkbench().getDisplay();
      d.asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !d.isDisposed() )
          {
            final IProfilChange change = e.getProfilChange();
            if( change == null || change.getInfo() == null )
              MessageDialog.openWarning( d.getActiveShell(), "Unvollständige Ausführung", e.getMessage() );
            else
              MessageDialog.openWarning( d.getActiveShell(), e.getMessage(), change.getInfo() );
          }
        }
      } );
    }
    finally
    {
      // auf jeden Fall monitor beenden und
      // einen fire auf allen changes absetzen (zuviel ist nicht schlimm)
      m_canUndo = undoChanges.size() > 0;
      monitor.done();
      m_profile.fireProfilChanged( hint, doneChanges.toArray( new IProfilChange[doneChanges.size()] ) );
    }
    // auf jeden Fall OK zurückgeben da sonst die UNDO-Liste nicht gefüllt wird
    return Status.OK_STATUS;
  }

  private void rollback( final List<IProfilChange> changes )
  {
    final ProfilChangeHint hint = new ProfilChangeHint();
    for( final IProfilChange undo : changes )
    {
      try
      {
        if( undo != null )
          undo.doChange( hint );
      }
      catch( final IllegalProfileOperationException e )
      {
        // should never happen
        e.printStackTrace();
      }
    }
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
