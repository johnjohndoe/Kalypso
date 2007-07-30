/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPartConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.WorkbenchPart;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author bce
 */
public abstract class AbstractEditorPart extends WorkbenchPart implements IResourceChangeListener, ICommandTarget
{
  /**
   * The property id for <code>isDirty</code>.
   */
  public static final int PROP_DIRTY = IWorkbenchPartConstants.PROP_DIRTY;

  /**
   * Editor input, or <code>null</code> if none.
   */
  private IEditorInput editorInput = null;

  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run( )
    {
      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireDirty();
        }
      } );
    }
  };

  protected final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), m_dirtyRunnable );

  /**
   * This flag prevents reload on save.
   */
  private boolean m_isSaving = false;

  public AbstractEditorPart( )
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
  }

  /*
   * (non-Javadoc) Method declared on IEditorPart.
   */
  public IEditorInput getEditorInput( )
  {
    return editorInput;
  }

  /*
   * (non-Javadoc) Method declared on IEditorPart.
   */
  public IEditorSite getEditorSite( )
  {
    return (IEditorSite) getSite();
  }

  /*
   * (non-Javadoc) Returns whether the contents of this editor should be saved when the editor is closed. <p> This
   * method returns <code>true</code> if and only if the editor is dirty (<code>isDirty</code>). </p>
   */
  public boolean isSaveOnCloseNeeded( )
  {
    return isDirty();
  }

  @Override
  public void dispose( )
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_commandTarget.dispose();
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public final void doSave( final IProgressMonitor monitor )
  {
    // save only possible when input is a file
    if( !(getEditorInput() instanceof FileEditorInput) )
    {
      // given user a chance to use save-as
      MessageDialog.openInformation( getSite().getShell(), "Speichern", "Der Inhalt kann nicht direkt gespeichert werden weil noch\n" + "keine grundliegende Vorlagedatei vorhanden ist. Es handelt\n"
          + "sich möglicherweise um eine 'virtuelle' Vorlage\n" + "Bitte benutzen Sie das 'Speichern als' Kommando." );

      return;
    }

    final IFileEditorInput input = (IFileEditorInput) getEditorInput();

    if( input != null )
    {
      try
      {
        m_isSaving = true;
        doSaveInternal( monitor, input );
        m_commandTarget.resetDirty();
        fireDirty();
      }
      catch( final CoreException e )
      {
        e.printStackTrace();

        ErrorDialog.openError( getSite().getShell(), "Fehler", "Fehler beim Speichern der Ansicht", e.getStatus() );
      }
      finally
      {
        m_isSaving = false;
      }
    }
  }

  protected abstract void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input ) throws CoreException;

  /**
   * Returns the status line manager of this editor.
   * 
   * @return the status line manager of this editor
   */
  private IStatusLineManager getStatusLineManager( )
  {
    // final IEditorActionBarContributor contributor = getEditorSite().getActionBarContributor();
    // if( !(contributor instanceof EditorActionBarContributor) )
    // return null;
    //
    // final IActionBars actionBars = ((EditorActionBarContributor) contributor).getActionBars();
    // if( actionBars == null )
    // return null;

    final IActionBars actionBars = getActionBars( getSite() );

    return actionBars.getStatusLineManager();
  }

  protected IActionBars getActionBars( final IWorkbenchPartSite site )
  {
    final IActionBars actionBars;
    if( site instanceof IViewSite )
    {
      actionBars = ((IViewSite) site).getActionBars();
    }
    else
    { // site instanceof IEditorSite
      actionBars = ((IEditorSite) site).getActionBars();
    }
    return actionBars;
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty( )
  {
    return m_commandTarget.isDirty();
  }

  /**
   * Returns the progress monitor related to this editor.
   * 
   * @return the progress monitor related to this editor
   */
  protected IProgressMonitor getProgressMonitor( )
  {
    IProgressMonitor pm = null;

    final IStatusLineManager manager = getStatusLineManager();
    if( manager != null )
      pm = manager.getProgressMonitor();

    return pm != null ? pm : new NullProgressMonitor();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSaveAs()
   */
  public void doSaveAs( )
  {
    final Shell shell = getSite().getShell();
    final IEditorInput input = getEditorInput();

    final SaveAsDialog dialog = new SaveAsDialog( shell );

    final IFile original = (input instanceof IFileEditorInput) ? ((IFileEditorInput) input).getFile() : null;
    if( original != null )
      dialog.setOriginalFile( original );
    else
      dialog.setOriginalName( getTitle() );

    dialog.create();

    if( dialog.open() == Window.CANCEL )
      return;

    final IPath filePath = dialog.getResult();
    if( filePath == null )
      return;

    final IProgressMonitor monitor = getProgressMonitor();

    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IFile file = workspace.getRoot().getFile( filePath );
    final IFileEditorInput newInput = new FileEditorInput( file );

    try
    {
      monitor.beginTask( "Save file", 1000 );
      doSaveInternal( new SubProgressMonitor( monitor, 1000 ), newInput );
      m_commandTarget.resetDirty();

      monitor.done();
    }
    catch( final CoreException ce )
    {
      ce.printStackTrace();

      ErrorDialog.openError( shell, "Fehler", "Fehlern beim Speichern der Ansicht", ce.getStatus() );
      return;
    }

    setInput( newInput );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
   */
  public void init( final IEditorSite site, final IEditorInput input )
  {
    setSite( site );

    setInput( input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
   */
  public boolean isSaveAsAllowed( )
  {
    return true;
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  protected final void setInput( final IEditorInput input )
  {
    if( !(input instanceof IStorageEditorInput) )
      throw new IllegalArgumentException( "input must be instanceof IStorageEditorInput" );

    editorInput = input;
    load();
  }

  protected final void load( )
  {
    final IStorageEditorInput input = (IStorageEditorInput) getEditorInput();
    try
    {
      loadInternal( new NullProgressMonitor(), input );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( getSite().getShell(), getPartName(), "Fehler beim Laden", status );
    }

    m_commandTarget.resetDirty();

    getSite().getShell().getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        setDocumentTitle( input );
      }
    } );
  }

  protected abstract void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception, CoreException;

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    if( !(getEditorInput() instanceof IFileEditorInput) )
      return;

    final IFileEditorInput input = (IFileEditorInput) getEditorInput();

    if( event.getType() == IResourceChangeEvent.POST_CHANGE && input != null )
    {
      final IResourceDelta delta = event.getDelta().findMember( input.getFile().getFullPath() );
      if( delta != null && delta.getKind() == IResourceDelta.CHANGED )
      {
        // if its only a marker change, do not reload
        if( delta.getFlags() != IResourceDelta.MARKERS )

          // TODO: ask user?
          // if( !m_isSaving
          // && MessageDialog.openQuestion( getSite().getShell(),
          // "FeatureEditor",
          // "Die Vorlagendatei hat sich geändert. Neu laden?" ) )
          if( !m_isSaving )
            load();
      }
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final IActionBars actionBars = getActionBars( getSite() );
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );

    actionBars.updateActionBars();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    // nix
  }

  public void fireDirty( )
  {
    firePropertyChange( PROP_DIRTY );
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  public JobExclusiveCommandTarget getCommandTarget( )
  {
    return m_commandTarget;
  }

  protected void setDocumentTitle( final IStorageEditorInput input )
  {
    setPartName( input.getName() );
    if( input instanceof IFileEditorInput )
      setContentDescription( ((IFileEditorInput) input).getFile().getFullPath().toOSString() );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == ICommandTarget.class )
      return m_commandTarget;

    return super.getAdapter( adapter );
  }
}