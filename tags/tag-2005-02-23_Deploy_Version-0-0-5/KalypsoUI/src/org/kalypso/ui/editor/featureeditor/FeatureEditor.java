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
package org.kalypso.ui.editor.featureeditor;

import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * <p>
 * Eclipse-Editor zum Editieren von Features
 * </p>
 * 
 * @author belger
 */
public class FeatureEditor extends EditorPart
{
  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run()
    {
      final Shell shell = getSite().getShell();
      if( shell != null )
      {
        shell.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            fireDirtyChange();
          }
        } );
      }
    }
  };

  protected final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget(
      new DefaultCommandManager(), m_dirtyRunnable );

  private final FeatureTemplateviewer m_viewer = new FeatureTemplateviewer( m_commandTarget );

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    m_commandTarget.dispose();

    m_viewer.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.ISaveablePart#isSaveAsAllowed()
   */
  public boolean isSaveAsAllowed()
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSaveAs()
   */
  public void doSaveAs()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
   *      org.eclipse.ui.IEditorInput)
   */
  public void init( final IEditorSite site, final IEditorInput input ) throws PartInitException
  {
    if( !( input instanceof IStorageEditorInput ) )
      throw new PartInitException( "Can only use IStorageEditorInput" );

    setSite( site );

    setInput( input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  protected final void setInput( final IEditorInput input )
  {
    if( !( input instanceof IStorageEditorInput ) )
      throw new IllegalArgumentException( "Only IStorageEditorInput supported" );

    super.setInput( input );

    load( (IStorageEditorInput)input );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void doSave( final IProgressMonitor monitor )
  {
    try
    {
      m_viewer.saveGML( monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = KalypsoGisPlugin.createErrorStatus( "", e );
      ErrorDialog.openError( getSite().getShell(), "Speichern", "Fehler beim Speichern der Daten",
          status );
    }
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty()
  {
    return m_commandTarget.isDirty();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // nix
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_viewer.createControls( parent, SWT.NONE );

    final IActionBars actionBars = getEditorSite().getActionBars();
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );
    actionBars.updateActionBars();
  }

  protected final void load( final IStorageEditorInput input )
  {
    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        loadInput( input, monitor );
      }
    };

    final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
    try
    {
      progressService.busyCursorWhile( op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final Throwable targetException = e.getTargetException();

      final IStatus status;
      if( targetException instanceof CoreException )
        status = ( (CoreException)targetException ).getStatus();
      else
      {
        final String locmsg = targetException.getLocalizedMessage();
        final String msg = locmsg == null ? "" : locmsg;
        status = KalypsoGisPlugin.createErrorStatus( msg, targetException );
      }

      ErrorDialog.openError( getEditorSite().getShell(), "Fehler", "Fehler beim Laden der Ansicht",
          status );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
  }

  protected final void loadInput( final IStorageEditorInput input, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );

    try
    {
      final IStorage storage = input.getStorage();

      final Reader r;
      if( storage instanceof IEncodedStorage )
        r = new InputStreamReader( storage.getContents(), ( (IEncodedStorage)storage ).getCharset() );
      else
        r = new InputStreamReader( storage.getContents() );

      final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile( storage.getFullPath() );
      final URL context = ResourceUtilities.createURL( file );

      m_viewer.loadInput( r, context, monitor, new Properties() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Parsen der Context-URL", e ) );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Lesen von XML", e ) );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw e;
    }
    finally
    {
      monitor.done();
    }
  }

  protected void fireDirtyChange()
  {
    firePropertyChange( PROP_DIRTY );
  }
}