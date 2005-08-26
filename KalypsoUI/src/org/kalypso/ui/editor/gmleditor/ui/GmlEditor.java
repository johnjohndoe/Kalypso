package org.kalypso.ui.editor.gmleditor.ui;

import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IEncodedStorage;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * @author Küpferle
 */
public class GmlEditor extends AbstractEditorPart implements ICommandTarget
{
  protected GmlTreeView m_viewer = null;

  public void dispose()
  {
    if( m_viewer != null )
      m_viewer.dispose();
    //unregister site selection provieder
    getSite().setSelectionProvider( null );
    super.dispose();
  }

  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input )
  {
  // not implemented
  }

  public GmlTreeView getTreeView()
  {
    return m_viewer;
  }

  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception,
      CoreException
  {

    if( !( input instanceof IStorageEditorInput ) )
      throw new IllegalArgumentException( "Only IStorageEditorInput supported" );

    monitor.beginTask( "Vorlage wird geladen", 1000 );
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

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {

        public void run()
        {
          try
          {
            if( m_viewer != null )
              m_viewer.loadInput( r, context, monitor );
          }
          catch( CoreException e )
          {
            e.printStackTrace();
          }

        }
      } );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Parsen der Context-URL") );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen von XML" ) );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Laden der Vorlagendatei." ) );
    }
    finally
    {
      monitor.done();
    }
  }

  public synchronized void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );
    m_viewer = new GmlTreeView( parent );
    //register as site selection provider
    getSite().setSelectionProvider( m_viewer );
    createContextMenu();
  }

  /**
   *  
   */
  private void createContextMenu()
  {
    //  create context menu for editor
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator() );
      }
    } );
    TreeViewer treeViewer = m_viewer.getTreeViewer();
    Menu menu = menuManager.createContextMenu( treeViewer.getControl() );
    getSite().registerContextMenu( menuManager, m_viewer );
//    getSite().registerContextMenu( menuManager, m_viewer );
    treeViewer.getControl().setMenu( menu );
//    m_viewer.getControl().setMenu( menu );

  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IPostSelectionProvider.class )
      return m_viewer;
    else if( adapter == ISelectionProvider.class )
      return m_viewer;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#setFocus()
   */
  public void setFocus()
  {
    m_viewer.getTreeViewer().getControl().setFocus();
  }



}