package org.kalypso.ui.editor.gmleditor.ui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.actions.FeatureActionUtilities;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;

/**
 * @author Küpferle
 */
public class GmlEditor extends AbstractEditorPart implements ICommandTarget
{
  public static final String ID = "org.kalypso.ui.editor.GmlEditor";

  private GmlTreeView m_viewer = null;

  @Override
  public void dispose( )
  {
    if( m_viewer != null )
      m_viewer.dispose();

    // unregister site selection provieder
    getSite().setSelectionProvider( null );
    super.dispose();
  }

  @Override
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input ) throws CoreException
  {
    ByteArrayInputStream bis = null;
    OutputStreamWriter writer = null;

    try
    {
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      writer = new OutputStreamWriter( bos );
      m_viewer.saveInput( writer, monitor );
      writer.close();

      bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      monitor.worked( 1000 );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( bis );
      IOUtils.closeQuietly( writer );
    }
  }

  public GmlTreeView getTreeView( )
  {
    return m_viewer;
  }

  @Override
  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception, CoreException
  {
    monitor.beginTask( "Vorlage wird geladen", 1000 );
    try
    {
      final IStorage storage = input.getStorage();

      final Reader r;
      if( storage instanceof IEncodedStorage )
        r = new InputStreamReader( storage.getContents(), ((IEncodedStorage) storage).getCharset() );
      else
        r = new InputStreamReader( storage.getContents() );

      final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile( storage.getFullPath() );
      final URL context = ResourceUtilities.createURL( file );

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          try
          {
            final GmlTreeView treeView = getTreeView();
            if( treeView != null )
              treeView.loadInput( r, context, monitor );
          }
          catch( final CoreException e )
          {
            e.printStackTrace();
          }
        }
      } );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Parsen der Context-URL" ) );
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

  @Override
  public synchronized void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    m_viewer = new GmlTreeView( parent, KalypsoCorePlugin.getDefault().getSelectionManager() );

    // register as site selection provider
    getSite().setSelectionProvider( m_viewer );

    createContextMenu();
  }

  private void createContextMenu( )
  {
    // create context menu for editor
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        handleMenuAboutToShow( manager );
      }
    } );

    final TreeViewer treeViewer = m_viewer.getTreeViewer();
    final Menu menu = menuManager.createContextMenu( treeViewer.getControl() );
    getSite().registerContextMenu( menuManager, m_viewer );
    treeViewer.getControl().setMenu( menu );
  }

  /**
   * Add some special actions to the menuManager, dependend on the current selection.
   */
  public void handleMenuAboutToShow( final IMenuManager manager )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();
    final IFeatureSelectionManager selectionManager = m_viewer.getSelectionManager();

    final IMenuManager newMenuManager = FeatureActionUtilities.createFeatureNewMenu( selection, selectionManager );
    manager.add( newMenuManager );

    // add additions seperator: if not, eclipse whines
    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IPostSelectionProvider.class )
      return m_viewer;

    if( adapter == ISelectionProvider.class )
      return m_viewer;

    if( adapter == ModellEventProvider.class )
      return m_viewer;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    m_viewer.getTreeViewer().getControl().setFocus();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#isDirty()
   */
  @Override
  public boolean isDirty( )
  {
    return super.isDirty() || m_viewer.isDirty();
  }
}