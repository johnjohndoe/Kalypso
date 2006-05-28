package org.kalypso.ogc.gml.schemaeditor;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.part.EditorPart;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.basics.GMLSchemaLabelProvider;
import org.kalypso.gmlschema.basics.GMLSchemaTreeContentProvider;
import org.kalypso.ui.KalypsoGisPlugin;

public class GMLSchemaEditor extends EditorPart
{
  private TreeViewer m_viewer;

  @Override
  public void doSave( final IProgressMonitor monitor )
  {
    // not supported...
  }

  @Override
  public void doSaveAs( )
  {
    // not supported...
  }

  /**
   * @see org.eclipse.ui.IEditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
   */
  @Override
  public void init( final IEditorSite site, final IEditorInput input )
  {
    setSite( site );
    setInput( input );
  }

  @Override
  public boolean isDirty( )
  {
    return false;
  }

  @Override
  public boolean isSaveAsAllowed( )
  {
    return false;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( Composite parent )
  {
    parent.setLayout( new GridLayout() );
    m_viewer = new TreeViewer( parent, SWT.V_SCROLL );
    m_viewer.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_viewer.setContentProvider( new GMLSchemaTreeContentProvider( null, false ) );
    m_viewer.setLabelProvider( new GMLSchemaLabelProvider() );
  }

  @Override
  public void setFocus( )
  {
    if( m_viewer != null )
      m_viewer.getControl().setFocus();
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
   */
  @Override
  protected final void setInput( final IEditorInput input )
  {
    super.setInput( input );
    load();
  }

  private void load( )
  {
    new Job( "Schema laden" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          final IGMLSchema gmlSchema = createSchemaFromInput();

          getSite().getShell().getDisplay().asyncExec( new Runnable()
          {
            public void run( )
            {
              final TreeViewer viewer = getViewer();
              if( viewer != null )
                viewer.setInput( gmlSchema );
            }
          } );
        }
        catch( final Exception e )
        {
          final IStatus statusFromThrowable = StatusUtilities.statusFromThrowable( e, "Fehler beim Laden der Ansicht" );

          KalypsoGisPlugin.getDefault().getLog().log( statusFromThrowable );

          return statusFromThrowable;
        }
        finally
        {
          monitor.done();
        }

        return Status.OK_STATUS;
      }
    }.schedule();
  }

  protected TreeViewer getViewer( )
  {
    return m_viewer;
  }

  protected IGMLSchema createSchemaFromInput( ) throws CoreException, MalformedURLException, GMLSchemaException
  {
    final IEditorInput editorInput = getEditorInput();
    if( editorInput instanceof IStorageEditorInput )
    {

      final IStorageEditorInput input = (IStorageEditorInput) editorInput;

      final IStorage storage = input.getStorage();
      final IFile file = (IFile) storage.getAdapter( IFile.class );
      final URL context = file == null ? null : ResourceUtilities.createURL( file );

      // if we have a context, load the schema via the cache
      if( context != null )
        // this does not load the schema from the cache but puts it at least into the cache
        return GMLSchemaCatalog.getSchema( null, context );
      else
      {
        InputStream contents = null;
        try
        {
          contents = storage.getContents();
          return GMLSchemaFactory.createGMLSchema( contents, null, context );
        }
        finally
        {
          IOUtils.closeQuietly( contents );
        }
      }
    }
    else if( editorInput instanceof GmlSchemaEditorInput )
    {
      final GmlSchemaEditorInput schemaInput = (GmlSchemaEditorInput) editorInput;

      try
      {
        return GMLSchemaCatalog.getSchema( schemaInput.getNamespace(), schemaInput.getGmlVersion(), schemaInput.getLocation() );
      }
      catch( final InvocationTargetException e )
      {
        throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    throw new IllegalArgumentException( "Invalid editor input. Must be either IStorageEditorInput or GmlSchemaEditorInput." );
  }
}
