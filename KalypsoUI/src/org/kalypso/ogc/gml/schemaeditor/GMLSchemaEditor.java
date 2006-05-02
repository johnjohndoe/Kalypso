package org.kalypso.ogc.gml.schemaeditor;

import java.net.URL;

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
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.basics.GMLSchemaLabelProvider;
import org.kalypso.gmlschema.basics.GMLSchemaTreeContentProvider;
import org.kalypso.ui.KalypsoGisPlugin;

public class GMLSchemaEditor extends EditorPart
{

  TreeViewer m_viewer;

  public GMLSchemaEditor( )
  {
    super();
    // TODO Auto-generated constructor stub
  }

  @Override
  public void doSave( IProgressMonitor monitor )
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
  public void init( IEditorSite site, IEditorInput input )
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
    // m_viewer.setContentProvider( new GMLSchemaTreeContentProvider( null, true ) );
    m_viewer.setContentProvider( new GMLSchemaTreeContentProvider( null, false ) );
    m_viewer.setLabelProvider( new GMLSchemaLabelProvider() );
  }

  @Override
  public void setFocus( )
  {
    // TODO Auto-generated method stub

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
        final IStorageEditorInput input = (IStorageEditorInput) getEditorInput();
        try
        {
          // TODO: we get problems here because this is not loaded via the schema cache, but inside,
          // we load depending schemata from the cache

          final IStorage storage = input.getStorage();
          final IFile file = (IFile) storage.getAdapter( IFile.class );
          final URL context = file == null ? null : ResourceUtilities.createURL( file );

          final GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema( storage.getContents(), context );
          if( m_viewer != null )
          {
            m_viewer.getControl().getDisplay().asyncExec( new Runnable()
            {
              public void run( )
              {
                m_viewer.setInput( gmlSchema );
              }

            } );
          }
        }
        catch( final CoreException e )
        {
          final IStatus status = e.getStatus();
          
          KalypsoGisPlugin.getDefault().getLog().log( status );

          return status;
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
}
