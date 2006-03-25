package org.kalypso.ogc.gml.schemaeditor;

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
          // final URL url = input.getStorage().getContents().gettoFile().toURL();
          final GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema( input.getStorage().getContents(), null );
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
          e.printStackTrace();

          monitor.done();
          return e.getStatus();
        }
        catch( final Exception e )
        {
          e.printStackTrace();

          monitor.done();
          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Laden der Ansicht", e );
        }
        // getEditorSite().getShell().getDisplay().syncExec( new Runnable()
        // {
        // public void run( )
        // {
        // if( schema != null )
        // setPartName( "Schema: " + schema.getTargetNamespace() );
        // else
        // setPartName( "Schema: <kein Schema>" );
        // }
        // } );
        monitor.done();
        return Status.OK_STATUS;
      }
    }.schedule();
  }
}
