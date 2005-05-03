package org.kalypso.ui.createGisMapView;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class CreateGisMapViewWizard extends Wizard implements INewWizard
{

  private CreateGisMapViewWizardPage page;

  private ISelection m_selection;

  public CreateGisMapViewWizard()
  {
    super();
    setNeedsProgressMonitor( true );
  }

  /**
   * Adding the page to the wizard.
   */

  public void addPages()
  {
    page = new CreateGisMapViewWizardPage( m_selection );
    addPage( page );
  }

  public boolean performFinish()
  {
    final String containerName = page.getContainerName();
    final String fileName = page.getFileName();
    IRunnableWithProgress op = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          doFinish( containerName, fileName, monitor );
        }
        catch( CoreException e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };
    try
    {
      getContainer().run( true, false, op );
    }
    catch( InterruptedException e )
    {
      return false;
    }
    catch( InvocationTargetException e )
    {
      Throwable realException = e.getTargetException();
      MessageDialog.openError( getShell(), "Error", realException.getMessage() );
      return false;
    }
    return true;
  }

  void doFinish( String containerName, String fileName, IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Creating " + fileName, 2 );
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    IResource resource = root.findMember( new Path( containerName ) );
    if( !resource.exists() || !( resource instanceof IContainer ) )
    {
      throwCoreException( "Container \"" + containerName + "\" does not exist." );
    }
    IContainer container = (IContainer)resource;
    final IFile file = container.getFile( new Path( fileName ) );
    try
    {
      InputStream stream = openContentStream();
      if( !file.exists() )
      {
        file.create( stream, true, monitor );
      }
      stream.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    monitor.worked( 1 );
    monitor.setTaskName( "Opening file for editing..." );
    getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        IWorkbenchPage workbenchPage = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
            .getActivePage();
        try
        {
          IDE.openEditor( workbenchPage, file, true );
        }
        catch( PartInitException e )
        {
          e.printStackTrace();
        }
      }
    } );
    monitor.worked( 1 );
  }

  private InputStream openContentStream() throws JAXBException, IOException
  {
    // Create GisMapView
    GM_Envelope dummyEnvelope = GeometryFactory.createGM_Envelope( 0, 0, 0, 0 );
    Gismapview gismapview = GisTemplateHelper.emptyGisView( dummyEnvelope );
    ObjectFactory mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();
    Marshaller marshaller = mapTemplateOF.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    StringWriter stringWriter = new StringWriter();
    marshaller.marshal( gismapview, stringWriter );
    String contents = stringWriter.toString();
    stringWriter.close();
    return new ByteArrayInputStream( contents.getBytes() );
  }

  private void throwCoreException( String message ) throws CoreException
  {
    IStatus status = new Status( IStatus.ERROR, "org.kalypso.ui.createGisMapView", IStatus.OK, message,
        null );
    throw new CoreException( status );
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    m_selection = selection;
  }
}