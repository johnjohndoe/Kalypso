package org.kalypso.ui.repository.wizard;

import java.io.File;
import java.io.FileOutputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.zml.ObservationType;

/**
 * ExportAsFileWizard
 * 
 * @author schlienger
 */
public class ExportAsFileWizard extends Wizard
{
  private DateRangeInputWizardPage m_page1;
  private FileSelectWizardPage m_page2;
  
  private static String DEFAULT_FILE = "";
  
  private final IObservation m_obs;
  protected IProject m_project = null;

  public ExportAsFileWizard( final IObservation obs )
  {
    m_obs = obs;
    
    final IDialogSettings settings = KalypsoGisPlugin.getDefault()
        .getDialogSettings();

    IDialogSettings section = settings.getSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    if( section == null )
      section = settings.addNewSection( "ExportAsFileWizard" ); //$NON-NLS-1$

    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  public void addPages( )
  {
    super.addPages();
    
    final IProject[] projects = ResourceUtilities.getSelectedProjects();
    final String fileName;
    
    if( projects.length > 0 )
    {
      m_project = projects[0];
      fileName = ResourceUtilities.makeFileFromPath( m_project.getFullPath() ).getAbsolutePath();
    }
    else
      fileName = DEFAULT_FILE;
    
    m_page1 = new DateRangeInputWizardPage();
    m_page2 = new FileSelectWizardPage( "fileselect", fileName );
    
    addPage( m_page1 );
    addPage( m_page2 );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  public void createPageControls( Composite pageContainer )
  {
    //super.createPageControls( pageContainer );
    
    setWindowTitle( "Als Datei exportieren" );
	setNeedsProgressMonitor( true );
  }
  
  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish( )
  {
    final DateRangeArgument dateRange = m_page1.getDateRange();
    final String filePath = m_page2.getFilePath();
    
    DEFAULT_FILE = filePath;
    
    FileOutputStream outs = null;
    try
    {
      final ObservationType ot = ZmlFactory.createXML( m_obs, dateRange );
      
      outs = new FileOutputStream( new File( filePath) );
      ZmlFactory.getMarshaller().marshal( ot, outs );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    finally
    {
      IOUtils.closeQuietly( outs );
    }

    if( m_project != null )
    {
      final Job refreshJob = new Job( "Projekt " + m_project.getName() + " aktualisieren" )
      {
        protected IStatus run( IProgressMonitor monitor )
        {
          try
          {
            m_project.refreshLocal( IResource.DEPTH_INFINITE, monitor );
          }
          catch( CoreException e )
          {
            e.printStackTrace();
            
            return KalypsoGisPlugin.createErrorStatus( "", e );
          }
          
          return Status.OK_STATUS;
        }
      };
      
      refreshJob.schedule();
    }
    
    return true;
  }
}
