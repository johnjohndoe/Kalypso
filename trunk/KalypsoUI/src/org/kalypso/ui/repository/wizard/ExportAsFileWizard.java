package org.kalypso.ui.repository.wizard;

import java.io.File;
import java.io.FileOutputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
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
    
    m_page1 = new DateRangeInputWizardPage();
    m_page2 = new FileSelectWizardPage( "fileselect", DEFAULT_FILE );
    
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
    
    return true;
  }
}
