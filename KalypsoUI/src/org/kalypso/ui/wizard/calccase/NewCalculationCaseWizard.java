package org.kalypso.ui.wizard.calccase;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;

/**
 * TODO: Bezeichnungen ändern
 * 
 * @author belger
 */
public class NewCalculationCaseWizard extends BasicNewResourceWizard
{
  private NewCalculationCaseCreateFolderPage m_createFolderPage;
  private SteuerparameterWizardPage m_createControlPage;

  
  
  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    super.init( workbench, currentSelection );
    setWindowTitle( "neuer Rechenfall" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.ui.wizards.newresource.BasicNewResourceWizard#initializeDefaultPageImageDescriptor()
   */
  protected void initializeDefaultPageImageDescriptor()
  {
    super.initializeDefaultPageImageDescriptor();
  //    String iconPath = "icons/full/";//$NON-NLS-1$

  // TODO: set Icon
  //    try
  //    {
  //      URL installURL = Platform.getPlugin( PlatformUI.PLUGIN_ID
  // ).getDescriptor().getInstallURL();
  //      URL url = new URL( installURL, iconPath + "wizban/newfolder_wiz.gif"
  // );//$NON-NLS-1$
  //      ImageDescriptor desc = ImageDescriptor.createFromURL( url );
  //      setDefaultPageImageDescriptor( desc );
  //    }
  //    catch( MalformedURLException e )
  //    {
  //      // Should not happen. Ignore.
  //    }
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();
    m_createFolderPage = new NewCalculationCaseCreateFolderPage( "Rechenfall", getSelection() );
    m_createControlPage = new SteuerparameterWizardPage( "Steuerparameter", m_createFolderPage );
    
    addPage( m_createFolderPage );
    addPage( m_createControlPage );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  public boolean performFinish()
  {
    // zuerst die Grunddaten erzeugen erzeugen
    final IFolder folder = m_createFolderPage.createCalculationCase();
    if( folder == null )
      return false;
    
    // jetzt die Zeitreihen aktualisieren
    if( !m_createControlPage.saveControl( folder ) )
      return false;

    // im Navigator zeigen
    selectAndReveal( folder );

    return true;
  }
  
  
  
}