package org.kalypso.ui.wizard.ocs;

import java.util.List;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * OcsExportWizard
 * 
 * @author schlienger
 */
public class OcsExportWizard extends Wizard implements IExportWizard
{
  private IStructuredSelection m_selection;
  private OcsExportWizardResourcesPage m_resPage;
  private OcsExportWizardIdentifiersPage m_idPage;

  public OcsExportWizard( )
  {
    final IDialogSettings settings = KalypsoGisPlugin.getDefault()
        .getDialogSettings();
    
    IDialogSettings section = settings.getSection( "OcsExportWizard" ); //$NON-NLS-1$
    if( section == null )
      section = settings.addNewSection( "OcsExportWizard" ); //$NON-NLS-1$
    
    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages( )
  {
    super.addPages();

    m_resPage = new OcsExportWizardResourcesPage( m_selection );
    m_idPage = new OcsExportWizardIdentifiersPage( m_selection );
    
    addPage( m_resPage );
    addPage( m_idPage );
  }
  
  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection currentSelection )
  {
    m_selection = currentSelection;
	List selectedResources = IDE.computeSelectedResources(currentSelection);
	if (!selectedResources.isEmpty()) {
		m_selection = new StructuredSelection(selectedResources);
	}
	
	setWindowTitle( "Zeitreihenexport Wizard" );
	setNeedsProgressMonitor( true );
  }
  
  /**
   * @see org.eclipse.jface.wizard.IWizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public IWizardPage getNextPage( IWizardPage page )
  {
    if( page == m_resPage )
      m_idPage.setResourcesToExport( m_resPage.getSelectedResources() );
    
    return super.getNextPage( page );
  }
}