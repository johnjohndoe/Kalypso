package org.kalypso.services.ocs.wizard;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.services.ocs.wizard.idtable.IdTableViewer;

/**
 * OcsExportWizardRepositoryPage
 * 
 * @author schlienger
 */
public class OcsExportWizardIdentifiersPage extends WizardPage
{
  private IStructuredSelection m_selection;
  private IdTableViewer m_tableViewer;

  public OcsExportWizardIdentifiersPage( final IStructuredSelection selection )
  {
    super( "Zeitreihen Zuordnung" );
    
    m_selection = selection;
    
    setTitle( "Zeitreihen Zuordnung" );
    setDescription( "Überprüfen Sie die Zuordnung der Zeitreihen mit dem Server" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_tableViewer = new IdTableViewer( parent, SWT.BORDER | SWT.SINGLE );
    
    setControl( m_tableViewer.getControl() );
    
    m_tableViewer.setInput( m_selection );
  }
}
