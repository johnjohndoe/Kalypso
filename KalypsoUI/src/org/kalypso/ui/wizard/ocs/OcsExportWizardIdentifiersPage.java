package org.kalypso.ui.wizard.ocs;

import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizard.ocs.idtable.IdTableViewer;

/**
 * OcsExportWizardRepositoryPage
 * 
 * @author schlienger
 */
public class OcsExportWizardIdentifiersPage extends WizardPage
{
  private IdTableViewer m_tableViewer;

  public OcsExportWizardIdentifiersPage( )
  {
    super( "Zeitreihen Zuordnung", "Zeitreihen Zuordnung", ImageProvider.IMAGE_UTIL_UPLOAD_WIZ );
    
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
    
    //m_tableViewer.setInput( m_selection );
  }

  /**
   * @param selectedResources
   */
  public void setResourcesToExport( List selectedResources )
  {
    m_tableViewer.setInput( selectedResources );
  }
}
