package org.kalypso.services.ocs.wizard;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.dialogs.WizardExportResourcesPage;

/**
 * OcsExportWizardResourcesPage
 * 
 * @author schlienger
 */
public class OcsExportWizardResourcesPage extends WizardExportResourcesPage
{
  protected Button m_guessIdentifiersCheckBox;

  private final static String STORE_GUESS_IDS = "OcsExportWizardResourcesPage.STORE_GUESS_IDS"; //$NON-NLS-1$

  public OcsExportWizardResourcesPage( final IStructuredSelection selection )
  {
    super( "Zeitreihen Selektion", selection );
    
    setTitle( "Zeitreihen Selektion" );
    setDescription( "Wählen Sie die Dateien die auf dem Server zurückgespeichert werden sollen." );
  }

  /**
   * @see org.eclipse.ui.dialogs.WizardExportResourcesPage#createDestinationGroup(org.eclipse.swt.widgets.Composite)
   */
  protected void createDestinationGroup( Composite parent )
  {
    // empty
  }
  
  /**
   * @see org.eclipse.ui.dialogs.WizardDataTransferPage#createOptionsGroupButtons(org.eclipse.swt.widgets.Group)
   */
  protected void createOptionsGroupButtons( Group optionsGroup )
  {
    m_guessIdentifiersCheckBox = new Button( optionsGroup, SWT.CHECK | SWT.LEFT );
    m_guessIdentifiersCheckBox.setText( "Kennzeichen aus Metadaten benutzen" );

    m_guessIdentifiersCheckBox.setSelection( true );
  }
  
  /**
   * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
   */
  public void handleEvent( Event event )
  {
    // empty
  }
}