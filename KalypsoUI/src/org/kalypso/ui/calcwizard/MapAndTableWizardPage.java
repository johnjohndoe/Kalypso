package org.kalypso.ui.calcwizard;

import java.util.Properties;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

/**
 * @author gernot
 */
public class MapAndTableWizardPage extends WizardPage
{
  private final Properties m_arguments;
  
  public MapAndTableWizardPage( final String pagetitle, final ImageDescriptor imagedesc, final Properties arguments )
  {
    super( "MapAndTableWizardPage", pagetitle, imagedesc );
    
    m_arguments = arguments;
  }
  
  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Text text = new Text( parent, SWT.NONE );
    
    text.setText( m_arguments.toString() );
    
    setControl( text );
    // TODO implement it
  }
}
