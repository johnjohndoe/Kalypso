package org.kalypso.model.wspm.ui.profil.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A Frame, which holds exactly one ProfilView
 * 
 * @author gernot
 *  
 */
public class ProfilViewFrame
{
  private final IProfilView m_view;

  public ProfilViewFrame( final IProfilView view )
  {
    m_view = view;
  }

  public void open( final Shell shell )
  {
    shell.setLayout( new GridLayout() );

    // create view
    final Control viewControl = m_view.createControl( shell, SWT.NONE );
    viewControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    
    // open shell
    shell.open();
  }
  
  public void dispose( )
  {
    m_view.dispose();
  }
}