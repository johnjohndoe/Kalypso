package org.kalypso.portal.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ui.ImageProvider;

public class LaunchProtocolView extends ViewPart
{

  private Composite m_top;

  // TODO here for testing issues -> remove soon
  public LaunchProtocolView( )
  {
  }

  @Override
  public void createPartControl( Composite parent )
  {
    m_top = new Composite( parent, SWT.NONE );
    m_top.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData();
    m_top.setLayoutData( data );
    Button button = new Button( m_top, SWT.PUSH );
    button.setText( "testButton" );
    button.setImage( ImageProvider.IMAGE_KALYPSO_ICON_BIG.createImage() );

  }

  @Override
  public void setFocus( )
  {

  }

}
