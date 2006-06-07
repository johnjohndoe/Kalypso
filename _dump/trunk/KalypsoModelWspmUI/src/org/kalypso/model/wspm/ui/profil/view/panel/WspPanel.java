package org.kalypso.model.wspm.ui.profil.view.panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.core.result.IResultSet.TYPE;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;


/**
 * @author belger
 */
public class WspPanel extends AbstractProfilView
{
  private static final String VALUE_FORMAT = "%.2f";

  private Composite m_panel;

  private final IStationResult m_result;

  public WspPanel( final IProfilEventManager pem, final ProfilViewData viewdata, final IStationResult result )
  {
    super( pem, viewdata, null );

    m_result = result;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    m_panel = new Composite( parent, style );
    m_panel.setLayout( new GridLayout( 2, false ) );

    final TYPE[] types = m_result.getTypes();
    for( final TYPE type : types )
    {
      final Label typeLabel = new Label( m_panel, SWT.NONE );
      typeLabel.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING | GridData.FILL_HORIZONTAL ) );
      typeLabel.setText( type.toString() + " [" + type.getUnit() + "] :" );

      final Text typeText = new Text( m_panel, SWT.TRAIL | SWT.SINGLE );
      typeText.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL ) );
      typeText.setEditable( false );
      typeText.setText( String.format( VALUE_FORMAT, m_result.getValue( type ) ) );
    }

    return m_panel;
  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

}
