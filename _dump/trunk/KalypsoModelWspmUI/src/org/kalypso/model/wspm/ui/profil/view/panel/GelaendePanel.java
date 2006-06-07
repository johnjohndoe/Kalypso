package org.kalypso.model.wspm.ui.profil.view.panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;


/**
 * @author gernot
 * 
 */
public class GelaendePanel extends AbstractProfilView
{
  public GelaendePanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( ) );
    
    final Group editgroup = new Group( panel, SWT.NONE );
    editgroup.setText( "Editieren" );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    editgroup.setLayoutData( gridData );
    editgroup.setLayout( new GridLayout() );

    final Button horzbutton = new Button( editgroup, SWT.CHECK );
    horzbutton.setSelection( getViewData().isEdithorz() );
    horzbutton.setText( "Horizontal" );
    horzbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setEdithorz( horzbutton.getSelection() );
      }
    } );

    final Button vertbutton = new Button( editgroup, SWT.CHECK );
    vertbutton.setSelection( getViewData().isEditvert() );
    vertbutton.setText( "Vertikal" );
    vertbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setEditvert( vertbutton.getSelection() );
      }
    } );
    
    return panel;
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
  }
}
