/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.auth.ui;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.auth.ImageProvider;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.commons.java.util.StringUtilities;

/**
 * The KalypsoLoginDialog allows to enter user login information as well as make scenario selection. Both are optional
 * since Kalypso has various login possibilities depending on the installation.
 * 
 * @author schlienger
 */
public class KalypsoLoginDialog extends TitleAreaDialog
{
  private static final String STR_SCENARIO_TOOLTIP = "Mit Szenarios werden zum Beispiel spezielle Anwendungsfälle"
      + " wie Katastrophentest unterstützt";
  private final boolean m_userNameChangeable;
  private String m_userName;

  private final boolean m_passwordEnabled;
  private String m_passerword;

  private final boolean m_useScenarios;
  private IScenario m_selectedScenario;
  private IScenario[] m_scenarios;

  private final String m_message;
  private final static String m_title = "Kalypso - Login";
  private Image m_image;

  private ListViewer m_sceViewer;
  private Text m_txtPass;
  private Text m_txtName;
  private ISelectionChangedListener m_listener;

  public KalypsoLoginDialog( final Shell shell, final String msg, final boolean isUserNameChangeable,
      final String defaultUserName, final boolean isPasswordEnabled, final boolean isAskForScenario,
      final IScenario[] scenarios )
  {
    super( shell );

    m_message = msg;

    m_userNameChangeable = isUserNameChangeable;
    m_userName = defaultUserName;
    m_passwordEnabled = isPasswordEnabled;
    m_useScenarios = isAskForScenario;
    m_scenarios = scenarios;

    setShellStyle( SWT.SYSTEM_MODAL | SWT.ON_TOP );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );

    final GridLayout gridLayout = new GridLayout( 2, false );
    composite.setLayout( gridLayout );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Label lblName = new Label( composite, SWT.LEFT );
    lblName.setText( "Benutzername:" );
    m_txtName = new Text( composite, SWT.LEFT | SWT.BORDER );
    m_txtName.setText( m_userName );
    m_txtName.setEditable( m_userNameChangeable );
    m_txtName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Label lblPass = new Label( composite, SWT.LEFT );
    lblPass.setText( "Passwort:" );
    m_txtPass = new Text( composite, SWT.LEFT | SWT.PASSWORD | SWT.BORDER );
    m_txtPass.setEnabled( m_passwordEnabled );
    m_txtPass.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    if( m_useScenarios )
    {
      // separator
      final GridData gd = new GridData( GridData.FILL_HORIZONTAL );
      gd.horizontalSpan = 2;
      new Label( composite, SWT.SEPARATOR | SWT.HORIZONTAL ).setLayoutData( gd );

      final Label lblSce = new Label( composite, SWT.LEFT );
      lblSce.setText( "Wählen Sie bitte ein Szenario aus:" );
      lblSce.setToolTipText( STR_SCENARIO_TOOLTIP );
      final GridData gdSce = new GridData( GridData.FILL, GridData.FILL, true, false, 2, 1 );
      lblSce.setLayoutData( gdSce );

      // fake label in order to eat one column
      new Label( composite, SWT.NONE );

      // scenario selection
      m_sceViewer = new ListViewer( composite, SWT.SINGLE | SWT.BORDER );
      m_sceViewer.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
      m_sceViewer.add( m_scenarios );
      m_sceViewer.getControl().setToolTipText( STR_SCENARIO_TOOLTIP );

      // fake label
      new Label( composite, SWT.NONE );

      final Text txtDesc = new Text( composite, SWT.LEFT | SWT.WRAP );
      txtDesc.setEditable( false );
      final GridData gdDesc = new GridData( GridData.FILL, GridData.FILL, true, true );
      gdDesc.heightHint = convertHeightInCharsToPixels( 2 );
      txtDesc.setLayoutData( gdDesc );

      m_listener = new ISelectionChangedListener()
      {
        public void selectionChanged( final SelectionChangedEvent event )
        {
          handleScenarioSelected( txtDesc, event );
        }
      };

      m_sceViewer.addSelectionChangedListener( m_listener );

      // default select first item
      if( m_scenarios.length > 0 )
      {
        m_selectedScenario = m_scenarios[0];
        m_sceViewer.setSelection( new StructuredSelection( m_scenarios[0] ), true );
      }
    }

    m_image = ImageProvider.IMAGE_LOGIN.createImage();
    setTitleImage( m_image );
    setTitle( m_title );
    setMessage( m_message );

    setMyFocus();

    return composite;
  }

  /**
   * Helper that sets the focus according to dialog settings
   */
  private void setMyFocus()
  {
    if( m_userNameChangeable )
      m_txtName.setFocus();
    else if( m_passwordEnabled )
      m_txtPass.setFocus();
    else if( m_useScenarios )
      m_sceViewer.getList().setFocus();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed()
  {
    m_userName = m_txtName == null ? null : m_txtName.getText();
    m_passerword = m_txtPass == null ? null : m_txtPass.getText();

    super.okPressed();
  }

  @Override
  public boolean close()
  {
    if( m_listener != null && m_sceViewer != null )
      m_sceViewer.removeSelectionChangedListener( m_listener );

    if( m_image != null )
      m_image.dispose();

    return super.close();
  }

  public IScenario getSelectedScenario()
  {
    return m_selectedScenario;
  }

  public String getUserName()
  {
    return m_userName;
  }

  public String getPassword()
  {
    return m_passerword;
  }

  protected void handleScenarioSelected( final Text txtDesc, final SelectionChangedEvent event )
  {
    final IStructuredSelection sel = (IStructuredSelection)event.getSelection();
    m_selectedScenario = (IScenario)sel.getFirstElement();
    String desc = m_selectedScenario.getDescription();

    if( desc == null )
      desc = "";

    txtDesc.setText( desc );
    txtDesc.setToolTipText( StringUtilities.spanOverLines( desc, 70, true, StringUtilities.ALIGNMENT_LEFT ) );
  }
}
