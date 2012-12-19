/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.IPdbSettingsControl;
import org.kalypso.model.wspm.pdb.connect.SettingsNameValue;
import org.kalypso.model.wspm.pdb.db.OpenConnectionThreadedOperation;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * A {@link org.eclipse.jface.dialogs.IDialogPage} that edits the parameters of one {@link IPdbSettings}.
 * 
 * @author Gernot Belger
 */
class SettingsPage extends WizardPage
{
  private IPdbSettings m_settings;

  private ScrolledForm m_form;

  private IAction m_validateAction;

  private StatusComposite m_validationComposite;

  private DatabindingWizardPage m_binding;

  private String m_initialName;

  public SettingsPage( final String pageName, final IPdbSettings settings )
  {
    super( pageName );

    setDescription( Messages.getString( "SettingsPage.0" ) ); //$NON-NLS-1$

    setSettings( settings );

    // This name is ok, if we come from editing a state
    m_initialName = settings == null ? StringUtils.EMPTY : settings.getName();
  }

  void setSettings( final IPdbSettings connection )
  {
    m_settings = connection;
    m_initialName = null;

    if( connection == null )
      return;

    setTitle( connection.getName() );

    m_validateAction = new ValidateSettingsAction( this );

    updateControl();
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_form = new ScrolledForm( parent );
    m_form.setExpandHorizontal( true );
    m_form.setExpandVertical( true );

    final Composite body = m_form.getBody();
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( body );

    updateControl();

    setControl( m_form );
  }

  private void updateControl( )
  {
    if( m_binding != null )
    {
      m_binding.dispose();
      m_binding = null;
    }

    if( m_form == null )
      return;

    if( m_settings == null )
      return;

    m_binding = new DatabindingWizardPage( this, null );

    final Composite body = m_form.getBody();

    createNameControl( body );
    createSettingsGroup( body );
    createTestAction( body );

    m_form.reflow( true );
  }

  private void createNameControl( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "SettingsPage.1" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( Messages.getString( "SettingsPage.2" ) ); //$NON-NLS-1$

    final IObservableValue target = SWTObservables.observeText( field, new int[] { SWT.Modify } );
    final IObservableValue model = new SettingsNameValue( m_settings );

    final DataBinder binder = new DataBinder( target, model );

    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.WARNING, StringBlankValidator.DEFAULT_WARNING_MESSAGE ) );
    binder.addTargetAfterGetValidator( new UniqueSettingsNameValidator( m_initialName ) );

    model.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        setTitle( (String) event.diff.getNewValue() );
      }
    } );

    m_binding.bindValue( binder );
  }

  private void createSettingsGroup( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "SettingsPage.3" ) ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );

    final IPdbSettingsControl editor = m_settings.createEditControl( m_binding.getBindingContext(), group );
    setImageDescriptor( editor.getPageImage() );
  }

  private void createTestAction( final Composite parent )
  {
    ActionButton.createButton( null, parent, m_validateAction );
    m_validationComposite = new StatusComposite( parent, StatusComposite.HIDE_DETAILS_IF_DISABLED | StatusComposite.DETAILS );
    m_validationComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  @Override
  public IWizardPage getPreviousPage( )
  {
    return null;
  }

  public void testConnection( )
  {
    final OpenConnectionThreadedOperation operation = new OpenConnectionThreadedOperation( m_settings, true );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    m_validationComposite.setStatus( result );
  }
}