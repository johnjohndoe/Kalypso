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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import java.util.Calendar;
import java.util.List;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.pdb.db.mapping.States;
import org.kalypso.ui.editor.styleeditor.binding.DataBinder;
import org.kalypso.ui.editor.styleeditor.binding.DatabindingWizardPage;

/**
 * @author Gernot Belger
 */
public class EditStatePage extends WizardPage
{
  private final States m_state;

  private DatabindingWizardPage m_binding;

  private final List<States> m_existingStates;

  public EditStatePage( final String pageName, final States state, final List<States> existingStates )
  {
    super( pageName );

    m_state = state;
    m_existingStates = existingStates;

    setTitle( "Enter State Properties" );
    setDescription( "Enter the properties of the freshly created state" );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createNameControls( panel );
    createCommentControls( panel );

    createSourceControls( panel );
    createMeasureDateControls( panel );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Name" );

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    field.setMessage( "<Eindeutiger Name des Zustands>" );
    field.setTextLimit( States.NAME_LIMIT );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_STATE );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, "'Name' is empty" ) );
    binder.addTargetAfterGetValidator( new UniqueStateNameValidator( m_existingStates ) );
    binder.addTargetBeforeSetValidator( new UniqueStateNameValidator( m_existingStates ) );
    // FIXME: does not work correctly: if file is changed on file page, we will not get a correct validation here
    // using a warning here at least shows the correct
    binder.addModelAfterGetValidator( new UniqueStateNameValidator( m_existingStates, IStatus.WARNING ) );

    m_binding.bindValue( binder );
  }

  private void createCommentControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Description" );
    label.setLayoutData( new GridData( SWT.LEFT, SWT.TOP, false, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( States.COMMENT_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    field.setMessage( "<Beschreibung des Zustands>" );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_COMMENT );

    m_binding.bindValue( target, model );
  }

  private void createSourceControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Source" );

    final Text field = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_SOURCE );

    m_binding.bindValue( target, model );
  }

  private void createMeasureDateControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Measurement Date" );

    final DateTime dateField = new DateTime( parent, SWT.DATE | SWT.MEDIUM | SWT.DROP_DOWN );
    dateField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final DateTime timeField = new DateTime( parent, SWT.TIME | SWT.SHORT );
    timeField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Calendar cal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    cal.setTime( m_state.getMeasurementDate() );
    final DateTimeSelectionProperty selectionProperty = new DateTimeSelectionProperty( cal );

    final IObservableValue dateTarget = selectionProperty.observe( dateField );
    final IObservableValue timeTarget = selectionProperty.observe( timeField );

    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_MEASUREMENTDATE );

    m_binding.bindValue( dateTarget, model );
    m_binding.bindValue( timeTarget, model );
  }
}