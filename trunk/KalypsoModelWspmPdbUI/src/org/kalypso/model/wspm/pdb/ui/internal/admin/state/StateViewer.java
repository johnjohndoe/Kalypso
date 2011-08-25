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
package org.kalypso.model.wspm.pdb.ui.internal.admin.state;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.gaf.DateTimeSelectionProperty;
import org.kalypso.model.wspm.pdb.ui.internal.admin.gaf.UniqueStateNameValidator;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class StateViewer extends Composite
{
  private final DatabindingWizardPage m_binding;

  private final State m_state;

  private final Mode m_mode;

  private final IStatesProvider m_statesProvider;

  private final boolean m_editable;

  public StateViewer( final Composite parent, final DatabindingWizardPage binding, final State state, final Mode mode, final IStatesProvider statesProvider )
  {
    super( parent, SWT.NONE );

    m_binding = binding;
    m_state = state;
    m_mode = mode;
    m_statesProvider = statesProvider;
    m_editable = m_mode != Mode.VIEW;

    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( this );

    createNameControls( this );
    createCommentControls( this );
    createSourceControls( this );
    createMeasureDateControls( this );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString("StateViewer.0") ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    field.setMessage( Messages.getString("StateViewer.1") ); //$NON-NLS-1$
    field.setTextLimit( State.NAME_LIMIT );
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString("StateViewer.2") ) ); //$NON-NLS-1$

    /* Ignore own name in edit mode */
    final String ignoreName = m_mode == Mode.EDIT ? m_state.getName() : null;

    final State[] existingStates = m_statesProvider == null ? new State[0] : m_statesProvider.getStates();

    final UniqueStateNameValidator uniqueStateNameValidator = new UniqueStateNameValidator( existingStates, ignoreName );
    binder.addTargetAfterGetValidator( uniqueStateNameValidator );
    binder.addTargetBeforeSetValidator( uniqueStateNameValidator );
    // FIXME: does not work correctly: if file is changed on file page, we will not get a correct validation here
    // using a warning here at least shows the correct
    binder.addModelAfterGetValidator( new UniqueStateNameValidator( existingStates, IStatus.WARNING, ignoreName ) );

    m_binding.bindValue( binder );
  }

  private void createCommentControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString("StateViewer.3") ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.TOP, false, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( State.COMMENT_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    field.setMessage( Messages.getString("StateViewer.4") ); //$NON-NLS-1$
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_DESCRIPTION );

    m_binding.bindValue( target, model );
  }

  private void createSourceControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString("StateViewer.5") ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_SOURCE );

    m_binding.bindValue( target, model );
  }

  private void createMeasureDateControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString("StateViewer.6") ); //$NON-NLS-1$

    final DateTime dateField = new DateTime( parent, SWT.DATE | SWT.MEDIUM | SWT.DROP_DOWN );
    dateField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    dateField.setEnabled( m_editable );

// final DateTime timeField = new DateTime( parent, SWT.TIME | SWT.SHORT );
// timeField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Calendar cal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    final Date measurementDate = m_state.getMeasurementDate();
    if( measurementDate != null )
      cal.setTime( measurementDate );
    final DateTimeSelectionProperty selectionProperty = new DateTimeSelectionProperty( cal );

    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_MEASUREMENTDATE );

    final IObservableValue dateTarget = selectionProperty.observe( dateField );
    m_binding.bindValue( dateTarget, model );

    // final IObservableValue timeTarget = selectionProperty.observe( timeField );
    // m_binding.bindValue( timeTarget, model );
  }
}