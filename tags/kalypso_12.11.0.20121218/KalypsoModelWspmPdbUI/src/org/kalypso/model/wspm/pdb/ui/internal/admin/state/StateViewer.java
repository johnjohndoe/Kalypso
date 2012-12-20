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
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.property.value.DateTimeSelectionProperty;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.pdb.db.constants.StateConstants;
import org.kalypso.model.wspm.pdb.db.constants.StateConstants.ZeroState;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStatePage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class StateViewer extends Composite
{
  private final DatabindingWizardPage m_binding;

  private final Mode m_mode;

  private final boolean m_editable;

  private final State m_state;

  private final IValidator m_stateNameValidator;

  public StateViewer( final Composite parent, final DatabindingWizardPage binding, final State state, final Mode mode, final IValidator stateNameValidator )
  {
    super( parent, SWT.NONE );

    m_binding = binding;
    m_state = state;
    m_mode = mode;
    m_stateNameValidator = stateNameValidator;
    m_editable = m_mode != Mode.VIEW;

    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( this );

    createNameControls( this );
    createCommentControls( this );
    createSourceControls( this );
    createMeasureDateControls( this );
    createIsZeroControls( this );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "StateViewer.0" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    field.setMessage( Messages.getString( "StateViewer.1" ) ); //$NON-NLS-1$
    field.setTextLimit( State.NAME_LIMIT );
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "StateViewer.2" ) ) ); //$NON-NLS-1$

    if( m_stateNameValidator != null )
    {
      binder.addTargetAfterGetValidator( m_stateNameValidator );
      // binder.addTargetBeforeSetValidator( m_stateNameValidator );

      // FIXME: does not work correctly: if file is changed on file page, we will not get a correct validation here
      // using a warning here at least shows the correct
      // binder.addModelAfterGetValidator( m_stateNameValidator );
    }

//    final UniqueStateNameValidator uniqueStateNameValidator = new UniqueStateNameValidator( existingStates, ignoreName );
//    binder.addTargetAfterGetValidator( uniqueStateNameValidator );
//    binder.addTargetBeforeSetValidator( uniqueStateNameValidator );
//
//    // FIXME: does not work correctly: if file is changed on file page, we will not get a correct validation here
//    // using a warning here at least shows the correct
//    binder.addModelAfterGetValidator( new UniqueStateNameValidator( existingStates, IStatus.WARNING, ignoreName ) );

    m_binding.bindValue( binder );
  }

  private void createCommentControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "StateViewer.3" ) ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.TOP, false, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( State.COMMENT_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    field.setMessage( Messages.getString( "StateViewer.4" ) ); //$NON-NLS-1$
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_DESCRIPTION );

    m_binding.bindValue( target, model );
  }

  private void createSourceControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "StateViewer.5" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    field.setEditable( m_editable );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_state, State.PROPERTY_SOURCE );

    m_binding.bindValue( target, model );
  }

  private void createMeasureDateControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "StateViewer.6" ) ); //$NON-NLS-1$

    final DateTime dateField = new DateTime( parent, SWT.DATE | SWT.MEDIUM | SWT.DROP_DOWN );
    dateField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    dateField.setEnabled( m_editable );

    // final DateTime timeField = new DateTime( parent, SWT.TIME | SWT.SHORT );
    // timeField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Calendar cal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );

    // TODO: why is it necessary to initialize the target value?
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

  private void createIsZeroControls( final Composite parent )
  {
    /* new states get state 0 == on; should not be changed */
    if( m_mode == Mode.NEW )
      return;

    new Label( parent, SWT.NONE ).setText( Messages.getString("StateViewer.7") ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setInput( ZeroState.values() );

    viewer.getControl().setEnabled( m_editable );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_state, StateConstants.PROPERTY_ISSTATEZERO );
    m_binding.bindValue( targetSelection, modelSelection );
  }
}