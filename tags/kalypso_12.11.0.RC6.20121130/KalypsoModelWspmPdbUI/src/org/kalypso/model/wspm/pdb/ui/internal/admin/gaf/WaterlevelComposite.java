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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.conversion.IConverter;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
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
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.conversion.IdentityConverter;
import org.kalypso.commons.databinding.conversion.MapConverter;
import org.kalypso.commons.databinding.property.value.DateTimeSelectionProperty;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.IEditEventPageData;

/**
 * @author Gernot Belger
 */
public class WaterlevelComposite extends Composite
{
  public static final int SHOW_MEASUREMENT_DATE = 1 << 1;

  private final Event m_event;

  private final IDataBinding m_binding;

  private ComboViewer m_typeField;

  private Text m_sourceField;

  private Text m_nameField;

  private UniqueEventNameValidator m_uniqueEventNameValidator;

  private Binding m_nameBinding;

  private final String m_ignoreName;

  private final int m_style;

  private DateTime m_measurementField;

  private Text m_descriptionField;

  private ComboViewer m_stateField;

  private final IEditEventPageData m_data;

  public WaterlevelComposite( final Composite parent, final int style, final IEditEventPageData data, final IDataBinding binding, final String ignoreName )
  {
    super( parent, SWT.NONE );

    m_style = style;
    m_data = data;
    m_event = data.getEvent();
    m_binding = binding;
    m_ignoreName = ignoreName;

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    createNameControls( this );
    createSourceControls( this );
    createDescriptionControls( this );
    createTypeControls( this );

    if( (m_style & SHOW_MEASUREMENT_DATE) != 0 )
      createMeasurementControls( this );

    if( data.showStatesChooser() )
      createStateControls( this );

    /* update name controls if state or water body changes */
    m_event.addPropertyChangeListener( new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        final String property = evt.getPropertyName();
        if( Event.PROPERTY_WATER_BODY.equals( property ) || Event.PROPERTY_STATE.equals( property ) )
          updateExistingEvents();
      }
    } );

    updateExistingEvents();
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "WaterlevelComposite.0" ) ); //$NON-NLS-1$

    m_nameField = new Text( parent, SWT.BORDER | SWT.SINGLE );
    m_nameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_nameField.setMessage( Messages.getString( "WaterlevelComposite.1" ) ); //$NON-NLS-1$

    final ISWTObservableValue target = SWTObservables.observeText( m_nameField, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_event, Event.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "WaterlevelComposite.2" ) ) ); //$NON-NLS-1$
    m_uniqueEventNameValidator = new UniqueEventNameValidator( m_ignoreName );
    binder.addTargetAfterGetValidator( m_uniqueEventNameValidator );

    m_nameBinding = m_binding.bindValue( binder );
  }

  private void createSourceControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "WaterlevelComposite.3" ) ); //$NON-NLS-1$

    m_sourceField = new Text( parent, SWT.BORDER | SWT.SINGLE );
    m_sourceField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_sourceField.setMessage( Messages.getString( "WaterlevelComposite.4" ) ); //$NON-NLS-1$

    final ISWTObservableValue target = SWTObservables.observeText( m_sourceField, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_event, Event.PROPERTY_SOURCE );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.WARNING, Messages.getString( "WaterlevelComposite.5" ) ) ); //$NON-NLS-1$

    m_binding.bindValue( binder );
  }

  private void createDescriptionControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "WaterlevelComposite.6" ) ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.TOP, false, false ) );

    m_descriptionField = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    m_descriptionField.setTextLimit( Event.DESCRIPTION_LIMIT );
    m_descriptionField.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_descriptionField.setMessage( Messages.getString( "WaterlevelComposite.7" ) ); //$NON-NLS-1$

    final IObservableValue target = SWTObservables.observeText( m_descriptionField, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_event, State.PROPERTY_DESCRIPTION );

    m_binding.bindValue( target, model );
  }

  private void createTypeControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "WaterlevelComposite.8" ) ); //$NON-NLS-1$

    m_typeField = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_typeField.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_typeField.setContentProvider( new ArrayContentProvider() );
    m_typeField.setLabelProvider( new LabelProvider() );
    m_typeField.setInput( Event.TYPE.values() );

    final IObservableValue target = ViewersObservables.observeSinglePostSelection( m_typeField );
    final IObservableValue model = BeansObservables.observeValue( m_event, Event.PROPERTY_TYPE );

    final DataBinder binder = new DataBinder( target, model );
    m_binding.bindValue( binder );
  }

  private void createMeasurementControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "WaterlevelComposite.9" ) ); //$NON-NLS-1$

    m_measurementField = new DateTime( parent, SWT.DATE | SWT.MEDIUM | SWT.DROP_DOWN );
    m_measurementField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Calendar cal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    final Date measurementDate = m_event.getMeasurementDate();
    if( measurementDate != null )
      cal.setTime( measurementDate );
    final DateTimeSelectionProperty selectionProperty = new DateTimeSelectionProperty( cal );

    final IObservableValue model = BeansObservables.observeValue( m_event, Event.PROPERTY_MEASUREMENTDATE );
    final IObservableValue dateTarget = selectionProperty.observe( m_measurementField );

    m_binding.bindValue( dateTarget, model );
  }

  private void createStateControls( final Composite parent )
  {
    final String tooltip = Messages.getString("WaterlevelComposite.10"); //$NON-NLS-1$

    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString("WaterlevelComposite.11") ); //$NON-NLS-1$
    label.setToolTipText( tooltip );

    m_stateField = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_stateField.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_stateField.setContentProvider( new ArrayContentProvider() );
    m_stateField.setLabelProvider( new LabelProvider() );

    /* binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( m_stateField );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, IEditEventPageData.PROPERTY_STATES );
    m_binding.bindValue( targetInput, modelInput );

    final IObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( m_stateField );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_event, Event.PROPERTY_STATE );

    final DataBinder binder = new DataBinder( targetSelection, modelSelection );

    /* Converter for speical element, because ComboVier input may not be null. */
    final IdentityConverter<State> identity = new IdentityConverter<>( State.class );
    final IConverter nullToSpecial = new MapConverter<>( State.class, State.class, Collections.singletonMap( (State)null, IEditEventPageData.NO_EVENT_STATE ), identity );
    final IConverter specialToNull = new MapConverter<>( State.class, State.class, Collections.singletonMap( IEditEventPageData.NO_EVENT_STATE, (State)null ), identity );

    binder.setModelToTargetConverter( nullToSpecial );
    binder.setTargetToModelConverter( specialToNull );

    binder.addTargetAfterConvertValidator( new NotNullValidator<>( Object.class, IStatus.INFO, Messages.getString("WaterlevelComposite.12") ) ); //$NON-NLS-1$

    m_binding.bindValue( binder );
  }

  @Override
  public void setEnabled( final boolean enabled )
  {
    super.setEnabled( enabled );

    m_nameField.setEnabled( enabled );
    m_sourceField.setEnabled( enabled );
    m_descriptionField.setEnabled( enabled );
    m_typeField.getControl().setEnabled( enabled );

    if( m_stateField != null )
      m_stateField.getControl().setEnabled( enabled );

    if( m_measurementField != null )
      m_measurementField.setEnabled( enabled );
  }

  void updateExistingEvents( )
  {
    try
    {
      final Event[] existingEvents = m_data.getExistingEvents();

      m_uniqueEventNameValidator.setExistingEvents( existingEvents );

      m_nameBinding.validateTargetToModel();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString("WaterlevelComposite.13"), e ); //$NON-NLS-1$
      StatusDialog.open( getShell(), status, Messages.getString("WaterlevelComposite.14") ); //$NON-NLS-1$
    }
  }
}