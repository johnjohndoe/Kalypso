/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * Control for {@link org.kalypso.model.wspm.pdb.db.mapping.WaterBody}.
 *
 * @author Gernot Belger
 */
public class WaterBodyControl extends Composite
{
  // FIXME: should come from elsewhere
  public static final String[] RANK_INPUT = new String[] { Messages.getString( "EditWaterBodyPage.0" ), "I", "II", "III", "IV" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

  private final IDataBinding m_binding;

  private final WaterBody m_waterBody;

  private final Mode m_mode;

  private final WaterBody[] m_existingWaterbodies;

  public enum Mode
  {
    /** create new water body */
    NEW,
    /** Edit the properties */
    EDIT,
    /** only show as info, no edit */
    INFO
  }

  public WaterBodyControl( final Composite parent, final IDataBinding binding, final WaterBody waterBody, final WaterBody[] existingWaterbodies, final Mode mode )
  {
    super( parent, SWT.NONE );

    m_binding = binding;
    m_waterBody = waterBody;
    m_existingWaterbodies = existingWaterbodies;
    m_mode = mode;

    super.setLayout( GridLayoutFactory.swtDefaults().numColumns( 2 ).create() );

    createNameControls( this );
    createLabelControls( this );
    createDirectionControls( this );
    createOrderControls( this );
    createDescriptionControls( this );
  }

  private boolean isEditable( )
  {
    return m_mode == Mode.NEW || m_mode == Mode.EDIT;
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "EditWaterBodyPage.7" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( Messages.getString( "EditWaterBodyPage.8" ) ); //$NON-NLS-1$
    field.setTextLimit( WaterBodyConstants.NAME_LIMIT );

    field.setEnabled( isEditable() );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "EditWaterBodyPage.9" ) ) ); //$NON-NLS-1$

    final String ignoreName = m_mode == Mode.EDIT ? m_waterBody.getName() : null;

    if( m_existingWaterbodies != null )
      binder.addTargetAfterGetValidator( new UniqueWaterBodyNameValidator( m_existingWaterbodies, ignoreName ) );

    m_binding.bindValue( binder );
  }

  private void createLabelControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "EditWaterBodyPage.10" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( Messages.getString( "EditWaterBodyPage.11" ) ); //$NON-NLS-1$
    field.setTextLimit( WaterBody.NAME_LIMIT );

    field.setEnabled( isEditable() );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_LABEL );
    final DataBinder binder = new DataBinder( target, model );

    final String ignoreLabel = m_mode == Mode.EDIT ? m_waterBody.getLabel() : null;

    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "EditWaterBodyPage.12" ) ) ); //$NON-NLS-1$

    if( m_existingWaterbodies != null )
      binder.addTargetAfterGetValidator( new UniqueWaterBodyLabelValidator( m_existingWaterbodies, ignoreLabel ) );

    m_binding.bindValue( binder );
  }

  private void createDirectionControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "EditWaterBodyPage.13" ) ); //$NON-NLS-1$

    final ComboViewer comboViewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider() );
    comboViewer.setInput( WaterBodyConstants.STATIONING_DIRECTION.values() );

    comboViewer.getControl().setEnabled( isEditable() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( comboViewer );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_DIRECTION_OF_STATIONING );
    m_binding.bindValue( target, model );
  }

  private void createOrderControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "EditWaterBodyPage.14" ) ); //$NON-NLS-1$

    final ComboViewer comboViewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider() );
    comboViewer.setInput( RANK_INPUT );

    comboViewer.getControl().setEnabled( isEditable() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( comboViewer );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_RANK );

    final DataBinder binder = new DataBinder( target, model );
    binder.setTargetToModelConverter( new StringToRankConverter( RANK_INPUT ) );
    binder.setModelToTargetConverter( new RankToStringConverter( RANK_INPUT ) );
    m_binding.bindValue( binder );
  }

  private void createDescriptionControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString( "EditWaterBodyPage.15" ) ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.TOP, false, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( WaterBodyConstants.DESCRIPTION_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 1, 2 ) );
    field.setMessage( Messages.getString( "EditWaterBodyPage.16" ) ); //$NON-NLS-1$

    field.setEnabled( isEditable() );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_DESCRIPTION );

    m_binding.bindValue( target, model );
  }

  @Override
  public void setLayout( final Layout layout )
  {
    throw new UnsupportedOperationException();
  }
}