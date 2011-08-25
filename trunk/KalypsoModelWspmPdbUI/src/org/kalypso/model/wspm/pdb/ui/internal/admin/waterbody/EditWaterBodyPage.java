/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyPage extends WizardPage
{
  enum Mode
  {
    NEW,
    EDIT;
  }

  // FIXME: should come from elsewhere
  public static final String[] RANK_INPUT = new String[] { Messages.getString( "EditWaterBodyPage.0" ), "I", "II", "III", "IV" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

  private final WaterBody m_waterBody;

  private DatabindingWizardPage m_binding;

  private final WaterBody[] m_existingWaterbodies;

  private final Mode m_mode;

  public EditWaterBodyPage( final String pageName, final WaterBody waterBody, final WaterBody[] existingWaterbodies, final Mode mode )
  {
    super( pageName );

    m_waterBody = waterBody;
    m_existingWaterbodies = existingWaterbodies;
    m_mode = mode;

    setTitle( Messages.getString( "EditWaterBodyPage.5" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "EditWaterBodyPage.6" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    setControl( composite );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( composite );

    m_binding = new DatabindingWizardPage( this, null );

    createNameControls( composite );
    createLabelControls( composite );
    createDirectionControls( composite );
    createOrderControls( composite );
    createDescriptionControls( composite );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "EditWaterBodyPage.7" ) ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( Messages.getString( "EditWaterBodyPage.8" ) ); //$NON-NLS-1$
    field.setTextLimit( WaterBodyConstants.NAME_LIMIT );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "EditWaterBodyPage.9" ) ) ); //$NON-NLS-1$

    final String ignoreName = m_mode == Mode.EDIT ? m_waterBody.getName() : null;

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

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_LABEL );
    final DataBinder binder = new DataBinder( target, model );

    final String ignoreLabel = m_mode == Mode.EDIT ? m_waterBody.getLabel() : null;

    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "EditWaterBodyPage.12" ) ) ); //$NON-NLS-1$
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
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    field.setMessage( Messages.getString( "EditWaterBodyPage.16" ) ); //$NON-NLS-1$

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBody.PROPERTY_DESCRIPTION );

    m_binding.bindValue( target, model );
  }
}