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
package org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;
import org.kalypso.ui.editor.styleeditor.binding.DataBinder;
import org.kalypso.ui.editor.styleeditor.binding.DatabindingWizardPage;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyPage extends WizardPage
{
  private final WaterBodies m_waterBody;

  private DatabindingWizardPage m_binding;

  private final WaterBodies[] m_existingWaterbodies;

  public EditWaterBodyPage( final String pageName, final WaterBodies waterBody, final WaterBodies[] existingWaterbodies )
  {
    super( pageName );

    m_waterBody = waterBody;
    m_existingWaterbodies = existingWaterbodies;

    setTitle( "Edit Properties" );
    setDescription( "Edit the properties of the water body" );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    setControl( composite );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( composite );

    m_binding = new DatabindingWizardPage( this, null );

    createIDControls( composite );
    createNameControls( composite );
    createCommentControls( composite );
    // private Geometry riverline;
  }

  private void createIDControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Gew‰sserkennziffer" );

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( "<Eindeutige Kennziffer des Gew‰ssers>" );
    field.setTextLimit( WaterBodies.WATERBODY_LIMIT );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBodies.PROPERTY_WATERBODY );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, "'ID' is empty" ) );
    binder.addTargetAfterGetValidator( new UniqueWaterBodyIDValidator( m_existingWaterbodies ) );

    m_binding.bindValue( binder );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Name" );

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( "<Eindeutiger Name des Gew‰ssers>" );
    field.setTextLimit( WaterBodies.NAME_LIMIT );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBodies.PROPERTY_NAME );
    final DataBinder binder = new DataBinder( target, model );

    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, "'Name' is empty" ) );
    binder.addTargetAfterGetValidator( new UniqueWaterBodyNameValidator( m_existingWaterbodies ) );

    m_binding.bindValue( binder );
  }

  private void createCommentControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Description" );
    label.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( WaterBodies.COMMENT_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    field.setMessage( "<Beschreibung des Gew‰ssers>" );

    final IObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_waterBody, WaterBodies.PROPERTY_COMMENT );

    m_binding.bindValue( target, model );
  }
}