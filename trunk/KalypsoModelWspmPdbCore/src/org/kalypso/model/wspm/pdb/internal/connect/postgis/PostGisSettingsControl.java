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
package org.kalypso.model.wspm.pdb.internal.connect.postgis;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.connect.IPdbSettingsControl;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCoreImages;
import org.kalypso.model.wspm.pdb.internal.connect.SettingsPropertyValue;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PortValidator;

/**
 * @author Gernot Belger
 */
class PostGisSettingsControl extends Composite implements IPdbSettingsControl
{
  private final Collection<Text> m_fields = new ArrayList<>();

  private final PostgisSettings m_settings;

  private final DataBindingContext m_binding;

  public PostGisSettingsControl( final DataBindingContext binding, final Composite parent, final PostgisSettings settings )
  {
    super( parent, SWT.NONE );

    m_binding = binding;

    m_settings = settings;

    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( false ).applyTo( this );

    final StringBlankValidator hostValidator = new StringBlankValidator( IStatus.ERROR, StringBlankValidator.DEFAULT_ERROR_MESSAGE );
    createPropertyControl( "Host", SWT.NONE, PostgisSettings.PROPERTY_HOST, hostValidator ); //$NON-NLS-1$

    createPropertyControl( "Port", SWT.NONE, PostgisSettings.PROPERTY_PORT, new PortValidator( PostgisSettings.DEFAULT_PORT ) ); //$NON-NLS-1$

    final StringBlankValidator databaseValidator = new StringBlankValidator( IStatus.ERROR, StringBlankValidator.DEFAULT_ERROR_MESSAGE );
    createPropertyControl( "Database", SWT.NONE, PostgisSettings.PROPERTY_DBNAME, databaseValidator ); //$NON-NLS-1$

    final StringBlankValidator usernameValidator = new StringBlankValidator( IStatus.ERROR, StringBlankValidator.DEFAULT_ERROR_MESSAGE );
    createPropertyControl( "Username", SWT.NONE, PostgisSettings.PROPERTY_USERNAME, usernameValidator ); //$NON-NLS-1$

    final StringBlankValidator warningValidator = new StringBlankValidator( IStatus.WARNING, Messages.getString( "PostGisSettingsControl_0" ) ); //$NON-NLS-1$
    createPropertyControl( "Password", SWT.PASSWORD, PostgisSettings.PROPERTY_PASSWORD, warningValidator ); //$NON-NLS-1$
  }

  private void createPropertyControl( final String label, final int style, final String property, final IValidator... validators )
  {
    new Label( this, SWT.NONE ).setText( label );

    final Text field = new Text( this, SWT.BORDER | style );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( Messages.getString( "PostGisSettingsControl_1" ) ); //$NON-NLS-1$

    final UpdateValueStrategy targetToModel = new UpdateValueStrategy();
    for( final IValidator validator : validators )
      targetToModel.setAfterConvertValidator( validator );

    final IObservableValue target = SWTObservables.observeText( field, new int[] { SWT.Modify } );
    final IObservableValue model = new SettingsPropertyValue( m_settings, property );
    m_binding.bindValue( target, model, targetToModel, null );

    m_fields.add( field );
  }

  @Override
  public Control getControl( )
  {
    return this;
  }

  @Override
  public ImageDescriptor getPageImage( )
  {
    return WspmPdbCoreImages.IMAGE_POSTGIS_64x64;
  }

  @Override
  public void setEditable( final boolean editable )
  {
    for( final Text field : m_fields )
      field.setEditable( editable );
  }
}