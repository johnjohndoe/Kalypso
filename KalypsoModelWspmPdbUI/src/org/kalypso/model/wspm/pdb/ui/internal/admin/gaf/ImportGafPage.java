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

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.conversion.FileToStringConverter;
import org.kalypso.commons.databinding.conversion.StringToFileConverter;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.FileValueSelectionListener;
import org.kalypso.commons.databinding.validation.FileIsFileValidator;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * @author Gernot Belger
 */
public class ImportGafPage extends WizardPage
{
  private final ImportGafData m_data;

  private DatabindingWizardPage m_context;

  public ImportGafPage( final String pageName, final ImportGafData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "ImportGafPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportGafPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_context = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    createGafPathControl( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createSrsControl( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    // dokumente (images/dxf)
  }

  private Control createGafPathControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "ImportGafPage.2" ) ); //$NON-NLS-1$

    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( false ).applyTo( group );

    final Text text = new Text( group, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "ImportGafPage.3" ) ); //$NON-NLS-1$

    final Button fileSelectButton = new Button( group, SWT.PUSH );
    fileSelectButton.setText( "..." ); //$NON-NLS-1$

    /* Binding */
    final ISWTObservableValue target = SWTObservables.observeText( text, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_GAF_FILE );

    final DataBinder binder = new DataBinder( target, model );

    binder.setTargetToModelConverter( new StringToFileConverter() );
    binder.setModelToTargetConverter( new FileToStringConverter() );

    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "ImportGafPage.5" ) ) ); //$NON-NLS-1$
    binder.addTargetAfterConvertValidator( new FileIsFileValidator( IStatus.ERROR ) );

    m_context.bindValue( binder );

    final String titel = Messages.getString( "ImportGafPage.6" ); //$NON-NLS-1$
    final FileValueSelectionListener fileListener = new FileValueSelectionListener( model, titel, SWT.OPEN );
    fileListener.addFilter( Messages.getString( "ImportGafPage.7" ), "*.gaf" ); //$NON-NLS-1$ //$NON-NLS-2$
    fileListener.addAllFilter();
    fileSelectButton.addSelectionListener( fileListener );

    return group;
  }

  private Control createSrsControl( final Composite parent )
  {
    final CRSSelectionPanel crsPanel = new CRSSelectionPanel( parent, SWT.NONE );
    crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IObservableValue crsValue = crsPanel.observe();
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_SRS );

    final IValidator notNullValidator = new NotNullValidator<>( String.class, IStatus.ERROR, Messages.getString( "ImportGafPage.9" ) ); //$NON-NLS-1$

    m_context.bindValue( crsValue, model, notNullValidator );

    return crsPanel;
  }
}