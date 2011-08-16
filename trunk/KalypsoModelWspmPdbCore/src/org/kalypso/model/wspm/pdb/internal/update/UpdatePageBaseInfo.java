/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.internal.update;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.observable.value.PropertiesObservaleValue;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.commons.databinding.validation.StringToUrlValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.version.IUpdateScriptPage;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptPageData;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * Page showing base inf ofrom the info table, in order to be entered before creation of the database.
 * 
 * @author Gernot Belger
 */
public class UpdatePageBaseInfo extends WizardPage implements IUpdateScriptPage
{
  private UpdateScriptPageData m_data;

  private DatabindingWizardPage m_binding;

  public UpdatePageBaseInfo( )
  {
    super( "basePage" ); //$NON-NLS-1$

    setTitle( "Database Properties" );
    setDescription( "Please enter some basic properties for the database." );
  }

  @Override
  public void init( final UpdateScriptPageData data )
  {
    m_data = data;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createSrsControls( panel );
    createDocumentPathControls( panel );
  }

  private void createSrsControls( final Composite panel )
  {
    final CRSSelectionPanel srsPanel = new CRSSelectionPanel( panel, SWT.NONE );
    srsPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IObservableValue targetSRS = srsPanel.observe();
    final IObservableValue modelSRS = new PropertiesObservaleValue( m_data.getVariables(), PdbInfo.PROPERTY_SRID );

    final DataBinder binder = new DataBinder( targetSRS, modelSRS );
    binder.setTargetToModelConverter( new SridToSrsConverter() );
    binder.setTargetToModelConverter( new SrsToSridConverter() );
    binder.addModelBeforeSetValidator( new StringBlankValidator( ERROR, "A valid coordinate System must be selected" ) );
    m_binding.bindValue( binder );
  }

  private void createDocumentPathControls( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().applyTo( group );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( "Document Base" );

    final Text field = new Text( group, SWT.BORDER | SWT.SINGLE );
    field.setMessage( "<Base URL>" );
    field.setToolTipText( "The base location of all attached documents" );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue targetBase = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue modelBase = new PropertiesObservaleValue( m_data.getVariables(), PdbInfo.PROPERTY_DOCUMENT_SERVER );

    final TestDocumentBaseAction testAction = new TestDocumentBaseAction( modelBase );
    final ImageHyperlink hyperlink = ActionHyperlink.createHyperlink( null, group, SWT.PUSH, testAction );
    hyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final DataBinder binder = new DataBinder( targetBase, modelBase );
    binder.addTargetAfterGetValidator( new StringToUrlValidator() );
    m_binding.bindValue( binder );
  }
}