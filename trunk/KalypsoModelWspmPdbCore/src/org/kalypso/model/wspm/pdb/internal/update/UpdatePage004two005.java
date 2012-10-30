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
package org.kalypso.model.wspm.pdb.internal.update;

import java.util.Properties;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.conversion.FileToStringConverter;
import org.kalypso.commons.databinding.conversion.StringToFileConverter;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.observable.value.PropertiesObservaleValue;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.version.IUpdateScriptPage;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptPageData;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * Page showing base inf ofrom the info table, in order to be entered before creation of the database.
 *
 * @author Gernot Belger
 */
public class UpdatePage004two005 extends WizardPage implements IUpdateScriptPage
{
  private UpdateScriptPageData m_data;

  private DatabindingWizardPage m_binding;

  public UpdatePage004two005( )
  {
    super( "basePage" ); //$NON-NLS-1$

    setTitle( Messages.getString( "UpdatePageBaseInfo.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString("UpdatePage004two005.0") ); //$NON-NLS-1$
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

    final Properties variables = m_data.getVariables();

    createDemPathControls( panel, m_binding, variables );
  }

  static void createDemPathControls( final Composite parent, final DatabindingWizardPage binding, final Properties variables )
  {
    final FileAndHistoryData fileData = new FileAndHistoryData( "temp" ); //$NON-NLS-1$
    final IObservableValue modelDir = BeansObservables.observeValue( fileData, FileAndHistoryData.PROPERTY_FILE );

    /* first bind to our internal property as string */
    final IObservableValue modelProperty = new PropertiesObservaleValue( variables, PdbInfo.PROPERTY_DEM_SERVER );
    final DataBinder propertyBinder = new DataBinder( modelDir, modelProperty );
    propertyBinder.setModelToTargetConverter( new StringToFileConverter() );
    propertyBinder.setTargetToModelConverter( new FileToStringConverter() );

    binding.bindValue( propertyBinder );

    /* Now create ui against file data */

    final IFileChooserDelegate dirDelegate = new FileChooserDelegateDirectory();
    final FileBinding fb = new FileBinding( binding, modelDir, dirDelegate );

    final Group group = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( group );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    final String documentBaseLabel = Messages.getString("UpdatePage004two005.1"); //$NON-NLS-1$
    group.setText( documentBaseLabel );

    final Text field = fb.createFileField( group );
    field.setMessage( Messages.getString("UpdatePage004two005.2") ); //$NON-NLS-1$
    field.setToolTipText( Messages.getString("UpdatePage004two005.3") ); //$NON-NLS-1$
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button searchButton = fb.createFileSearchButton( group, field );
    searchButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

    final TestDemBaseAction testAction = new TestDemBaseAction( modelDir );
    final ImageHyperlink hyperlink = ActionHyperlink.createHyperlink( null, group, SWT.PUSH, testAction );
    hyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
  }
}