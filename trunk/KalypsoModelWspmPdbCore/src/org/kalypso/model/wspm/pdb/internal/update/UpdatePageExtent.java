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

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.observable.value.PropertiesObservaleValue;
import org.kalypso.commons.databinding.validation.StringAsDoubleValidator;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.version.IUpdateScriptPage;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptPageData;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * Page asks for the global extent of the pdb data. Needed for oracle spatial columns definitions.<br/>
 * We also ask for postgis, although it is not really used.
 * 
 * @author Gernot Belger
 */
public class UpdatePageExtent extends WizardPage implements IUpdateScriptPage
{
  private UpdateScriptPageData m_data;

  private DatabindingWizardPage m_binding;

  public UpdatePageExtent( )
  {
    super( "extentPage" ); //$NON-NLS-1$

    setTitle( Messages.getString( "UpdatePageExtent.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "UpdatePageExtent.1" ) ); //$NON-NLS-1$
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
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createExtentControls( panel );
    createActionControl( panel );
  }

  private void createExtentControls( final Composite parent )
  {
    final Group groupMin = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( groupMin );
    groupMin.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    groupMin.setText( Messages.getString( "UpdatePageExtent.2" ) ); //$NON-NLS-1$

    createPropertyControl( groupMin, "X", PdbInfo.PROPERTY_SRS_MIN_X ); //$NON-NLS-1$
    createPropertyControl( groupMin, "Y", PdbInfo.PROPERTY_SRS_MIN_Y ); //$NON-NLS-1$
    createPropertyControl( groupMin, "Z", PdbInfo.PROPERTY_SRS_MIN_Z ); //$NON-NLS-1$

    final Group groupMax = new Group( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( groupMax );
    groupMax.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    groupMax.setText( Messages.getString( "UpdatePageExtent.6" ) ); //$NON-NLS-1$

    createPropertyControl( groupMax, "X", PdbInfo.PROPERTY_SRS_MAX_X ); //$NON-NLS-1$
    createPropertyControl( groupMax, "Y", PdbInfo.PROPERTY_SRS_MAX_Y ); //$NON-NLS-1$
    createPropertyControl( groupMax, "Z", PdbInfo.PROPERTY_SRS_MAX_Z ); //$NON-NLS-1$
  }

  private void createPropertyControl( final Composite parent, final String label, final String property )
  {
    new Label( parent, SWT.NONE ).setText( label );

    final Text field = new Text( parent, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = new PropertiesObservaleValue( m_data.getVariables(), property );

    final DataBinder binder = new DataBinder( target, model );
    final String message = String.format( Messages.getString( "UpdatePageExtent.10" ), label ); //$NON-NLS-1$
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, message ) );
    binder.addTargetAfterGetValidator( new StringAsDoubleValidator( IStatus.ERROR, label ) );

    m_binding.bindValue( binder );
  }

  private void createActionControl( final Composite panel )
  {
    final Action lookupSrsAction = new LookupSRSAction( m_data );
    final ImageHyperlink hyperlink = ActionHyperlink.createHyperlink( null, panel, ERROR, lookupSrsAction );
    hyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
  }
}