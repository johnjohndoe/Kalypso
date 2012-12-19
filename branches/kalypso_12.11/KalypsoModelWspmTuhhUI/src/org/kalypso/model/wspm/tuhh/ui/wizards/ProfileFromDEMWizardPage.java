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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.DoubleNaNValidator;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.core.gml.IProfileSelection;
import org.kalypso.model.wspm.core.gml.SimpleProfileSelection;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.dialog.compare.ProfileChartComposite;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.util.themes.legend.provider.ThemeNameLabelProvider;

/**
 * @author barbarins
 */
public class ProfileFromDEMWizardPage extends WizardPage
{
  private DatabindingWizardPage m_binding;

  private final ProfileFromDEMData m_data;

  public ProfileFromDEMWizardPage( final ProfileFromDEMData data )
  {
    super( "profilefromdemwizardpage" ); //$NON-NLS-1$

    m_data = data;

    setTitle( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.4" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    m_binding = new DatabindingWizardPage( this, null );

    createThemeControl( container );
    createNameControl( container );
    createStationControls( container );
    createProfileViewer( container );
  }

  private void createThemeControl( final Composite parent )
  {
    final IKalypsoFeatureTheme[] themes = m_data.getThemes();
    /* Make things simple if we have only one theme */
    if( themes.length < 2 )
      return;

    final Label lName = new Label( parent, SWT.NONE );
    lName.setText( Messages.getString( "ProfileFromDEMWizardPage.2" ) ); //$NON-NLS-1$
    lName.setToolTipText( Messages.getString( "ProfileFromDEMWizardPage.3" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    viewer.getControl().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new ThemeNameLabelProvider() );
    viewer.setInput( themes );

    /* Binding */
    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ProfileFromDEMData.PROPERTY_THEME );

    final DataBinder binder = new DataBinder( target, model );
    m_binding.bindValue( binder );
  }

  private void createNameControl( final Composite parent )
  {
    final Label lName = new Label( parent, SWT.NONE );
    lName.setText( Messages.getString( "ProfileFromDEMWizardPage.0" ) ); //$NON-NLS-1$

    final Text tName = new Text( parent, SWT.BORDER );
    tName.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* Binding */
    final ISWTObservableValue target = SWTObservables.observeText( tName, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_data, ProfileFromDEMData.PROPERTY_NAME );

    final DataBinder binder = new DataBinder( target, model );

    final String warning = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.6" ); //$NON-NLS-1$
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.WARNING, warning ) );

    m_binding.bindValue( binder );
  }

  private void createStationControls( final Composite parent )
  {
    final Label lStation = new Label( parent, SWT.NONE );
    lStation.setText( Messages.getString( "ProfileFromDEMWizardPage.1" ) ); //$NON-NLS-1$

    final Text tStation = new Text( parent, SWT.BORDER );
    tStation.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* Binding */
    final ISWTObservableValue target = SWTObservables.observeText( tStation, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_data, ProfileFromDEMData.PROPERTY_STATION );

    final DataBinder binder = new DataBinder( target, model );

    final String error = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.7" ); //$NON-NLS-1$
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, error ) );
    binder.addTargetAfterConvertValidator( new DoubleNaNValidator( IStatus.WARNING, error ) );

    m_binding.bindValue( binder );
  }

  private void createProfileViewer( final Composite parent )
  {
    final IProfile profile = m_data.getProfile();

    final IProfilLayerProvider lp = KalypsoModelWspmUIExtensions.createProfilLayerProvider( profile.getType() );

    final IProfileSelection profileSelection = new SimpleProfileSelection( profile );
    final ProfileChartComposite profileChart = new ProfileChartComposite( parent, SWT.BORDER, lp, profileSelection );
    profileChart.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
  }
}