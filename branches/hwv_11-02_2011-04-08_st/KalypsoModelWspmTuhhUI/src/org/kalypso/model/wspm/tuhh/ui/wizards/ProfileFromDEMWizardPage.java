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

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.dialog.compare.ProfileChartComposite;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;

/**
 * @author barbarins
 */
public class ProfileFromDEMWizardPage extends WizardPage
{
  private String m_name;

  private double m_station;

  private final IProfil m_profile;

  public ProfileFromDEMWizardPage( final IProfil profile )
  {
    super( "profilefromdemwizardpage" ); //$NON-NLS-1$

    m_profile = profile;

    m_name = ""; //$NON-NLS-1$
    m_station = Double.NaN;

    setTitle( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.4" ) ); //$NON-NLS-1$
  }

  public String getProfileName( )
  {
    return m_name;
  }

  public double getProfileStation( )
  {
    return m_station;
  }

  protected void checkPageCompleted( )
  {
    setMessage( null );
    setPageComplete( true );

    if( Double.isNaN( m_station ) )
    {
      setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.7" ), ERROR ); //$NON-NLS-1$
      setPageComplete( false );
    }

    if( StringUtils.isEmpty( m_name ) )
    {
      setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizard.CreateProfileFromDem.6" ), ERROR ); //$NON-NLS-1$
      setPageComplete( false );
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    /* name */
    final Label lName = new Label( container, SWT.NONE );
    lName.setText( Messages.getString("ProfileFromDEMWizardPage.0") ); //$NON-NLS-1$

    final Text tName = new Text( container, SWT.BORDER );
    tName.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tName.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        setName( tName.getText() );
      }
    } );

    /* station */
    final Label lStation = new Label( container, SWT.NONE );
    lStation.setText( Messages.getString("ProfileFromDEMWizardPage.1") ); //$NON-NLS-1$

    final Text tStation = new Text( container, SWT.BORDER );
    tStation.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tStation.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        setStation( tStation.getText() );
      }
    } );

    final IProfilLayerProvider lp = m_profile == null ? null : KalypsoModelWspmUIExtensions.createProfilLayerProvider( m_profile.getType() );

    final ProfileChartComposite profileChart = new ProfileChartComposite( container, SWT.BORDER, lp, m_profile );
    profileChart.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
  }

  protected void setStation( final String text )
  {
    m_station = NumberUtils.parseQuietDouble( text );
    checkPageCompleted();
  }

  protected void setName( final String text )
  {
    m_name = text;
    checkPageCompleted();
  }

}
