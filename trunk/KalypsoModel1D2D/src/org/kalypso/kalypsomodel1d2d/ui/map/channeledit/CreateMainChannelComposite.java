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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelComposite extends Composite
{
  private static final int SPINNER_WIDTH = 50;

  private final CreateChannelData m_data;

  private final FormToolkit m_toolkit;

  private DatabindingForm m_binding;

  public CreateMainChannelComposite( final Composite parent, final FormToolkit toolkit, final int style, final CreateChannelData data )
  {
    super( parent, style );

    m_toolkit = toolkit;
    m_toolkit.adapt( this );

    m_data = data;

    setLayout( new FillLayout() );

    createContents();
  }

  /**
   * initialisation
   */
  private void createContents( )
  {
    final ScrolledForm form = m_toolkit.createScrolledForm( this );
    final Composite body = form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    m_binding = new DatabindingForm( form, m_toolkit );

    /* Create Profile control */
    final Control profileSection = createProfileSelectionSection( body );
    profileSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create Bank control */
    final Control bankSection = createBankSelectionSection( body );
    bankSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create segment switch control */
    final Control segmentSwitchSection = createSegmentSwitchSection( body );
    segmentSwitchSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create profile control */
    final Control profilSection = createProfilSection( body );
    profilSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* conversion to model composite */
    final Composite compConversion = m_toolkit.createComposite( body, SWT.NONE );
    compConversion.setLayout( new GridLayout( 2, false ) );

    final CreateMainChannelApplyAction applyToAction = new CreateMainChannelApplyAction( m_data );
    final Button applyToButton = ActionButton.createButton( m_toolkit, compConversion, applyToAction );
    applyToButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
  }

  /**
   * in the profil section the crosssections will be displayed. the data filling is done in the function
   * "updateProfilSection" (see below)
   */
  private Control createProfilSection( final Composite parent )
  {
    final Section profilSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );

    profilSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.39" ) ); //$NON-NLS-1$
    profilSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.40" ) ); //$NON-NLS-1$

    final Composite profileComposite = new CreateChannelProfileSection( m_toolkit, profilSection, m_data, m_binding );
    profilSection.setClient( profileComposite );

    return profilSection;
  }

  /**
   * in the segment section the crosssections will be displayed. the data filling is done in the function
   * "updateSegmentSection" (see below)
   */
  private Control createSegmentSwitchSection( final Composite parent )
  {
    final Section segmentSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );

    segmentSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.0" ) ); //$NON-NLS-1$
    segmentSection.setDescription( "Bearbeiten Sie die an das aktive Profil angrenzenden Uferlinien." );

    final CreateBankOptionsSection optionsComposite = new CreateBankOptionsSection( m_toolkit, segmentSection, m_data, m_binding, SPINNER_WIDTH );
    segmentSection.setClient( optionsComposite );

    return segmentSection;
  }

  /**
   * in the bank section you can select / draw your banks (left / right) and specify the number of instersections (at
   * first global for all bank segments)
   */
  private Control createBankSelectionSection( final Composite parent )
  {
    final Section bankSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );
    bankSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.16" ) ); //$NON-NLS-1$
    bankSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.17" ) ); //$NON-NLS-1$

    final Composite bankSelectionComposite = new CreateChannelBankSelectionComposite( m_toolkit, bankSection, m_data, m_binding, SPINNER_WIDTH );

    bankSection.setClient( bankSelectionComposite );

    return bankSection;
  }

  /**
   * in the profile section you can select / draw your profiles (WSPM-profiles) and specify the number of instersections
   * (global for all profiles)
   */
  private Control createProfileSelectionSection( final Composite parent )
  {
    /* profile selection expandable section */
    final Section selectionSection = m_toolkit.createSection( parent, Section.EXPANDED | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );

    selectionSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.30" ) ); //$NON-NLS-1$
    selectionSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.31" ) ); //$NON-NLS-1$

    final CreateChannelProfileSelection selectionComposite = new CreateChannelProfileSelection( m_toolkit, selectionSection, m_data, m_binding, SPINNER_WIDTH );

    selectionSection.setClient( selectionComposite );

    return selectionSection;
  }
}