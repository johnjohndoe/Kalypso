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

import org.eclipse.jface.action.Action;
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
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Thomas Jung
 */
class ChannelEditComposite extends Composite
{
  private static final int SPINNER_WIDTH = 50;

  private final ChannelEditData m_data;

  private final FormToolkit m_toolkit;

  private DatabindingForm m_binding;

  public ChannelEditComposite( final Composite parent, final FormToolkit toolkit, final int style, final ChannelEditData data, final IWidget infoWidget )
  {
    super( parent, style );

    m_toolkit = toolkit;
    m_toolkit.adapt( this );

    m_data = data;

    setLayout( new FillLayout() );

    createContents( infoWidget );
  }

  /**
   * initialisation
   */
  private void createContents( final IWidget infoWidget )
  {
    final ScrolledForm form = m_toolkit.createScrolledForm( this );
    final Composite body = form.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    // REMARK: performance, message manager is main performance bottle neck...
    form.getMessageManager().setAutoUpdate( false );

    m_binding = new DatabindingForm( form, m_toolkit );

    /* Create Profile control */
    final Control profileSection = createProfileSelectionSection( body );
    profileSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create Bank control */
    final Control bankSection = createBankSelectionSection( body );
    bankSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create segment switch control */
    final Control segmentSwitchSection = createBanklineOptionsSection( body );
    segmentSwitchSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create profile control */
    final Control profilSection = createProfilSection( body );
    profilSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* conversion to model composite */
    final Control applySection = createApplySection( body, infoWidget );
    applySection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    form.getMessageManager().setAutoUpdate( true );
  }

  private Control createApplySection( final Composite body, final IWidget infoWidget )
  {
    final Composite compConversion = m_toolkit.createComposite( body, SWT.NONE );
    compConversion.setLayout( new GridLayout( 3, false ) );

    /* apply button and info widget */
    final ApplyAction applyToAction = new ApplyAction( m_data );
    final Button applyToButton = ActionButton.createButton( m_toolkit, compConversion, applyToAction );
    applyToButton.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false, 1, 1 ) );

    /* Experimantel: show mesh in OpenGL window. */
    final Action show3DAction = new Show3DMeshAction( m_data );
    final Button show3DButton = ActionButton.createButton( m_toolkit, compConversion, show3DAction );

    final boolean openGlTest = false;
    show3DButton.setVisible( openGlTest );

    /* info widget */
    final SetWidgetAction infoWidgetAction = new SetWidgetAction( m_data, infoWidget );
    infoWidgetAction.setToolTipText( Messages.getString( "ChannelEditComposite.0" ) ); //$NON-NLS-1$
    infoWidgetAction.setImageDescriptor( KalypsoModel1D2DPlugin.getImageProvider().getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.CHANNEL_EDIT_INFO ) );

    // TODO: enable only if we have any meshes
    ChannelEditUtil.createWidgetSelectionButton( m_toolkit, compConversion, m_data, m_binding, infoWidgetAction, null );

    return compConversion;
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

    final Composite profileComposite = new ProfileSection( m_toolkit, profilSection, m_data, m_binding );
    profilSection.setClient( profileComposite );

    return profilSection;
  }

  /**
   * in the segment section the crosssections will be displayed. the data filling is done in the function
   * "updateSegmentSection" (see below)
   */
  private Control createBanklineOptionsSection( final Composite parent )
  {
    final Section segmentSection = m_toolkit.createSection( parent, Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );

    segmentSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.0" ) ); //$NON-NLS-1$
    segmentSection.setDescription( Messages.getString( "ChannelEditComposite.1" ) ); //$NON-NLS-1$

    final BankOptionsSection optionsComposite = new BankOptionsSection( m_toolkit, segmentSection, m_data, m_binding, SPINNER_WIDTH );
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

    final Composite bankSelectionComposite = new BankSelectionComposite( m_toolkit, bankSection, m_data, m_binding, SPINNER_WIDTH );

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

    final ProfileSelectionSection selectionComposite = new ProfileSelectionSection( m_toolkit, selectionSection, m_data, m_binding, SPINNER_WIDTH );

    selectionSection.setClient( selectionComposite );

    return selectionSection;
  }
}