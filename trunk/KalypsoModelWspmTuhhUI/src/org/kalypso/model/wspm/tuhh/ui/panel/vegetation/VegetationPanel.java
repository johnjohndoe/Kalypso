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
package org.kalypso.model.wspm.tuhh.ui.panel.vegetation;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.contribs.eclipse.ui.pager.ElementsComposite;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.contribs.eclipse.ui.pager.IElementPageListener;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages.VegetationClassesPage;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages.VegetationPropertiesPage;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationPanelHelper;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;

/**
 * @author Dirk Kuch
 */
public class VegetationPanel extends AbstractProfilView implements IElementPageListener
{
  static final Image IMG_ADD_ROUGHNESS = new Image( null, VegetationPanel.class.getResourceAsStream( "images/vegetationPanelAdd.gif" ) ); //$NON-NLS-1$

  private static String LAST_SELECTED_PAGE;

  public VegetationPanel( final IProfile profile )
  {
    super( profile );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
// TODO
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    final Composite body = toolkit.createComposite( parent, SWT.FLAT );
    body.setLayout( new GridLayout() );

    /** handle existing roughnesses */
    final IElementPage[] pages = getPages();
    final ElementsComposite composite = new ElementsComposite( body, toolkit, pages, getSelectedPage( pages ) );
    composite.addPageListener( this );

    composite.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    composite.setShowAlwaysComboViewer( true );
    composite.update();

    toolkit.createLabel( body, " " ); // spacer //$NON-NLS-1$

    /** handle missing roughnesses */
    createMissingVegetationPropertiesControl( body, toolkit );

    body.layout();

    return body;
  }

  private IElementPage getSelectedPage( final IElementPage[] pages )
  {
    if( LAST_SELECTED_PAGE == null )
      return pages[0];

    for( final IElementPage page : pages )
    {
      if( page.getIdentifier().equals( LAST_SELECTED_PAGE ) )
        return page;
    }

    if( ArrayUtils.isEmpty( pages ) )
      return null;

    return pages[0];
  }

  protected enum MISSING_TYPES
  {
    eVegetationTypes(Messages.getString( "VegetationPanel.2" )), //$NON-NLS-1$
    eVegetationClass(Messages.getString( "VegetationPanel.3" )); //$NON-NLS-1$

    private final String m_label;

    MISSING_TYPES( final String label )
    {
      m_label = label;
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      return m_label;
    }

  }

  private void createMissingVegetationPropertiesControl( final Composite parent, final FormToolkit toolkit )
  {
    if( WspmClassifications.hasVegetationProperties( getProfile() ) && WspmClassifications.hasVegetationClass( getProfile() ) )
      return;

    final Group group = new Group( parent, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "VegetationPanel.4" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( group, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );

    final MISSING_TYPES[] missing = getMissingTypes();

    viewer.setInput( missing );
    viewer.setSelection( new StructuredSelection( missing[0] ) );

    final ImageHyperlink lnkAdd = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkAdd.setImage( IMG_ADD_ROUGHNESS );

    lnkAdd.addHyperlinkListener( new HyperlinkAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.HyperlinkAdapter#linkActivated(org.eclipse.ui.forms.events.HyperlinkEvent)
       */
      @Override
      public void linkActivated( final HyperlinkEvent e )
      {
        final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
        final Object selected = selection.getFirstElement();
        if( MISSING_TYPES.eVegetationTypes.equals( selected ) )
          VegetationPanelHelper.addVegetationTypes( getProfile() );
        else if( MISSING_TYPES.eVegetationClass.equals( selected ) )
          VegetationPanelHelper.addVegetationClass( getProfile() );
      }
    } );

    toolkit.adapt( group );
    group.layout();
    parent.layout();
  }

  private MISSING_TYPES[] getMissingTypes( )
  {
    final Set<MISSING_TYPES> types = new LinkedHashSet<>();

    if( !WspmClassifications.hasVegetationProperties( getProfile() ) )
      types.add( MISSING_TYPES.eVegetationTypes );

    if( !WspmClassifications.hasVegetationClass( getProfile() ) )
      types.add( MISSING_TYPES.eVegetationClass );

    return types.toArray( new MISSING_TYPES[] {} );
  }

  private IElementPage[] getPages( )
  {
    final IProfile profile = getProfile();

    final List<IElementPage> pages = new ArrayList<>();

    if( WspmClassifications.hasVegetationProperties( profile ) )
      pages.add( new VegetationPropertiesPage( profile ) );
    if( WspmClassifications.hasVegetationClass( profile ) )
      pages.add( new VegetationClassesPage( profile, profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS ) ) );

    return pages.toArray( new IElementPage[] {} );
  }

  @Override
  public void pageChanged( final String identifier )
  {
    LAST_SELECTED_PAGE = identifier;
  }
}
