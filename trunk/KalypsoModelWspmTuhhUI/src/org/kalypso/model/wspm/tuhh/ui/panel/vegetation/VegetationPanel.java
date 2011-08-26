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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.pager.ElementsComposite;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.contribs.eclipse.ui.pager.IElementPageListener;
import org.kalypso.model.wspm.core.gml.classifications.helper.Vegetations;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages.VegetationPropertiesPage;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;

/**
 * @author Dirk Kuch
 */
public class VegetationPanel extends AbstractProfilView implements IElementPageListener
{
  static final Image IMG_ADD_ROUGHNESS = new Image( null, VegetationPanel.class.getResourceAsStream( "images/vegetationPanelAdd.gif" ) );

  private static String LAST_SELECTED_PAGE;

  public VegetationPanel( final IProfil profile )
  {
    super( profile );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // TODO Auto-generated method stub

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

    toolkit.createLabel( body, " " ); // spacer

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

    return pages[0];
  }

  private void createMissingVegetationPropertiesControl( final Composite parent, final FormToolkit toolkit )
  {
    if( Vegetations.hasVegetationProperties( getProfile() ) )
      return;

    final Group group = new Group( parent, SWT.NULL );
    group.setLayout( Layouts.createGridLayout( 2 ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( "Profiloperationen" );

    toolkit.createLabel( group, "TODO" );
  }

  private IElementPage[] getPages( )
  {
    final List<IElementPage> pages = new ArrayList<IElementPage>();

    final IProfil profile = getProfile();
    if( Vegetations.hasVegetationProperties( profile ) )
      pages.add( new VegetationPropertiesPage( profile ) );

    return pages.toArray( new IElementPage[] {} );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPageListener#pageChanged(java.lang.String)
   */
  @Override
  public void pageChanged( final String identifier )
  {
    LAST_SELECTED_PAGE = identifier;
  }
}
