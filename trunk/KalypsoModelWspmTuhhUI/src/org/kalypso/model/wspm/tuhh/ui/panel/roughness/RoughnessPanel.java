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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness;

import java.util.ArrayList;
import java.util.List;

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
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.pager.ElementsComposite;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.contribs.eclipse.ui.pager.IElementPageListener;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class RoughnessPanel extends AbstractProfilView implements IElementPageListener
{
  static final Image IMG_ADD_ROUGHNESS = new Image( null, RoughnessPanel.class.getResourceAsStream( "images/roughnessPanelAdd.gif" ) );

  private static String LAST_SELECTED_PAGE;

  public RoughnessPanel( final IProfil profile )
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
    final IComponent[] roughnesses = RoughnessPanelHelper.fromProfile( getProfil() );
    final IElementPage[] pages = getPages( roughnesses );
    final ElementsComposite composite = new ElementsComposite( body, toolkit, pages, getSelectedPage( pages ) );
    composite.addPageListener( this );

    composite.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    composite.setShowAlwaysComboViewer( true );
    composite.update();

    toolkit.createLabel( body, " " ); // spacer

    /** handle missing roughnesses */
    createMissingRoughnessesControl( body, toolkit );

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

  private void createMissingRoughnessesControl( final Composite parent, final FormToolkit toolkit )
  {
    final String[] missing = RoughnessPanelHelper.findMissing( getProfil() );
    if( ArrayUtils.isEmpty( missing ) )
      return;

    final Group group = new Group( parent, SWT.NULL );
    group.setLayout( Layouts.createGridLayout( 2 ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( "Profiloperationen" );

    final ComboViewer viewer = new ComboViewer( group, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( element ) )
          return "Add roughness of type ks ";
        else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( element ) )
          return "Add roughness of type kst";
        else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( element ) )
          return "Add roughness classes";
        else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( element ) )
          return "Add roughness factor";

        return super.getText( element );
      }
    } );

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
        if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( selected ) )
          RoughnessPanelHelper.addRoughness( getProfil(), IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
        else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( selected ) )
          RoughnessPanelHelper.addRoughness( getProfil(), IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
        else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( selected ) )
          RoughnessPanelHelper.addRoughness( getProfil(), IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );
        else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( selected ) )
          RoughnessPanelHelper.addRoughness( getProfil(), IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR );
      }
    } );

    group.layout();
    parent.layout();
  }

  private IElementPage[] getPages( final IComponent[] roughnesses )
  {
    final List<IElementPage> pages = new ArrayList<IElementPage>();

    for( final IComponent roughness : roughnesses )
    {
      if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( roughness.getId() ) )
        pages.add( new RoughnessKsComposite( getProfil(), roughness ) );
      else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( roughness.getId() ) )
        pages.add( new RoughnessKstComposite( getProfil(), roughness ) );
      else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( roughness.getId() ) )
        pages.add( new RoughnessClassComposite( getProfil(), roughness ) );
      else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( roughness.getId() ) )
        pages.add( new RoughnessFactorComposite( getProfil(), roughness ) );
      else
        pages.add( new MissingRoughnessTypePage( roughness ) );
    }

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
