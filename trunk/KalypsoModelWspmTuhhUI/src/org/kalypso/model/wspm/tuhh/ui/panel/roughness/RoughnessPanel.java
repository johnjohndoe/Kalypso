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
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.contribs.eclipse.ui.pager.ElementsComposite;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.model.wspm.core.IWspmProperties;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class RoughnessPanel extends AbstractProfilView
{

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
    body.setLayout( Layouts.createGridLayout() );

    /** handle existing roughnesses */
    final IComponent[] roughnesses = RoughnessPanelHelper.fromProfile( getProfil() );
    final IElementPage[] pages = getPages( roughnesses );
    final ElementsComposite composite = new ElementsComposite( body, toolkit, pages );
    composite.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    composite.setShowAlwaysComboViewer( true );
    composite.update();

    toolkit.createLabel( body, "" ); // spacer

    /** handle missing roughnesses */
    createMissingRoughnessesControl( body, toolkit );

    return body;
  }

  private void createMissingRoughnessesControl( final Composite parent, final FormToolkit toolkit )
  {
    final String[] missing = RoughnessPanelHelper.findMissing( getProfil() );
    if( ArrayUtils.isEmpty( missing ) )
      return;

    final Composite body = toolkit.createComposite( parent );
    body.setLayout( Layouts.createGridLayout( 2 ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    toolkit.createLabel( body, "Komponente" );

    final ComboViewer viewer = new ComboViewer( body, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        if( IWspmProperties.POINT_PROPERTY_RAUHEIT_KS.equals( element ) )
          return RoughnessKsComposite.RAUHEIT_KS_LABEL;
        else if( IWspmProperties.POINT_PROPERTY_RAUHEIT_KST.equals( element ) )
          return RoughnessKstComposite.RAUHEIT_KST_LABEL;
        else if( IWspmProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( element ) )
          return RoughnessClassComposite.RAUHEIT_KLASSE_LABEL;

        return super.getText( element );
      }
    } );

  }

  private IElementPage[] getPages( final IComponent[] roughnesses )
  {
    final List<IElementPage> pages = new ArrayList<IElementPage>();

    for( final IComponent roughness : roughnesses )
    {
      if( IWspmProperties.POINT_PROPERTY_RAUHEIT_KS.equals( roughness.getId() ) )
        pages.add( new RoughnessKsComposite( getProfil(), roughness ) );
      else if( IWspmProperties.POINT_PROPERTY_RAUHEIT_KS.equals( roughness.getId() ) )
        pages.add( new RoughnessKstComposite( getProfil(), roughness ) );
      else if( IWspmProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( roughness.getId() ) )
        pages.add( new RoughnessClassComposite( getProfil(), roughness ) );
      else
        pages.add( new MissingRoughnessTypePage( roughness ) );
    }

    return pages.toArray( new IElementPage[] {} );
  }
}
