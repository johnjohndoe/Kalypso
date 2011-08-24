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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.AbstractDatabinding;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class RoughnessKsComposite extends AbstractRoughnessComposite
{

  public static final String RAUHEIT_KS_LABEL = "Rauheit: ks[-]";

  public RoughnessKsComposite( final IProfil profile, final IComponent roughness )
  {
    super( profile, roughness );
  }

  @Override
  public String getLabel( )
  {
    return RAUHEIT_KS_LABEL;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPage#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( Layouts.createGridLayout( 2 ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( "Roughnesses for Flow Zones" );
    toolkit.adapt( group );

    setBinding( new AbstractDatabinding( toolkit )
    {
    } );

    build( group, toolkit, "Left Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_LEFT_FLOODPLAIN, null );
    build( group, toolkit, "River Tube", ProfileRoguhnessesDataModel.PROPERTY_RIVER_TUBE, null );
    build( group, toolkit, "Right Flood-Plain", ProfileRoguhnessesDataModel.PROPERTY_RIGHT_FLOODPLAIN, null );
  }

}
