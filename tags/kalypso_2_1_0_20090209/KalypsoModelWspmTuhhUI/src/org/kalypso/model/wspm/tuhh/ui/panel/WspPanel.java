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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

/**
 * @author belger
 */
public class WspPanel extends AbstractProfilView
{
  private static final String VALUE_FORMAT = "%.2f";

  private Composite m_panel;

  private final IStationResult m_result;

  public WspPanel( final IProfil profile, final ProfilViewData viewdata, final IStationResult result )
  {
    super( profile, viewdata );

    m_result = result;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    m_panel = new Composite( parent, style );
    m_panel.setLayout( new GridLayout( 2, false ) );

    final String[] types = m_result.getComponentIds();
    for( final String type : types )
    {
      final Label typeLabel = new Label( m_panel, SWT.NONE );
      typeLabel.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING | GridData.FILL_HORIZONTAL ) );
      typeLabel.setText( type.toString() /* + " [" + type.getUnit() + "] :" */);

      final Text typeText = new Text( m_panel, SWT.TRAIL | SWT.SINGLE );
      typeText.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL ) );
      typeText.setEditable( false );
      typeText.setText( String.format( VALUE_FORMAT, m_result.getComponentValue( type ) ) );
    }

    return m_panel;
  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

}
