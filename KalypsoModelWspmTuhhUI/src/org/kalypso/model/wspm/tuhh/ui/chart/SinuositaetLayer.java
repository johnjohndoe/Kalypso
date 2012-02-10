/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectRemove;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.SinuositaetPanel;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;

import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;

/**
 * @author kimwerner
 */
public class SinuositaetLayer extends ComponentLayer
{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#removeYourself()
   */
  @Override
  public void removeYourself( )
  {
    final ISinuositaetProfileObject[] sin = getProfil().getProfileObjects( ISinuositaetProfileObject.class );
    final ProfilOperation operation = new ProfilOperation( Messages.getString("SinuositaetLayer_0") , getProfil(), true );  //$NON-NLS-1$
    operation.addChange( new ProfileObjectRemove( getProfil(), sin ) );
    new ProfilOperationJob( operation ).schedule();
  }

  public SinuositaetLayer( IProfil profil )
  {
    super( profil, "NullComponent" ); //$NON-NLS-1$

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#createLayerPanel()
   */
  @Override
  public IProfilView createLayerPanel( )
  {
    return new SinuositaetPanel( getProfil() );
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getLegendEntries()
   */
  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final LegendEntry le = new LegendEntry( this, "NullComponent" ) //$NON-NLS-1$
    {

      @Override
      public void paintSymbol( GC gc, Point size )
      {
        // TODO get nice icon

      }

    };
    return new ILegendEntry[] { le };
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    final ISinuositaetProfileObject[] sin = getProfil().getProfileObjects( ISinuositaetProfileObject.class );
    if( sin.length > 0 && sin[0] instanceof SinuositaetProfileObject )
    {
      final SinuositaetProfileObject sinuositaet = (SinuositaetProfileObject) sin[0];
      return sinuositaet.getObservation().getDescription();
    }
    return super.getTitle();
  }
}
