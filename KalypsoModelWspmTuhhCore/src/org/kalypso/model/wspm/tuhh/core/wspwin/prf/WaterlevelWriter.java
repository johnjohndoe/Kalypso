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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.IDataBlockNames;

/**
 * Helper class that creates WSP_HOEHE datablocks from a profile and a waterlevel.
 * 
 * @author Gernot Belger
 */
public class WaterlevelWriter
{
  private final IProfil m_profil;

  private final IWaterlevel[] m_waterlevels;

  public WaterlevelWriter( final IProfil profil, final IWaterlevel[] waterlevels )
  {
    m_profil = profil;
    m_waterlevels = waterlevels;
  }

  public IDataBlock[] createDataBlocks( )
  {
    final Collection<IDataBlock> datablocks = new ArrayList<IDataBlock>();

    for( final IWaterlevel waterlevel : m_waterlevels )
    {
      final IDataBlock db = createDataBlock( waterlevel );
      datablocks.add( db );
    }

    return datablocks.toArray( new IDataBlock[datablocks.size()] );
  }

  private IDataBlock createDataBlock( final IWaterlevel waterlevel )
  {
    final String calcName = waterlevel.getLabel();
    final String riverName = Messages.getString("WaterlevelWriter_0"); //$NON-NLS-1$
    final double discharge = waterlevel.getDischarge();
    // TODO: enforce 3 significant digits
// final BigDecimal bigDischarge = new BigDecimal( discharge, new MathContext( 3 ) );
    final String dischargeText = String.format( "%.3f m3/s", discharge ); //$NON-NLS-1$

    final String secondLine = String.format( "%-100s%s@%s", dischargeText, calcName, riverName ); //$NON-NLS-1$ //$NON-NLS-2$

    final CoordDataBlockCreator creator = new CoordDataBlockCreator( IDataBlockNames.WSP_HOEHE, secondLine );

    final double wsp = waterlevel.getWaterlevel();
    writeCoords( creator, wsp );

    return creator.createDataBlock();
  }

  private void writeCoords( final CoordDataBlockCreator creator, final double waterlevel )
  {
    final Double[] intersections = WspmProfileHelper.calculateWspIntersections( m_profil, waterlevel );
    for( final Double intersection : intersections )
      creator.add( intersection, waterlevel );
  }
}
