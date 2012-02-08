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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base;

import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public enum KNAUF_FLIESSGESETZ
{
  eManningStrickler(1, Messages.getString("KNAUF_FLIESSGESETZ_0")), //$NON-NLS-1$
  ePrandtlColeBrook(2, Messages.getString("KNAUF_FLIESSGESETZ_1")), //$NON-NLS-1$
  eEinstein(3, Messages.getString("KNAUF_FLIESSGESETZ_2")), //$NON-NLS-1$
  ePcBewuchsKaiser(4, Messages.getString("KNAUF_FLIESSGESETZ_3")), //$NON-NLS-1$
  ePcBewuchsNuding(5, Messages.getString("KNAUF_FLIESSGESETZ_4")), //$NON-NLS-1$
  ePcBewuchsMertens(6, Messages.getString("KNAUF_FLIESSGESETZ_5")), //$NON-NLS-1$
  ePcBewuchsPasche(7, Messages.getString("KNAUF_FLIESSGESETZ_6")), //$NON-NLS-1$
  ePcEinstein(8, Messages.getString("KNAUF_FLIESSGESETZ_7")); //$NON-NLS-1$

  private final Integer m_type;

  private final String m_name;

  KNAUF_FLIESSGESETZ( final Integer type, final String name )
  {
    m_type = type;
    m_name = name;
  }

  public Integer toInt( )
  {
    return m_type;
  }

  @Override
  public String toString( )
  {
    return m_name;
  }

  /**
   * * 46 A1 ABSZETA bei ABSZETA = A wird ZETA als absolute Verlusthöhe in m eingesetzt <br>
   * ABSZETA = S : Manning-Strickler<br>
   * ABSZETA = P : Prandtl-Colebrook
   */
  public char toABSZeta( )
  {
    switch( this )
    {
      case eManningStrickler:
        return 'S';

      default:
        return 'P';
    }
  }

  public Double getRoughnessValue( final IProfileRecord point )
  {
    switch( this )
    {
      case eManningStrickler:
        return point.getKstValue();

      default:
        return point.getKsValue();
    }

  }
}
