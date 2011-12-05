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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.model.wspm.tuhh.core.wprof.WProfProfileType;

/**
 * Für Rolf: Erzeugt nur Gelände-Profile.... und kiene Profilkommentare...
 * 
 * @author Gernot Belger
 */
public class SoilOnlyProfileCreatorStrategy implements IProfileCreatorStrategy
{
  private final boolean m_createComment;

  public SoilOnlyProfileCreatorStrategy( final boolean createComment )
  {
    m_createComment = createComment;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final String commentString = m_createComment ? Messages.getString("SoilOnlyProfileCreatorStrategy_0") : Messages.getString("SoilOnlyProfileCreatorStrategy_1"); //$NON-NLS-1$ //$NON-NLS-2$
    return String.format( Messages.getString("SoilOnlyProfileCreatorStrategy_2"), commentString ); //$NON-NLS-1$
  }

  @Override
  public IProfileCreator createProfileCreator( final ProfileData data )
  {
    final IProfileCreator creator = doCreateProfileCreator( data );
    if( creator instanceof AbstractProfileCreator )
      ((AbstractProfileCreator) creator).setDoCreateProfileComment( m_createComment );

    return creator;
  }

  private IProfileCreator doCreateProfileCreator( final ProfileData data )
  {
    final ProfilePolygones polygones = data.getProfilePolygones();

    final IWProfPoint anyPoint = polygones.getAnyPoint();
    final WProfProfileType profileType = anyPoint.getProfileType();
    final String profileLabel = profileType == null ? Messages.getString("SoilOnlyProfileCreatorStrategy_3") : profileType.getLabel(); //$NON-NLS-1$

    if( polygones.hasPoints( "D01" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + Messages.getString("SoilOnlyProfileCreatorStrategy_4"), data, "D01" );  //$NON-NLS-1$//$NON-NLS-2$

    if( polygones.hasPoints( "D91" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + Messages.getString("SoilOnlyProfileCreatorStrategy_5"), data, "D91" );  //$NON-NLS-1$//$NON-NLS-2$

    // Rarer Fall, nur V01er (z.B. mit V08)
    if( polygones.hasPoints( "V01" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + " (V01)", data, "V01" );  //$NON-NLS-1$//$NON-NLS-2$

    if( polygones.hasPoints( "21" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel, data, "21" ); //$NON-NLS-1$

    if( polygones.hasPoints( "2314" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel, data, "2314" ); //$NON-NLS-2$ //$NON-NLS-1$ //$NON-NLS-1$

    return new EmptyProfileCreator( data );
  }

  @Override
  public IProfileSecondaryCreator[] createSecondaryCreators( final ProfileData[] data )
  {
    final Collection<IProfileSecondaryCreator> result = new ArrayList<IProfileSecondaryCreator>();
    result.add( new KreisOWCreator( data ) );

    return result.toArray( new IProfileSecondaryCreator[result.size()] );
  }
}
