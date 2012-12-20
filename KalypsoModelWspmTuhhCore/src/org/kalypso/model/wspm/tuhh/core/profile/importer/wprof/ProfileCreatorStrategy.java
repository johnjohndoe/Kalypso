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
 * @author Gernot Belger
 */
public class ProfileCreatorStrategy implements IProfileCreatorStrategy
{
  @Override
  public String toString( )
  {
    return Messages.getString( "ProfileCreatorStrategy_0" ); //$NON-NLS-1$
  }

  @Override
  public IProfileCreator createProfileCreator( final ProfileData data )
  {
    final ProfilePolygones polygones = data.getProfilePolygones();

    final IWProfPoint anyPoint = polygones.getAnyPoint();
    final WProfProfileType profileType = anyPoint.getProfileType();
    final String profileLabel = profileType == null ? Messages.getString( "ProfileCreatorStrategy_1" ) : profileType.getLabel(); //$NON-NLS-1$

    // TODO: either like this... (using Objekttyp Profil and Objekttyp Punkt)
    switch( profileType )
    {
      case Brückenprofil:
      case Verdohlungsprofil:
      {
        final String soilPolygonName = findSoilPolygonNameForBridgeOrCulvert( polygones );
        return new GoodBridgeProfileCreator( data, soilPolygonName, null, null, "V99", profileLabel ); //$NON-NLS-1$
      }
    }

    // TODO: or like this... (guessing bride type by Objekttyp Verbundprofil)
    final String[] allIDs = polygones.getAllIDs();
    for( final String id : allIDs )
    {
      if( id.startsWith( "K" ) ) //$NON-NLS-1$
      {
        if( polygones.hasPoints( "V01" ) ) //$NON-NLS-1$
          return new KreisProfileCreator( profileLabel + Messages.getString( "ProfileCreatorStrategy_2" ), data, "V01", "V03", "V99" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        if( polygones.hasPoints( "D01" ) ) //$NON-NLS-1$
          return new KreisProfileCreator( profileLabel + Messages.getString( "ProfileCreatorStrategy_3" ), data, "D01", "D03", "V99" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        if( polygones.hasPoints( "D91" ) ) //$NON-NLS-1$
          return new KreisProfileCreator( profileLabel + Messages.getString( "ProfileCreatorStrategy_4" ), data, "D91", "D93", "V99" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      }
    }

    if( polygones.hasPoints( "V01", "V02", "V03" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      return new BridgeProfileCreator( data, "V01", "V02", "V03", "V99", profileLabel ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    if( polygones.hasPoints( "D01", "D02", "D03" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      return new BridgeProfileCreator( data, "D01", "D02", "D03", "V99", profileLabel + Messages.getString( "ProfileCreatorStrategy_5" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    if( polygones.hasPoints( "D01", "D02", "D05" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      return new BridgeProfileCreator( data, "D01", "D02", "D05", "V99", profileLabel + Messages.getString( "ProfileCreatorStrategy_6" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    if( polygones.hasPoints( "D91", "D92", "D93" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      return new BridgeProfileCreator( data, "D91", "D92", "D93", "V99", profileLabel + Messages.getString( "ProfileCreatorStrategy_7" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    if( polygones.hasPoints( "D91", "D92", "D95" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      return new BridgeProfileCreator( data, "D91", "D92", "D95", "V99", profileLabel + Messages.getString( "ProfileCreatorStrategy_8" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    /* Wehre */

    // FIXME: enum!
    if( profileType == WProfProfileType.Absturzprofil )
    {
      // TODO: wie die unteren Fälle erwischen?
// return new WeirProfileCreator( "Absturz", data, "2314", "2314" );
    }

    // FIXME wir sollten eigentlich den Profiltyp erst mal auswerten: z.B. 4 = Absurzprofil...

    // dieser Fall ist von den Attributen her gar kein Wehr...
    if( polygones.hasPoints( "V01", "V03" ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return new WeirProfileCreator( profileLabel, data, "V01", "V03" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$

    if( polygones.hasPoints( "21", "2314" ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return new WeirProfileCreator( profileLabel, data, "21", "2314" ); //$NON-NLS-1$//$NON-NLS-2$

    if( polygones.hasPoints( "2314" ) ) //$NON-NLS-1$
      return new WeirProfileCreator( profileLabel, data, "2314", "2314" ); //$NON-NLS-1$//$NON-NLS-2$

    // Im Zweifelsfall auch noch mal alles nur als Gelände versuchen

    if( polygones.hasPoints( "D01" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + Messages.getString( "ProfileCreatorStrategy_12" ), data, "D01" ); //$NON-NLS-1$//$NON-NLS-2$

    if( polygones.hasPoints( "D91" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + Messages.getString( "ProfileCreatorStrategy_13" ), data, "D91" ); //$NON-NLS-1$//$NON-NLS-2$

    // Rarer Fall, nur V01er (z.B. mit V08)
    if( polygones.hasPoints( "V01" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel + " (V01)", data, "V01" ); //$NON-NLS-1$//$NON-NLS-2$

    if( polygones.hasPoints( "2314" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel, data, "2314" ); //$NON-NLS-1$

    if( polygones.hasPoints( "21" ) ) //$NON-NLS-1$
      return new GelaendeProfileCreator( profileLabel, data, "21" ); //$NON-NLS-1$

    return new EmptyProfileCreator( data );
  }

  private String findSoilPolygonNameForBridgeOrCulvert( final ProfilePolygones polygones )
  {
    if( polygones.hasPoints( "V01" ) ) //$NON-NLS-1$
      return "V01"; //$NON-NLS-1$
    if( polygones.hasPoints( "D01" ) ) //$NON-NLS-1$
      return "V01"; //$NON-NLS-1$
    if( polygones.hasPoints( "D91" ) ) //$NON-NLS-1$
      return "V01"; //$NON-NLS-1$

    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public IProfileSecondaryCreator[] createSecondaryCreators( final ProfileData[] data )
  {
    final Collection<IProfileSecondaryCreator> result = new ArrayList<>();
    result.add( new KreisOWCreator( data ) );

    return result.toArray( new IProfileSecondaryCreator[result.size()] );
  }
}
