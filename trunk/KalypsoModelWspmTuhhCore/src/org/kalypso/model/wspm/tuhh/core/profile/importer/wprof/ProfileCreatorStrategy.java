/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;

/**
 * @author Gernot Belger
 */
public class ProfileCreatorStrategy
{
  private final ProfileData[] m_data;

  private final TuhhWspmProject m_project;

  public ProfileCreatorStrategy( final ProfileData[] data, final TuhhWspmProject project )
  {
    m_data = data;
    m_project = project;
  }

  public void addProfiles( ) throws CoreException
  {
    for( final ProfileData currentData : m_data )
    {
      final IProfileCreator profileCreator = createProfileCreator( currentData );
      currentData.createProfile( m_project, profileCreator );
    }

    final IProfileSecondaryCreator[] secondaryCreators = createSecondaryCreators( m_data );
    for( final IProfileSecondaryCreator secondaryCreator : secondaryCreators )
      secondaryCreator.execute( m_project, m_data );
  }

  public IProfileCreator createProfileCreator( final ProfileData data )
  {
    final ProfilePolygones polygones = data.getProfilePolygones();

    // FIXME: ROLF
    final String[] allIDs = polygones.getAllIDs();
    for( final String id : allIDs )
    {
      if( id.startsWith( "K" ) )
      {
        if( polygones.hasPoints( "V01" ) )
          return new KreisProfileCreator( "Kreis", data, "V01", "V03", "V99" );

        if( polygones.hasPoints( "D01" ) )
          return new KreisProfileCreator( "Verdohlung Einlauf - Kreis", data, "D01", "D03", "V99" );

        if( polygones.hasPoints( "D91" ) )
          return new KreisProfileCreator( "Verdohlung Auslauf - Kreis", data, "D91", "D93", "V99" );
      }
    }

    if( polygones.hasPoints( "V01", "V02", "V03" ) )
      return new BridgeProfileCreator( data, "V01", "V02", "V03", "V99", "Br�cke" );

    if( polygones.hasPoints( "D01", "D02", "D03" ) )
      return new BridgeProfileCreator( data, "D01", "D02", "D03", "V99", "Verdohlung Einlauf" );

    if( polygones.hasPoints( "D01", "D02", "D05" ) )
      return new BridgeProfileCreator( data, "D01", "D02", "D05", "V99", "Verdohlung Einlauf - Gel�nder als Oberkante" );

    if( polygones.hasPoints( "D91", "D92", "D93" ) )
      return new BridgeProfileCreator( data, "D91", "D92", "D93", "V99", "Verdohlung Auslauf" );

    if( polygones.hasPoints( "D91", "D92", "D95" ) )
      return new BridgeProfileCreator( data, "D91", "D92", "D95", "V99", "Verdohlung Auslauf - Gel�nder als Oberkante" );

    /* Wehre */
    final IWProfPoint anyPoint = polygones.getAnyPoint();
    final int profileType = anyPoint.getProfileType();
    // FIXME: enum!
    if( profileType == 4 )
    {
      // TODO: wie die unteren F�lle erwischen?
// return new WeirProfileCreator( "Absturz", data, "2314", "2314" );
    }

    // FIXME wir sollten eigentlich den Profiltyp erst mal auswerten: z.B. 4 = Absurzprofil...

    // dieser Fall ist von den Attributen her gar kein Wehr...
    if( polygones.hasPoints( "V01", "V03" ) )
      return new WeirProfileCreator( "Absturz", data, "V01", "V03" );

    if( polygones.hasPoints( "2314" ) )
      return new WeirProfileCreator( "Absturz", data, "2314", "2314" );

    // Im Zweifelsfall auch noch mal alles nur als Gel�nde versuchen

    if( polygones.hasPoints( "D01" ) )
      return new GelaendeProfileCreator( "Verdohlung Einlauf (nur Gel�nde)", data, "D01" );

    if( polygones.hasPoints( "D91" ) )
      return new GelaendeProfileCreator( "Verdohlung Einlauf (nur Gel�nde)", data, "D91" );

    // Rarer Fall, nur V01er (z.B. mit V08)
    if( polygones.hasPoints( "V01" ) )
      return new GelaendeProfileCreator( "Gel�nde (V01)", data, "V01" );

    if( polygones.hasPoints( "2314" ) )
      return new GelaendeProfileCreator( "Absturz", data, "2314" );

    if( polygones.hasPoints( "21" ) )
      return new GelaendeProfileCreator( data, "21" );

    return new EmptyProfileCreator( data );
  }

  private IProfileSecondaryCreator[] createSecondaryCreators( final ProfileData[] data )
  {
    final Collection<IProfileSecondaryCreator> result = new ArrayList<IProfileSecondaryCreator>();
    result.add( new KreisOWCreator( data ) );

    return result.toArray( new IProfileSecondaryCreator[result.size()] );
  }
}
