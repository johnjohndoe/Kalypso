/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
import org.kalypso.shape.geometry.SHPPoint;

/**
 * @author Holger Albert
 */
public class EwawiProfileObjectCreator
{
  private final EwawiSta m_staIndex;

  private final EwawiProfile m_ewawiProfile;

  private final IProfileFeature m_profileFeature;

  public EwawiProfileObjectCreator( final EwawiSta staIndex, final EwawiProfile ewawiProfile, final IProfileFeature profileFeature )
  {
    m_staIndex = staIndex;
    m_ewawiProfile = ewawiProfile;
    m_profileFeature = profileFeature;
  }

  public void createProfileObjects( ) throws EwawiException
  {
    final List<IProfileObject> profileObjects = new ArrayList<>();

    final EwawiProfilePart[] parts = m_ewawiProfile.getParts();
    for( final EwawiProfilePart part : parts )
    {
      /* Ignore the base profile. */
      final Integer horizont = part.getHorizont();
      if( horizont.intValue() == EwawiHorizont._0.getKey() )
        continue;

      final GenericProfileHorizon profileObject = new GenericProfileHorizon( String.format( "EWAWI_%d", horizont.intValue() ) );
      final IProfileObjectRecords records = profileObject.getRecords();

      final EwawiProLine[] proLines = part.getProLines();
      for( final EwawiProLine proLine : proLines )
        addProLine( records, proLine );

      final EwawiHorizont ewawiHorizont = EwawiHorizont.valueOf( String.format( "_%d", horizont.intValue() ) );
      final String description = ewawiHorizont.getLabel();
      profileObject.setDescription( description );

      profileObjects.add( profileObject );
    }

    if( profileObjects.size() > 0 )
    {
      final IProfile profile = m_profileFeature.getProfile();
      profile.addProfileObjects( profileObjects.toArray( new IProfileObject[] {} ) );
    }
  }

  private void addProLine( final IProfileObjectRecords records, final EwawiProLine proLine ) throws EwawiException
  {
    final IProfileObjectRecord record = records.addNewRecord();

    final EwawiProfilePoint profilePoint = EwawiUtilities.createProfilePoint( m_staIndex, proLine );
    final SHPPoint shape = profilePoint.getShape();

    final String id = String.format( "%d", proLine.getPunktNummer() ); //$NON-NLS-1$
    final String comment = EwawiUtilities.getRecordDescription( proLine );
    final double rechtswert = shape.getX();
    final double hochwert = shape.getY();
    final double breite = profilePoint.getBreite().doubleValue();
    final double hoehe = profilePoint.getHoehe().doubleValue();
    final String code = EwawiUtilities.getRecordCode( proLine );

    record.setId( id );
    record.setComment( comment );
    record.setBreite( breite );
    record.setHoehe( hoehe );
    record.setRechtswert( rechtswert );
    record.setHochwert( hochwert );
    record.setCode( code );
  }
}