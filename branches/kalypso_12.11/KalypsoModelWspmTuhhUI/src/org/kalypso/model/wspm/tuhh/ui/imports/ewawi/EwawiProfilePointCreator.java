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

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePoint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.shape.geometry.SHPPoint;

/**
 * @author Holger Albert
 */
public class EwawiProfilePointCreator
{
  private final EwawiSta m_staIndex;

  private final EwawiProfilePart m_basePart;

  private final IProfileFeature m_profileFeature;

  public EwawiProfilePointCreator( final EwawiSta staIndex, final EwawiProfilePart basePart, final IProfileFeature profileFeature )
  {
    m_staIndex = staIndex;
    m_basePart = basePart;
    m_profileFeature = profileFeature;
  }

  public void createProfilePoints( ) throws EwawiException
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );
    final IComponent idComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_ID );
    final IComponent commentComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT );
    final IComponent rechtswertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hochwertComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent codeComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_CODE );

    final IProfile profil = m_profileFeature.getProfile();
    profil.addPointProperty( idComponent );
    profil.addPointProperty( commentComponent );
    profil.addPointProperty( rechtswertComponent );
    profil.addPointProperty( hochwertComponent );
    profil.addPointProperty( breiteComponent );
    profil.addPointProperty( hoeheComponent );
    profil.addPointProperty( codeComponent );

    final EwawiProLine[] proLines = m_basePart.getProLines();
    for( final EwawiProLine proLine : proLines )
    {
      final EwawiProfilePoint profilePoint = EwawiUtilities.createProfilePoint( m_staIndex, proLine );
      final SHPPoint shape = profilePoint.getShape();

      final String id = String.format( "%d", proLine.getPunktNummer() ); //$NON-NLS-1$
      final String comment = EwawiUtilities.getRecordDescription( proLine );
      final double rechtswert = shape.getX();
      final double hochwert = shape.getY();
      final double breite = profilePoint.getBreite().doubleValue();
      final double hoehe = profilePoint.getHoehe().doubleValue();
      final String code = EwawiUtilities.getRecordCode( proLine );

      final IProfileRecord record = profil.createProfilPoint();
      record.setValue( record.indexOfProperty( idComponent ), id );
      record.setValue( record.indexOfProperty( commentComponent ), comment );
      record.setValue( record.indexOfProperty( rechtswertComponent ), rechtswert );
      record.setValue( record.indexOfProperty( hochwertComponent ), hochwert );
      record.setValue( record.indexOfProperty( breiteComponent ), breite );
      record.setValue( record.indexOfProperty( hoeheComponent ), hoehe );
      record.setValue( record.indexOfProperty( codeComponent ), code );

      profil.addPoint( record );
    }
  }
}