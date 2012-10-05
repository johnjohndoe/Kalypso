/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.pdb.wspm;

import java.math.BigDecimal;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartParameter;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.model.wspm.pdb.internal.wspm.CrossSectionConverter;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Holger Albert
 */
public class CheckinHorizonPartOperation
{
  private final CheckinStateOperationData m_data;

  private final IProfile m_profile;

  private final IProfileObject m_profileObject;

  private final String m_profileCoordinateSystem;

  private final IStatusCollector m_stati;

  private final CrossSectionPart m_part;

  public CheckinHorizonPartOperation( final CheckinStateOperationData data, final IProfile profile, final IProfileObject profileObject, final String profileCoordinateSystem )
  {
    m_data = data;
    m_profile = profile;
    m_profileObject = profileObject;
    m_profileCoordinateSystem = profileCoordinateSystem;

    m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );
    m_part = new CrossSectionPart();
  }

  public CrossSectionPart getPart( )
  {
    return m_part;
  }

  public IStatus execute( ) throws PdbConnectException
  {
    /* Names must be unique within each part. */
    final PDBNameGenerator nameGenerator = new PDBNameGenerator();

    /* Copy description. */
    final String description = m_profileObject.getDescription();
    m_part.setDescription( description );

    /* Copy metadata (except the part_* ones). */
    copyMetadata( m_profileObject, m_part );

    /* Some values (part_*) of the new part comes from the metadata. */
    copySpecialMetadata();

    /* Get the profile object records. */
    final IProfileObjectRecords records = m_profileObject.getRecords();
    for( int i = 0; i < records.getSize(); i++ )
    {
      /* Get the profile object record. */
      final IProfileObjectRecord record = records.getRecord( i );

      /* Get all properties from the profile object record. */
      final String id = record.getId();
      final String comment = record.getComment();
      final Double breite = record.getBreite();
      final Double hoehe = record.getHoehe();
      final Double rechtswert = record.getRechtswert();
      final Double hochwert = record.getHochwert();
      final String code = record.getCode();

      /* Keep old point name if possible, else create a new unique one. */
      final String uniquePointName = nameGenerator.createUniqueName( id );
      final Point point = new Point( null, m_part, uniquePointName, i );

      /* Create the pdb code. */
      final String pdbCode = checkCode( code );

      /* Create the location. */
      final GM_Position gmPosition = GeometryFactory.createGM_Position( rechtswert, hochwert, hoehe );
      final GM_Point gmPoint = GeometryFactory.createGM_Point( gmPosition, m_profileCoordinateSystem );
      final com.vividsolutions.jts.geom.Point location = CheckinHelper.toPoint( gmPoint, m_data.getTransformer() );

      /* Set the values to the point. */
      point.setDescription( comment );
      point.setWidth( new BigDecimal( breite.doubleValue() ) );
      point.setHeight( new BigDecimal( hoehe.doubleValue() ) );
      point.setCode( pdbCode );
      point.setLocation( location );
    }

    final double station = m_profile.getStation();
    final String type = m_profileObject.getValue( CrossSectionConverter.PART_TYPE, null );
    final String warning = String.format( Messages.getString( "CheckinPartOperation_0" ), station, type ); //$NON-NLS-1$
    return m_stati.asMultiStatusOrOK( warning );
  }

  private void copyMetadata( final IProfileObject profileObject, final CrossSectionPart part )
  {
    final IProfileMetadata metadata = profileObject.getMetadata();
    final String[] keys = metadata.getKeys();
    for( final String key : keys )
    {
      if( key.startsWith( "PART_" ) )
        continue;

      final String value = metadata.getMetadata( key );

      final CrossSectionPartParameter parameter = new CrossSectionPartParameter( null, key, value, part );

      final Set<CrossSectionPartParameter> parameters = part.getCrossSectionPartParameters();
      parameters.add( parameter );
    }
  }

  private void copySpecialMetadata( )
  {
    final String name = m_profileObject.getValue( CrossSectionConverter.PART_NAME, null );
    m_part.setName( name );

    final String type = m_profileObject.getValue( CrossSectionConverter.PART_TYPE, null );
    final CrossSectionPartType partType = m_data.findPartType( type );
    m_part.setCrossSectionPartType( partType );

    // final String event = m_profileObject.getValue( CrossSectionConverter.PART_EVENT, null );
    // TODO Set the Event...
  }

  private String checkCode( final String code )
  {
    // Eventually guess...
    return code;
  }
}