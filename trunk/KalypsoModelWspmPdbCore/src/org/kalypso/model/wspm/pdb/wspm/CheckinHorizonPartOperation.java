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
package org.kalypso.model.wspm.pdb.wspm;

import java.math.BigDecimal;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartParameter;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypso.model.wspm.pdb.gaf.GafPartsMapping;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.referencing.FactoryException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Holger Albert
 */
public class CheckinHorizonPartOperation
{
  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final CrossSectionPart m_part = new CrossSectionPart();

  private final IProfileObject m_profileObject;

  private final double m_station;

  private final CrossSectionPartTypes m_partTypes;

  private final Event m_event;

  private JTSTransformer m_transformer;

  private final int m_profileSRID;

  private final int m_targetSRID;

  private final ICoefficients m_coefficients;

  public CheckinHorizonPartOperation( final IProfileObject profileObject, final int profileSRID, final int targetSRID, final double station, final CrossSectionPartTypes partTypes, final Event event, final ICoefficients coefficients )
  {
    m_profileObject = profileObject;
    m_profileSRID = profileSRID;
    m_targetSRID = targetSRID;
    m_station = station;
    m_partTypes = partTypes;
    m_event = event;
    m_coefficients = coefficients;
  }

  private JTSTransformer getTransformer( ) throws FactoryException
  {
    if( m_transformer == null )
      m_transformer = new JTSTransformer( m_profileSRID, m_targetSRID );

    return m_transformer;
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

    /* set part type */
    final CrossSectionPartType partType = determinePartType();
    m_part.setCrossSectionPartType( partType );

    /* Copy metadata (except the part_* ones). */
    copyMetadata( m_profileObject, m_part );

    /* Some values (part_*) of the new part comes from the metadata. */
    copySpecialMetadata();

    /* Get the profile object records. */
    final IProfileObjectRecords records = m_profileObject.getRecords();
    for( int i = 0; i < records.size(); i++ )
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
      final com.vividsolutions.jts.geom.Point location = toDBLocation( hoehe, rechtswert, hochwert );

      /* Set the values to the point. */
      point.setDescription( comment );
      point.setWidth( new BigDecimal( breite.doubleValue() ) );
      point.setHeight( new BigDecimal( hoehe.doubleValue() ) );
      point.setCode( pdbCode );
      point.setLocation( location );

      point.setRoughness( m_coefficients.getUnknownRoughness() );
      point.setVegetation( m_coefficients.getUnknownVegetation() );

      m_part.getPoints().add( point );
    }

    final String warning = String.format( Messages.getString( "CheckinPartOperation_0" ), m_station, partType.getCategory() ); //$NON-NLS-1$
    return m_stati.asMultiStatusOrOK( warning );
  }

  private CrossSectionPartType determinePartType( )
  {
    final String id = m_profileObject.getType();
    // REMARK: the PDB works with part names of GAF, rather than of WPSM
    final String gafKindName = new GafPartsMapping().partType2kindName( id );

    if( m_partTypes != null )
      return m_partTypes.findPartType( gafKindName );

    // REMARK: this only happens when exporting to GAF; else we need a valid type from the database
    return new CrossSectionPartType( gafKindName, null, null, null );
  }

  private com.vividsolutions.jts.geom.Point toDBLocation( final Double hoehe, final Double rechtswert, final Double hochwert ) throws PdbConnectException
  {
    try
    {
      final Coordinate position = new Coordinate( rechtswert, hochwert, hoehe );
      final Coordinate transformed = getTransformer().transform( position );
      final com.vividsolutions.jts.geom.Point location = JTSAdapter.jtsFactory.createPoint( transformed );
      location.setSRID( m_targetSRID );
      return location;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to transform location", e ); //$NON-NLS-1$
    }
  }

  private void copyMetadata( final IProfileObject profileObject, final CrossSectionPart part )
  {
    final IProfileMetadata metadata = profileObject.getMetadata();
    final String[] keys = metadata.getKeys();
    for( final String key : keys )
    {
      if( key.startsWith( "PART_" ) ) //$NON-NLS-1$
        continue;

      final String value = metadata.getMetadata( key );

      final CrossSectionPartParameter parameter = new CrossSectionPartParameter( null, key, value, part );

      final Set<CrossSectionPartParameter> parameters = part.getCrossSectionPartParameters();
      parameters.add( parameter );
    }
  }

  private void copySpecialMetadata( )
  {
    final String name = m_profileObject.getValue( IGafConstants.PART_NAME, null );
    m_part.setName( name );

    m_part.setEvent( m_event );
  }

  private String checkCode( final String code )
  {
    // Eventually guess...
    return code;
  }
}