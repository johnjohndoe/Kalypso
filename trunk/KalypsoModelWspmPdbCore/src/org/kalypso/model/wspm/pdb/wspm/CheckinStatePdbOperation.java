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
package org.kalypso.model.wspm.pdb.wspm;

import java.math.BigDecimal;
import java.net.URI;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.Gaf2Db;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckinPartOperation;
import org.kalypso.model.wspm.pdb.internal.wspm.ClassChecker;
import org.kalypso.model.wspm.pdb.internal.wspm.IPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.OKPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.PPPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.UKPartBuilder;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public class CheckinStatePdbOperation implements IPdbOperation
{
  public static final String STR_FAILED_TO_CONVERT_GEOMETRY = Messages.getString( "CheckinStatePdbOperation.0" ); //$NON-NLS-1$

  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final PDBNameGenerator m_sectionNames = new PDBNameGenerator();

  private final IProgressMonitor m_monitor;

  private final String m_waterCode;

  private final State m_state;

  private final TuhhReach m_reach;

  private final IProfileFeature[] m_profiles;

  private final IGeoTransformer m_transformer;

  private final ICoefficients m_coefficients;

  private final GafCodes m_gafCodes;

  private final GeometryFactory m_geometryFactory;

  private final URI m_documentBase;

  private final boolean m_checkSectionNames;

  private final ClassChecker m_classChecker;

  private final CrossSectionPartType[] m_partTypes;

  /**
   * @param dbSrs
   *          The coordinate system of the database
   */
  public CheckinStatePdbOperation( final CrossSectionPartType[] partTypes, final GafCodes gafCodes, final ICoefficients coefficients, final String waterCode, final State state, final TuhhReach reach, final IProfileFeature[] profiles, final String dbSrs, final URI documentBase, final boolean checkSectionNames, final IProgressMonitor monitor )
  {
    m_partTypes = partTypes;
    m_gafCodes = gafCodes;
    m_coefficients = coefficients;
    m_classChecker = new ClassChecker( profiles );
    m_state = state;
    m_waterCode = waterCode;
    m_reach = reach;
    m_profiles = profiles;
    m_documentBase = documentBase;
    m_checkSectionNames = checkSectionNames;

    m_monitor = monitor;

    final int srid = JTSAdapter.toSrid( dbSrs );
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), srid );

    m_transformer = GeoTransformerFactory.getGeoTransformer( dbSrs );
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "CheckinStateOperation.1" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    m_monitor.beginTask( Messages.getString( "CheckinStatePdbOperation.2" ), 10 + m_profiles.length ); //$NON-NLS-1$

    m_monitor.subTask( Messages.getString( "CheckinStatePdbOperation.3" ) ); //$NON-NLS-1$
    // FIXME: handle existing state
    Gaf2Db.addState( session, m_state );
    m_monitor.worked( 10 );

    final CheckinDocumentWorker worker = new CheckinDocumentWorker( m_documentBase );
    worker.createDocuments( session, m_state, m_reach );

    final WaterBody waterBody = WaterBodyUtils.findWaterBody( session, m_waterCode );
    if( m_waterCode != null && waterBody == null )
      throw new PdbConnectException( "bad code" );

    for( final IProfileFeature feature : m_profiles )
    {
      final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      m_monitor.subTask( String.format( Messages.getString( "CheckinStatePdbOperation.4" ), label ) ); //$NON-NLS-1$
      uploadProfile( session, waterBody, feature );
      m_monitor.worked( 1 );
    }

    final IStatus classStatus = m_classChecker.execute();
    if( !classStatus.isOK() )
      m_stati.add( classStatus );

    m_monitor.subTask( Messages.getString( "CheckinStatePdbOperation.5" ) ); //$NON-NLS-1$
  }

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatusOrOK( Messages.getString( "CheckinStatePdbOperation.7" ) ); //$NON-NLS-1$
  }

  private void uploadProfile( final Session session, final WaterBody waterBody, final IProfileFeature feature ) throws PdbConnectException
  {
    final IProfile profil = feature.getProfil();

    final CrossSection section = new CrossSection();

    /* db relevant data */
    section.setWaterBody( waterBody );
    section.setState( m_state );
    section.setCreationDate( m_state.getCreationDate() );
    section.setEditingDate( m_state.getEditingDate() );
    section.setEditingUser( m_state.getEditingUser() );
    section.setMeasurementDate( m_state.getMeasurementDate() );

    /* Data from profile */
    final BigDecimal station = getStation( feature );
    section.setStation( station );
    final String name = CheckinStateOperation.createCrossSectionName( profil.getName(), station );

    /* Check for uniqueness of profile name */
    if( m_checkSectionNames && !m_sectionNames.addUniqueName( name ) )
    {
      final String message = String.format( Messages.getString( "CheckinStatePdbOperation.6" ), station, name ); //$NON-NLS-1$
      throw new PdbConnectException( message );
    }

    section.setName( name );
    section.setDescription( profil.getComment() );

    final String srsName = feature.getSrsName();
    createParts( section, profil, srsName );

    saveSection( session, section );

    final CheckinDocumentWorker worker = new CheckinDocumentWorker( m_documentBase );
    worker.createDocuments( session, section, feature );
  }

  private void saveSection( final Session session, final CrossSection section )
  {
    if( session == null )
    {
      /* HINT: We use the state as object to store only the cross sections, if no session is available. */
      m_state.getCrossSections().add( section );
      return;
    }

    session.save( section );

    final Set<CrossSectionPart> parts = section.getCrossSectionParts();
    for( final CrossSectionPart part : parts )
    {
      session.save( part );

      final Set<Point> points = part.getPoints();
      for( final Point point : points )
        session.save( point );
    }
  }

  private BigDecimal getStation( final IProfileFeature feature )
  {
    final BigDecimal station = feature.getBigStation();
    if( station == null )
      return null;

    return station.movePointRight( 3 );
  }

  protected LineString toLine( final GM_Curve curve ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedCurve = m_transformer.transform( curve );
      return (LineString)JTSAdapter.export( transformedCurve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }

  private void createParts( final CrossSection section, final IProfile profil, final String profilSRS ) throws PdbConnectException
  {
    final Set<CrossSectionPart> parts = new HashSet<>();

    /* Extract profile line */
    final CrossSectionPart pPart = builtPart( profil, profilSRS, new PPPartBuilder( profil ) );
    if( !isBlank( pPart ) )
    {
      parts.add( pPart );
      section.setLine( pPart.getLine() );
    }

    /* Extract building parts */
    final CrossSectionPart ukPart = builtPart( profil, profilSRS, new UKPartBuilder() );
    if( !isBlank( ukPart ) )
      parts.add( ukPart );

    final OKPartBuilder okBridgeBuilder = new OKPartBuilder( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    final CrossSectionPart okPart = builtPart( profil, profilSRS, okBridgeBuilder );
    if( !isBlank( okPart ) )
      parts.add( okPart );

    final OKPartBuilder weirBuilder = new OKPartBuilder( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final CrossSectionPart okWeirPart = builtPart( profil, profilSRS, weirBuilder );
    if( !isBlank( okWeirPart ) )
      parts.add( okWeirPart );

    /* extract extra parts */
    final CrossSectionPart[] additionalParts = createAdditionalParts();
    for( final CrossSectionPart additionalPart : additionalParts )
    {
      parts.add( additionalPart );
    }

    final PDBNameGenerator partNameGenerator = new PDBNameGenerator();
    for( final CrossSectionPart part : parts )
    {
      /* Instead of preserving the existing part name, we recreate it by the same system as when importing gaf */
      final String category = part.getCrossSectionPartType().getCategory();
      final String uniquePartName = partNameGenerator.createUniqueName( category );
      part.setName( uniquePartName );
      part.setCrossSection( section );
      section.getCrossSectionParts().add( part );
    }
  }

  private boolean isBlank( final CrossSectionPart part )
  {
    if( part == null )
      return true;

    return part.getPoints().isEmpty();
  }

  private CrossSectionPart builtPart( final IProfile profil, final String profilSRS, final IPartBuilder partBuilder ) throws PdbConnectException
  {
    final CheckinPartOperation partOperation = new CheckinPartOperation( this, profil, profilSRS, partBuilder, m_classChecker );

    final IStatus result = partOperation.execute();
    if( !result.isOK() )
      m_stati.add( result );

    final CrossSectionPart part = partOperation.getPart();

    final CrossSectionPartType type = findPartType( partBuilder.getKind() );
    part.setCrossSectionPartType( type );

    return part;
  }

  private CrossSectionPartType findPartType( final GafKind kind )
  {
    if( m_partTypes == null )
      return null;

    for( final CrossSectionPartType type : m_partTypes )
    {
      if( type.getCategory().equals( kind.toString() ) )
        return type;
    }

    throw new IllegalArgumentException( String.format( "Unknown part type: %s", kind ) );
  }

  private CrossSectionPart[] createAdditionalParts( )
  {
    // TODO
    return new CrossSectionPart[0];
  }

  public ICoefficients getCoefficients( )
  {
    return m_coefficients;
  }

  public GafCodes getGafCodes( )
  {
    return m_gafCodes;
  }

  public GeometryFactory getGeometryFactory( )
  {
    return m_geometryFactory;
  }

  public IGeoTransformer getTransformer( )
  {
    return m_transformer;
  }

}