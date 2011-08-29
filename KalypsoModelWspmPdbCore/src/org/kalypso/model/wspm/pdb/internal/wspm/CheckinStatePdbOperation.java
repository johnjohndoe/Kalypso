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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.activation.MimeType;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.URIUtil;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.Gaf2Db;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.Image;
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
  static final String STR_FAILED_TO_CONVERT_GEOMETRY = Messages.getString( "CheckinStatePdbOperation.0" ); //$NON-NLS-1$

  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final Map<String, WaterBody> m_waterBodies = new HashMap<String, WaterBody>();

  private final PDBNameGenerator m_sectionNames = new PDBNameGenerator();

  private final IProgressMonitor m_monitor;

  private final State m_state;

  private final IProfileFeature[] m_profiles;

  private final IGeoTransformer m_transformer;

  private final Coefficients m_coefficients;

  private final GafCodes m_gafCodes;

  private final GeometryFactory m_geometryFactory;

  private final URI m_documentBase;


  /**
   * @param dbSrs
   *          The coordinate system of the database
   */
  public CheckinStatePdbOperation( final GafCodes gafCodes, final Coefficients coefficients, final WaterBody[] waterBodies, final State state, final IProfileFeature[] profiles, final String dbSrs, final URI documentBase, final IProgressMonitor monitor )
  {
    m_gafCodes = gafCodes;
    m_coefficients = coefficients;
    m_state = state;
    m_profiles = profiles;
    m_documentBase = documentBase;

    for( final WaterBody waterBody : waterBodies )
      m_waterBodies.put( waterBody.getName(), waterBody );

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
    Gaf2Db.addState( session, m_state );
    m_monitor.worked( 10 );

    for( final IProfileFeature feature : m_profiles )
    {
      final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      m_monitor.subTask( String.format( Messages.getString( "CheckinStatePdbOperation.4" ), label ) ); //$NON-NLS-1$
      uploadProfile( session, feature );
      m_monitor.worked( 1 );
    }

    m_monitor.subTask( Messages.getString( "CheckinStatePdbOperation.5" ) ); //$NON-NLS-1$
  }

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatusOrOK( "Warnings occured during check-in" );
  }

  private void uploadProfile( final Session session, final IProfileFeature feature ) throws PdbConnectException
  {
    final IProfil profil = feature.getProfil();

    final WaterBody waterBody = findWaterBody( feature );

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
    final String name = profil.getName();

    /* Check for uniqueness of profile name */
    if( !m_sectionNames.addUniqueName( name ) )
    {
      final String message = String.format( Messages.getString( "CheckinStatePdbOperation.6" ), station, name ); //$NON-NLS-1$
      throw new PdbConnectException( message );
    }

    section.setName( name );
    section.setDescription( profil.getComment() );

    final String srsName = feature.getSrsName();
    createParts( section, profil, srsName );

    saveSection( session, section );

    // FIXME: cannot work at the moment, as filename is unique, but we do not upload new files?
    // createDocuments( session, section, feature );
  }

  private void saveSection( final Session session, final CrossSection section )
  {
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

  private WaterBody findWaterBody( final IProfileFeature feature )
  {
    final WspmWaterBody wspmWaterBody = feature.getWater();
    final String refNr = wspmWaterBody.getRefNr();
    return m_waterBodies.get( refNr );
  }

  protected LineString toLine( final GM_Curve curve ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedCurve = m_transformer.transform( curve );
      return (LineString) JTSAdapter.export( transformedCurve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }

  private void createParts( final CrossSection section, final IProfil profil, final String profilSRS ) throws PdbConnectException
  {
    final Set<CrossSectionPart> parts = new HashSet<CrossSectionPart>();

    /* Extract profile line */
    final CrossSectionPart pPart = builtPart( profil, profilSRS, IWspmConstants.POINT_PROPERTY_HOEHE, IGafConstants.KZ_CATEGORY_PROFILE );
    if( !isBlank( pPart ) )
    {
      parts.add( pPart );
      section.setLine( pPart.getLine() );
    }

    /* Extract building parts */
    final CrossSectionPart ukPart = builtPart( profil, profilSRS, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IGafConstants.KZ_CATEGORY_UK );
    if( !isBlank( ukPart ) )
      parts.add( ukPart );

    final CrossSectionPart okPart = builtPart( profil, profilSRS, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IGafConstants.KZ_CATEGORY_OK );
    if( !isBlank( okPart ) )
      parts.add( okPart );

    final CrossSectionPart okWeirPart = builtPart( profil, profilSRS, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, IGafConstants.KZ_CATEGORY_OK );
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
      final String uniquePartName = partNameGenerator.createUniqueName( part.getCategory() );
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

  private CrossSectionPart builtPart( final IProfil profil, final String profilSRS, final String mainComponentID, final String category ) throws PdbConnectException
  {
    final CheckinPartOperation partOperation = new CheckinPartOperation( this, profil, profilSRS, mainComponentID, category );

    final IStatus result = partOperation.execute();
    if( !result.isOK() )
      m_stati.add( result );

    final CrossSectionPart part = partOperation.getPart();
    part.setCategory( category );
    return part;
  }

  private CrossSectionPart[] createAdditionalParts( )
  {
    // TODO
    return new CrossSectionPart[0];
  }

  Coefficients getCoefficients( )
  {
    return m_coefficients;
  }

  GafCodes getGafCodes( )
  {
    return m_gafCodes;
  }

  GeometryFactory getGeometryFactory( )
  {
    return m_geometryFactory;
  }

  IGeoTransformer getTransformer( )
  {
    return m_transformer;
  }

  private void createDocuments( final Session session, final CrossSection section, final IProfileFeature profile )
  {
    final IFeatureBindingCollection<Image> images = profile.getImages();
    for( final Image image : images )
    {
      final MimeType mimeType = image.getMimeType();
      final URI uri = image.getUri();

      final Document document = new Document();

      document.setCreationDate( section.getCreationDate() );
      document.setCrossSection( section );
      document.setDescription( image.getDescription() );
      document.setEditingDate( section.getEditingDate() );
      document.setEditingUser( section.getEditingUser() );
      document.setFilename( asFilename( uri ) );
      document.setLocation( asPoint( image.getLocation() ) );
      document.setMeasurementDate( section.getMeasurementDate() ); // bad, should come from image
      document.setMimetype( mimeType == null ? null : mimeType.toString() );
      document.setName( asName( uri ) );
      // document.setShotdirection( null );
      document.setState( m_state );
      // document.setViewangle( null );
      document.setWaterBody( section.getWaterBody() );

      session.save( document );
    }
  }

  private String asName( final URI uri )
  {
    // FIXME: not perfect... but probably unique enough for now
    final String unencoded = URIUtil.toUnencodedString( uri );
    final String filename = FilenameUtils.getName( unencoded );
    return m_state.getName() + "/" + filename; //$NON-NLS-1$
  }

  /**
   * Makes the absolute local uri relative to the document server
   */
  private String asFilename( final URI uri )
  {
    if( uri == null )
      return null;

    final String fullLocation = uri.toString();
    if( m_documentBase == null )
      return fullLocation;

    final URI relative = URIUtil.makeRelative( uri, m_documentBase );
    return URIUtil.toUnencodedString( relative );
  }

  private com.vividsolutions.jts.geom.Point asPoint( final GM_Object location )
  {
    if( location == null )
      return null;

    try
    {
      final GM_Point centroid = location.getCentroid();
      return (com.vividsolutions.jts.geom.Point) JTSAdapter.export( centroid );
    }
    catch( final GM_Exception e )
    {
      // will never happen
      e.printStackTrace();
      return null;
    }
  }
}