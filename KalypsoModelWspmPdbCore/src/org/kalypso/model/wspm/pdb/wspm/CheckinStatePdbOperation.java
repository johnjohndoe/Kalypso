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
import java.util.Date;
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
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckinPartOperation;
import org.kalypso.model.wspm.pdb.internal.wspm.IPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.OKPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.PPPartBuilder;
import org.kalypso.model.wspm.pdb.internal.wspm.UKPartBuilder;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author Gernot Belger
 */
public class CheckinStatePdbOperation implements ICheckinStatePdbOperation
{
  public static final String STR_FAILED_TO_CONVERT_GEOMETRY = Messages.getString( "CheckinStatePdbOperation.0" ); //$NON-NLS-1$

  private final IStatusCollector m_log = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final PDBNameGenerator m_sectionNames = new PDBNameGenerator();

  private final boolean m_checkSectionNames;

  private final CheckinStateOperationData m_data;

  private IProgressMonitor m_monitor;

  /**
   * @param dbSrs
   *          The coordinate system of the database
   */
  public CheckinStatePdbOperation( final CheckinStateOperationData data, final boolean checkSectionNames )
  {
    m_data = data;

    m_checkSectionNames = checkSectionNames;
  }

  @Override
  public void setMonitor( final IProgressMonitor monitor )
  {
    m_monitor = monitor;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "CheckinStateOperation.1" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    final IProfileFeature[] profiles = m_data.getProfiles();

    m_monitor.beginTask( Messages.getString( "CheckinStatePdbOperation.2" ), 10 + profiles.length ); //$NON-NLS-1$

    m_monitor.subTask( Messages.getString( "CheckinStatePdbOperation.3" ) ); //$NON-NLS-1$

    final State state = saveOrUpdateState( session );
    m_monitor.worked( 10 );

    final CheckinDocumentWorker worker = new CheckinDocumentWorker( m_data.getDocumentBase() );
    worker.createDocuments( session, state, m_data.getReach() );

    final WaterBody waterBody = m_data.getWaterBody();

    for( final IProfileFeature feature : profiles )
    {
      final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      m_monitor.subTask( String.format( Messages.getString( "CheckinStatePdbOperation.4" ), label ) ); //$NON-NLS-1$
      uploadProfile( session, waterBody, state, feature );
      m_monitor.worked( 1 );
    }

    final IStatus classStatus = m_data.checkClasses();
    if( !classStatus.isOK() )
      m_log.add( classStatus );

    m_monitor.subTask( Messages.getString( "CheckinStatePdbOperation.5" ) ); //$NON-NLS-1$
  }

  private State saveOrUpdateState( final Session session )
  {
    final State state = m_data.getState();
    state.setEditingUser( m_data.getUsername() );
    state.setEditingDate( new Date() );

    /* if state already contains cross sections or documents: delete everything */
    // do not delete existing events, they do not get uploaded via this mechanism
    final Set<CrossSection> crossSections = state.getCrossSections();
    for( final CrossSection crossSection : crossSections )
      session.delete( crossSection );
    crossSections.clear();

    final Set<Document> documents = state.getDocuments();
    for( final Document document : documents )
      session.delete( document );
    documents.clear();

    if( session == null )
      return state;

    /* add state to db if it is new */
    if( !session.contains( state ) )
      session.save( state );
    else
      session.persist( state );

    // FIXME: necessary to prevent problems with same cross sectionsre-checked in; else db whines about duplicate name
    // check if this breaks the transaction
    session.flush();

    // REMARK: nothing to do if already attached to session, in this case all changes are persited when transaction is closed.

    return state;
  }

  @Override
  public IStatus getStatus( )
  {
    return m_log.asMultiStatusOrOK( Messages.getString( "CheckinStatePdbOperation.7" ) ); //$NON-NLS-1$
  }

  private void uploadProfile( final Session session, final WaterBody waterBody, final State state, final IProfileFeature feature ) throws PdbConnectException
  {
    final IProfile profil = feature.getProfile();

    final CrossSection section = new CrossSection();

    /* db relevant data */
    section.setWaterBody( waterBody );
    section.setState( state );
    section.setCreationDate( state.getCreationDate() );
    section.setEditingDate( state.getEditingDate() );
    section.setEditingUser( state.getEditingUser() );
    section.setMeasurementDate( state.getMeasurementDate() );

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

    saveSection( session, state, section );

    final CheckinDocumentWorker worker = new CheckinDocumentWorker( m_data.getDocumentBase() );
    worker.createDocuments( session, section, feature );
  }

  private void saveSection( final Session session, final State state, final CrossSection section )
  {
    if( session == null )
    {
      /* HINT: We use the state as object to store only the cross sections, if no session is available. */
      state.getCrossSections().add( section );
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
      final GM_Object transformedCurve = m_data.getTransformer().transform( curve );
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
      // REMARK: null happens, if we export to gaf format
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
    final CheckinPartOperation partOperation = new CheckinPartOperation( m_data, profil, profilSRS, partBuilder );

    final IStatus result = partOperation.execute();
    if( !result.isOK() )
      m_log.add( result );

    final CrossSectionPart part = partOperation.getPart();

    final CrossSectionPartType type = m_data.findPartType( partBuilder.getKind() );
    part.setCrossSectionPartType( type );

    return part;
  }

  private CrossSectionPart[] createAdditionalParts( )
  {
    // TODO
    return new CrossSectionPart[0];
  }
}