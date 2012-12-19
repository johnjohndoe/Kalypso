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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.IBeanValueProperty;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileTransaction;
import org.kalypso.model.wspm.core.profil.ProfileObjectFactory;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.Profiles;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartParameter;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.utils.ConsecutiveNumComparator;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.GafPartsMapping;
import org.kalypso.model.wspm.pdb.gaf.GafPointCode;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class CrossSectionConverter implements IProfileTransaction
{
  private final CrossSection m_section;

  private final IProfile m_profile;

  private final IProfilePointPropertyProvider m_provider;

  public CrossSectionConverter( final CrossSection section, final IProfile profile )
  {
    m_section = section;
    m_profile = profile;

    m_provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
  }

  @Override
  public IStatus execute( final IProfile profile )
  {
    m_profile.setName( m_section.getName() );
    m_profile.setDescription( m_section.getDescription() );

    /* [m] -> [km] */
    final BigDecimal station = m_section.getStation();
    m_profile.setStation( station.movePointLeft( 3 ).doubleValue() );

    convertP();
    convertParts();

    return Status.OK_STATUS;
  }

  private void convertP( )
  {
    final CrossSectionPart part = m_section.findPartByCategory( GafKind.P.toString() );
    if( part == null )
      return;

    /* Add points in their natural order into the profile. */
    final Set<Point> points = part.getPoints();
    final List<Point> sortedPoints = sortPoints( points );
    for( final Point point : sortedPoints )
    {
      final IProfileRecord record = m_profile.createProfilPoint();
      m_profile.addPoint( record );

      setValue( record, IWspmPointProperties.POINT_PROPERTY_BREITE, asDouble( point.getWidth() ) );
      setValue( record, IWspmPointProperties.POINT_PROPERTY_HOEHE, asDouble( point.getHeight() ) );

      convertStandardProperties( point, record );

      convertHyk( point, record );
    }
  }

  private void convertHyk( final Point point, final IProfileRecord record )
  {
    // REMARK: We do not add hyk as separate component, it is redundant in any way.
    // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getHyk() );

    // REMARK: We only use hyk (not code) to create markers.
    final String hyks = point.getHyk();
    if( StringUtils.isBlank( hyks ) )
      return;

    /* Several hyk codes possible. */
    final String[] hykCodes = StringUtils.split( hyks, IGafConstants.HYK_CODE_SEPARATOR );
    for( final String hyk : hykCodes )
    {
      final String markerType = toMarkerType( hyk );
      if( markerType != null )
        createMarker( record, markerType );
    }
  }

  private String toMarkerType( final String hyk )
  {
    if( StringUtils.isBlank( hyk ) )
      return null;

    final GafPointCode gafPointCode = GafPointCode.valueOf( hyk );

    switch( gafPointCode )
    {
      case PA:
      case PE:
        return IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE;

      case LBOK:
      case RBOK:
        return IWspmTuhhConstants.MARKER_TYP_BORDVOLL;

      case LU:
      case RU:
        return IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE;

      default:
        return null;
    }
  }

  protected void createMarker( final IProfileRecord point, final String markerType )
  {
    final IProfilePointMarker marker = m_profile.createPointMarker( markerType, point );
    final Object defaultValue = m_provider.getDefaultValue( markerType );
    marker.setValue( defaultValue );
  }

  private Double asDouble( final BigDecimal decimal )
  {
    if( decimal == null )
      return null;

    return decimal.doubleValue();
  }

  private void setValue( final IRecord record, final String componentID, final Object value )
  {
    if( value == null )
      return;

    final int component = ensureComponent( record, componentID );
    record.setValue( component, value );
  }

  private int ensureComponent( final IRecord record, final String componentID )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentID );
    if( index != -1 )
      return index;

    owner.addComponent( m_provider.getPointProperty( componentID ) );
    return owner.indexOfComponent( componentID );
  }

  private List<Point> sortPoints( final Set<Point> points )
  {
    final ArrayList<Point> sortedPoints = new ArrayList<>( points );
    Collections.sort( sortedPoints, new ConsecutiveNumComparator() );
    return sortedPoints;
  }

  private void convertStandardProperties( final Point point, final IRecord record )
  {
    setValue( record, IWspmPointProperties.POINT_PROPERTY_ID, point.getName() );
    setValue( record, IWspmPointProperties.POINT_PROPERTY_CODE, point.getCode() );
    setValue( record, IWspmPointProperties.POINT_PROPERTY_COMMENT, point.getDescription() );

    final com.vividsolutions.jts.geom.Point location = point.getLocation();
    if( location != null )
    {
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RECHTSWERT, location.getX() );
      setValue( record, IWspmPointProperties.POINT_PROPERTY_HOCHWERT, location.getY() );
    }

    /* REMARK: The checkout operation makes sure that all necessary classes are present. */
    final Roughness roughness = point.getRoughness();
    if( roughness != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS, roughness.getId().getName() );

    final BigDecimal roughnessKst = point.getRoughnessKstValue();
    if( roughnessKst != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, roughnessKst.doubleValue() );

    final BigDecimal roughnessK = point.getRoughnessKValue();
    if( roughnessK != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, roughnessK.doubleValue() );

    /* REMARK: The checkout operation makes sure that all necessary classes are present. */
    final Vegetation vegetation = point.getVegetation();
    if( vegetation != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS, vegetation.getId().getName() );

    final BigDecimal vegetationAx = point.getVegetationAx();
    if( vegetationAx != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX, vegetationAx.doubleValue() );

    final BigDecimal vegetationAy = point.getVegetationAy();
    if( vegetationAy != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY, vegetationAy.doubleValue() );

    final BigDecimal vegetationDp = point.getVegetationDp();
    if( vegetationDp != null )
      setValue( record, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP, vegetationDp.doubleValue() );
  }

  private void convertParts( )
  {
    /* Memory for the created profile objects. */
    final List<IProfileObject> profileObjects = new ArrayList<>();

    /* Get all cross section parts. */
    final Set<CrossSectionPart> parts = m_section.getCrossSectionParts();
    for( final CrossSectionPart part : parts )
    {
      /* Ignore p horizont, because this one should be already imported. */
      final String partCategory = part.getCrossSectionPartType().getCategory();
      if( partCategory.equals( GafKind.P.toString() ) )
        continue;

      /* Convert the cross section part to a profile object. */
      final IProfileObject profileObject = convertPart( part );
      if( profileObject != null )
        profileObjects.add( profileObject );
    }

    /* Add the profile objects to the profile. */
    /* Add all in one step, to reduce events. */
    if( profileObjects.size() > 0 )
      m_profile.addProfileObjects( profileObjects.toArray( new IProfileObject[] {} ) );

    /* Search one bridge and one ok without id and repair. */
    final IProfileObject[] pos = m_profile.getProfileObjects();
    repairBridges( pos );

    /* Update the components in the profile. */
    for( final IProfileObject po : pos )
      updateComponents( po );
  }

  private IProfileObject convertPart( final CrossSectionPart part )
  {
    /* Create the profile object. */
    final String partCategory = part.getCrossSectionPartType().getCategory();
    final IProfileObject profileObject = createProfileObject( partCategory );
    if( profileObject == null )
      return null;

    /* Update the description. */
    profileObject.setDescription( part.getDescription() );

    /* Fill records. */
    fillProfileObjectRecords( part, profileObject );

    /* Guess metadata. */
    guessMetadata( part, profileObject );

    return profileObject;
  }

  private IProfileObject createProfileObject( final String partCategory )
  {
    final String wspmPartType = new GafPartsMapping().kind2partType( partCategory );

    // REMARK: special case for OK-parts: if we have an OK but no culvert and no bridge, we assume that we have a weir. Problem is, that GAf does not distinguish between weirs and 'oberkanten'.
    if( BuildingBruecke.ID_OK.equals( wspmPartType ) )
    {
      final boolean hasCulvertOrBridge = hasCategory( GafKind.UK.toString(), GafKind.K.toString(), GafKind.EI.toString(), GafKind.MA.toString(), GafKind.AR.toString(), GafKind.HA.toString(), IGafConstants.KIND_TR );
      if( !hasCulvertOrBridge )
        return ProfileObjectFactory.createProfileObject( m_profile, BuildingWehr.ID );
    }

    return ProfileObjectFactory.createProfileObject( m_profile, wspmPartType );
  }

  private boolean hasCategory( final String... category )
  {
    for( final String element : category )
    {
      if( m_section.findPartByCategory( element ) != null )
        return true;
    }

    return false;
  }

  private void fillProfileObjectRecords( final CrossSectionPart part, final IProfileObject profileObject )
  {
    /* The profile object records. */
    final IProfileObjectRecords records = profileObject.getRecords();

    /* Add points in their natural order into the profile. */
    final Set<Point> points = part.getPoints();
    final List<Point> sortedPoints = sortPoints( points );
    for( final Point point : sortedPoints )
    {
      /* Get the values from the point. */
      // REMARK: Some values like roughness are not set here at the moment...
      // REMARK: If roughness is set, set it as roughness of the building (mean value?)...
      final String id = point.getName();
      final String comment = point.getDescription();
      final BigDecimal breite = point.getWidth();
      final BigDecimal hoehe = point.getHeight();
      final String code = point.getCode();
      final com.vividsolutions.jts.geom.Point location = point.getLocation();

      /* Add the values to the record. */
      final IProfileObjectRecord record = records.addNewRecord();
      record.setId( id );
      record.setComment( comment );
      record.setBreite( breite.doubleValue() );
      record.setHoehe( hoehe.doubleValue() );
      record.setRechtswert( location.getX() );
      record.setHochwert( location.getY() );
      record.setCode( code );
    }
  }

  private void repairBridges( final IProfileObject[] profileObjects )
  {
    /* The used ids. */
    final Set<String> usedIds = BridgeIdHelper.findUsedIds( profileObjects );

    /* Check each profile object. */
    for( final IProfileObject profileObject : profileObjects )
    {
      /* Repair bridges. */
      if( profileObject instanceof BuildingBruecke )
      {
        /* Cast. */
        final BuildingBruecke bridge = (BuildingBruecke)profileObject;

        /* If a ok profile object was found, both, bridge and ok profile object have an id. */
        final IProfileObject okProfileObject = BuildingBruecke.findOkProfileObject( bridge, profileObjects );
        if( okProfileObject != null )
          continue;

        /* We create/reuse the bridge id. */
        final String bridgeId = BridgeIdHelper.findFreeId( bridge, usedIds );
        bridge.setBrueckeId( bridgeId );

        /* Search for ok profile object without bridge id and use it. */
        final IProfileObject okPO = BridgeIdHelper.findOkProfileObjectWithoutId( profileObjects );
        if( okPO != null )
        {
          okPO.setValue( BuildingBruecke.KEY_BRUECKE_ID, bridgeId );
          continue;
        }

        /* REMARK: We do not create new ok profile objects. */
      }
    }
  }

  private void updateComponents( final IProfileObject profileObject )
  {
    if( profileObject instanceof BuildingBruecke )
    {
      final BuildingBruecke bridge = (BuildingBruecke)profileObject;
      insertRecordsAs( bridge, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

      final IProfileObject ok = bridge.findOkProfileObject( m_profile );
      if( ok != null )
        insertRecordsAs( ok, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    }

    if( profileObject instanceof BuildingWehr )
    {
      final BuildingWehr weir = (BuildingWehr)profileObject;
      insertRecordsAs( weir, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    }
  }

  private void insertRecordsAs( final IProfileObject profileObject, final String asComponent )
  {
    final IWspmClassification classification = WspmClassifications.getClassification( m_profile );
    final IVegetationClass unknownVegetationClass = classification.findUnknownVegetationClass();
    final IRoughnessClass unknownRoughnessClass = classification.findUnknownRoughnessClass();

    final String unknownVegetation = unknownVegetationClass == null ? null : unknownVegetationClass.getName();
    final String unknownRoughness = unknownRoughnessClass == null ? null : unknownRoughnessClass.getName();

    final IProfileObjectRecords records = profileObject.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final String id = record.getId();
      final String comment = record.getComment();
      final Double breite = record.getBreite();
      final Double hoehe = record.getHoehe();
      final Double rechtswert = record.getRechtswert();
      final Double hochwert = record.getHochwert();
      final String code = record.getCode();

      /* Find or insert point at 'width'. */
      final boolean insert = Objects.isNull( ProfileVisitors.findPoint( m_profile, breite.doubleValue() ) );
      final IProfileRecord pRecord = Profiles.addOrFindPoint( m_profile, breite );

      setValue( pRecord, asComponent, hoehe );

      // TODO: Check: If we have the same width, but different rw/hw, we just forget the old rw/hw here, which is bad...
      // TODO: Same holds for ID, Code, etc.
      if( insert )
      {
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_ID, id );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_COMMENT, comment );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_RECHTSWERT, rechtswert );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_HOCHWERT, hochwert );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_CODE, code );

        // HOTFIX: if a new point is inserted we need a roughness/vegetation class. Unfortunately, the class values
        // are not stored ni the corresponding profile object...
        // For now, we just set the 'unknown' classes here, assuming, that the buildings never have associated roughness values
        // TODO: either remember the real clases from the profile part; or a t least, use the class of the previous point.
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS, unknownRoughness );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS, unknownVegetation );
      }
    }
  }

  private void guessMetadata( final CrossSectionPart part, final IProfileObject profileObject )
  {
    /* Set generic metadata. */
    final Set<CrossSectionPartParameter> parameters = part.getCrossSectionPartParameters();
    for( final CrossSectionPartParameter parameter : parameters )
      profileObject.setValue( parameter.getKey(), parameter.getValue() );

    /* Set specific metadata. */
    profileObject.setValue( IGafConstants.PART_NAME, part.getName() );

    /* remember event */
    final Event event = part.getEvent();
    if( event != null )
    {
      profileObject.setValue( IWspmTuhhConstants.PROFIL_PROPERTY_EVENT_NAME, event.getName() );
      profileObject.setValue( IWspmTuhhConstants.PROFIL_PROPERTY_EVENT_TYPE, event.getType().name() );
    }

    /* Guess special cases. */
    if( profileObject instanceof BuildingKreis )
      guessSpecialCases( (ICulvertBuilding)profileObject, GafPointCode.KRUK.getKey(), GafPointCode.KRFS.getKey() );

    if( profileObject instanceof BuildingEi )
      guessSpecialCases( (ICulvertBuilding)profileObject, GafPointCode.EIUK.getKey(), GafPointCode.EIFS.getKey() );

    if( profileObject instanceof BuildingMaul )
      guessSpecialCases( (ICulvertBuilding)profileObject, GafPointCode.MAUK.getKey(), GafPointCode.MAFS.getKey() );
  }

  private void guessSpecialCases( final ICulvertBuilding profileObject, final String ukCode, final String fsCode )
  {
    /* Check, if one value is set. */
    final String[] properties = profileObject.getProperties();
    for( final String property : properties )
    {
      /* If one value is set, we do not guess. */
      final IBeanValueProperty beanValueProperty = BeanProperties.value( profileObject.getClass(), property );
      final Object value = beanValueProperty.getValue( profileObject );
      if( value != null )
        return;
    }

    /* Find the uk and fs point. */
    final IProfileObjectRecord ukRecord = findPoint( profileObject, ukCode );
    final IProfileObjectRecord fsRecord = findPoint( profileObject, fsCode );
    if( ukRecord == null || fsRecord == null )
      return;

    /* Get the geometries. */
    final Coordinate ukPoint = ukRecord.getWidthHeightLocation();
    final Coordinate fsPoint = fsRecord.getWidthHeightLocation();

    /* Calcuate the values for the profile object. */
    /* Egg/Maul buildings are interpreted with 1:1 diagonales. */
    final double bezugspunktX = fsPoint.x;
    final double bezugspunktY = fsPoint.y;
    final double breite = fsPoint.distance( ukPoint );
    final double hoehe = fsPoint.distance( ukPoint );

    /* Set the values. */
    profileObject.setBezugspunktX( new Double( bezugspunktX ) );
    profileObject.setBezugspunktY( new Double( bezugspunktY ) );
    profileObject.setBreite( new Double( breite ) );

    /* Only the egg and maul buildings do have a height. */
    if( profileObject instanceof BuildingEi || profileObject instanceof BuildingMaul )
    {
      final IBeanValueProperty beanValueProperty = BeanProperties.value( profileObject.getClass(), BuildingEi.PROPERTY_HOEHE );
      beanValueProperty.setValue( profileObject, new Double( hoehe ) );
    }
  }

  private IProfileObjectRecord findPoint( final IProfileObject profileObject, final String code )
  {
    final IProfileObjectRecords records = profileObject.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final String recordCode = record.getCode();
      if( code.equals( recordCode ) )
        return record;
    }

    return null;
  }
}