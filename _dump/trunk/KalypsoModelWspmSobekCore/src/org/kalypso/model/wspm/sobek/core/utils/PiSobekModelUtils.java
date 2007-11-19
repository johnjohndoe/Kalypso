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
package org.kalypso.model.wspm.sobek.core.utils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import nl.wldelft.fews.pi.BranchComplexType;
import nl.wldelft.fews.pi.CrossSectionXdataComplexType;
import nl.wldelft.fews.pi.DateTimeComplexType;
import nl.wldelft.fews.pi.EventComplexType;
import nl.wldelft.fews.pi.HeaderComplexType;
import nl.wldelft.fews.pi.NodePointComplexType;
import nl.wldelft.fews.pi.ObjectFactory;
import nl.wldelft.fews.pi.Parameter;
import nl.wldelft.fews.pi.Parameters;
import nl.wldelft.fews.pi.Structure;
import nl.wldelft.fews.pi.StructureDefinition;
import nl.wldelft.fews.pi.StructureDefinitions;
import nl.wldelft.fews.pi.TimeSerieComplexType;
import nl.wldelft.fews.pi.TimeSeriesType;
import nl.wldelft.fews.pi.TimeStepComplexType;
import nl.wldelft.fews.pi.TimeStepUnitEnumStringType;
import nl.wldelft.fews.pi.CrossSectionsComplexType.CrossSection;
import nl.wldelft.fews.pi.LocationsComplexType.Location;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.sobek.core.interfaces.IAbstractConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.model.BoundaryNode;
import org.kalypso.model.wspm.sobek.core.model.LinkageNode;
import org.kalypso.model.wspm.sobek.core.model.SbkStructWeir;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author thuel2
 */
public class PiSobekModelUtils
{

  public Map<String, String> lookUpModelToPi = new HashMap<String, String>()
  {
    static final long serialVersionUID = 1L;

    /**
     * @see java.util.HashMap#put(java.lang.Object, java.lang.Object)
     *      <p>
     *      both pairs (key, value) and (value, key) are added to the HashMap. If key and value evaluate to the same
     *      string only one pair is added.
     *      </p>
     */
    @Override
    public String put( final String key, final String value )
    {
      if( !(null == super.put( key, value )) )
        throw new IllegalArgumentException( key + " is not unique in HashMap lookUpModelToPi." );
      if( !key.equals( value ) )
        if( !(null == super.put( value, key )) )
          throw new IllegalArgumentException( value + " is not unique in HashMap lookUpModelToPi." );
      return null;
    }

    @Override
    public String get( Object key )
    {
      if( super.containsKey( key ) )
        return super.get( key );

      throw new IllegalArgumentException( key.toString() + " can't be found in HashMap lookUpModelToPi." );
    }
  };

  private static PiSobekModelUtils instance = null;

  private PiSobekModelUtils( )
  {
    lookUpModelToPi.put( BOUNDARY_TYPE.eQ.toString(), "Sobek.Nodes.Bound_Q" );
    lookUpModelToPi.put( BOUNDARY_TYPE.eW.toString(), "Sobek.Nodes.Bound_H" );
    lookUpModelToPi.put( INode.TYPE.eLinkageNode.toString(), "Sobek.Nodes.linkage" );
    lookUpModelToPi.put( INode.TYPE.eConnectionNode.toString(), "Sobek.Nodes.Connection" );

  }

  public static PiSobekModelUtils getInstance( )
  {
    if( PiSobekModelUtils.instance == null )
      PiSobekModelUtils.instance = new PiSobekModelUtils();
    return PiSobekModelUtils.instance;
  }

  public Location createLocationFromNode( final ObjectFactory factory, final INode node )
  {
    final Location location = factory.createLocationsComplexTypeLocation();

    location.setLocationId( node.getId() );

    final String stationName = node.getStationName();
    // stationName has to be set in PI but will be ignored by Sobek
    if( stationName != null )
      location.setStationName( stationName );
    else
      location.setStationName( node.getName() );
    location.setLongName( node.getName() );
    location.setX( node.getLocation().getX() );
    location.setY( node.getLocation().getY() );

    if( node instanceof IAbstractConnectionNode )
      if( node instanceof IConnectionNode )
        location.setLocationType( lookUpModelToPi.get( node.getType().toString() ) );
      else if( node instanceof ILinkageNode )
      {
        final LinkageNode ln = (LinkageNode) node;
        final IBranch linkedBranch = ln.getLinkToBranch();
        if( linkedBranch == null )
          throw new IllegalArgumentException( "Missing linked branch for linkage node " + ln.getName() );
        else
          location.setLocationType( lookUpModelToPi.get( ln.getType().toString() ) + "@" + linkedBranch.getId() );

      }
      else if( node instanceof IBoundaryNode )
      {
        final BoundaryNode bn = (BoundaryNode) node;
        location.setLocationType( lookUpModelToPi.get( bn.getBoundaryType().toString() ) );
      }
    return location;
  }

  public BranchComplexType createPiBranchFromBranch( final ObjectFactory factory, final IBranch branch ) throws GM_Exception
  {
    final BranchComplexType piBranch = factory.createBranchComplexType();
    piBranch.setBranchId( branch.getId() );
    piBranch.setBranchName( branch.getName() );
    final String description = branch.getDescription();
    if( !(description == null) )
      piBranch.setComment( description );
    piBranch.setDownNode( branch.getLowerNode().getId() );
    piBranch.setUpNode( branch.getUpperNode().getId() );

    piBranch.setStartChainage( 0.0 );
    final GM_Curve lineGeom = branch.getGeometryProperty();
    piBranch.setEndChainage( lineGeom.getLength() );

    // at first list of pt's represent true geometry of the branch
    final GM_Position[] positions = lineGeom.getAsLineString().getPositions();
    if( positions.length > 0 )
    {
      final Coordinate jtsFirstPos = JTSAdapter.export( positions[0] );
      double chainage = 0;

      for( final GM_Position position : positions )
      {
        final NodePointComplexType pt = factory.createNodePointComplexType();
        pt.setX( position.getX() );
        pt.setY( position.getY() );
        pt.setLabel( "" ); // label has to be set but will be ignored during import to Sobek

        chainage = chainage + JTSUtilities.getLengthBetweenPoints( jtsFirstPos, JTSAdapter.export( position ) );
        pt.setChainage( chainage ); // chainage has to be set but will be ignored during import to Sobek

        piBranch.getPt().add( pt );
      }
    }

    return piBranch;
  }

  public CrossSection createCrossSectionFromCSNode( final ObjectFactory factory, final ICrossSectionNode csNode )
  {
    final CrossSection piCrossSection = factory.createCrossSectionsComplexTypeCrossSection();
    piCrossSection.setX( csNode.getLocation().getX() );
    piCrossSection.setY( csNode.getLocation().getY() );
    piCrossSection.setCrossSectionID( csNode.getId() );
    piCrossSection.setBranchId( csNode.getLinkToBranch().getId() );
    piCrossSection.setCrossSectionName( csNode.getName() );
    piCrossSection.setLabel( csNode.getName() ); // label has to be set but will be ignored by import to Sobek
    final String description = csNode.getDescription();
    if( !(description == null) )
      piCrossSection.setComment( description );

    piCrossSection.setRoughnessType( "Sobek.RoughnessType.StricklerKs" ); // nofdp default kSt

    final IProfil profil = csNode.getProfile();
    final LinkedList<IProfilPoint> points = profil.getPoints();

    for( final IProfilPoint point : points )
    {
      final CrossSectionXdataComplexType csData = factory.createCrossSectionXdataComplexType();
      csData.setCsy( point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) );
      csData.setZ( point.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE ) );
// TODO get real Roughness from nofdpIDSSProfile
// csData.setRoughness( point.getValueFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) );
      csData.setRoughness( 2.1 );
      csData.setMark( new BigInteger( "0" ) );

      piCrossSection.getCrossSectionData().add( csData );
    }

    return piCrossSection;
  }

  public Structure createStructureFromSbkStruct( final ObjectFactory factory, final ISbkStructure sbkStruct )
  {
    final Structure piStruct = new Structure();
    piStruct.setBranchId( sbkStruct.getLinkToBranch().getId() );
    piStruct.setStructureId( sbkStruct.getId() );
    final String name = sbkStruct.getName();
    if( name != null )
      piStruct.setStructureName( name );

    // FIXME
    // piStruct.setX( new BigDecimal( sbkStruct.getLocation().getX() ) );
    // piStruct.setY( new BigDecimal( sbkStruct.getLocation().getY() ) );

    final StructureDefinitions structureDefinitions = factory.createStructureDefinitions();

    final List<ISbkStructure> allStructs = new ArrayList<ISbkStructure>();
    // TODO aus CompoundStructure noch alle auslesen
    allStructs.add( sbkStruct );
    for( final ISbkStructure sbkStructure : allStructs )
      if( sbkStructure instanceof SbkStructWeir )
      {
        final StructureDefinition structureDefinition = getStructureDefFromSbkWeir( factory, sbkStruct );
        structureDefinitions.getStructureDefinition().add( structureDefinition );
      }
      else
        throw new NotImplementedException();

    piStruct.setStructureDefinitions( structureDefinitions );
    return piStruct;
  }

  private StructureDefinition getStructureDefFromSbkWeir( final ObjectFactory factory, final ISbkStructure sbkStruct )
  {
    final ISbkStructWeir sbkWeir = (ISbkStructWeir) sbkStruct;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    final List<Object> content = structureDefinition.getContent();
    content.add( factory.createStructureDefinitionId( sbkWeir.getId() + "_weir" ) );
    String name = sbkWeir.getName();
    if( name == null )
      name = "";
    content.add( factory.createStructureDefinitionName( name + "_weir" ) );
    content.add( factory.createStructureDefinitionType( "Sobek.Structures.Weir" ) );

    final Parameters parameters = factory.createParameters();
    Parameter parameter = factory.createParameter();
    parameter.setId( "FlowDirection" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkWeir.getFlowDirection() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "CrestLevel" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getCrestLevel() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "CrestWidth" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getCrestWidth() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "DischargeCoef" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getDischargeCoeffCE() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "LateralContractioncCoef" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getLateralContractionCoeffCW() ) );
    parameters.getParameter().add( parameter );

    content.add( parameters );
    return structureDefinition;
  }

  public TimeSerieComplexType createTimeSeriesFromBNNodeAndLastfall( final ObjectFactory factory, final IBoundaryNode bnNode, final ILastfall lastfall ) throws Exception
  {
    final TimeSerieComplexType piTimeSerie = factory.createTimeSerieComplexType();

    final HeaderComplexType headerComplexType = factory.createHeaderComplexType();
    headerComplexType.setType( TimeSeriesType.INSTANTANEOUS );
    headerComplexType.setMissVal( Double.NaN );

    final IBoundaryNodeLastfallCondition lastfallCondition = bnNode.getLastfallCondition( lastfall );

    final GregorianCalendar observationStart = lastfallCondition.getObservationStart();
    final DatatypeFactory datatypeFactory = DatatypeFactory.newInstance();
    final DatatypeFactory dataTypeFactory = datatypeFactory;
    final XMLGregorianCalendar gregorianStartCalendar = dataTypeFactory.newXMLGregorianCalendar( observationStart );
    final DateTimeComplexType startDateTimeComplexType = factory.createDateTimeComplexType();

    final XMLGregorianCalendar startTime = datatypeFactory.newXMLGregorianCalendarTime( gregorianStartCalendar.getHour(), gregorianStartCalendar.getMinute(), gregorianStartCalendar.getSecond(), DatatypeConstants.FIELD_UNDEFINED );
    final XMLGregorianCalendar startDate = datatypeFactory.newXMLGregorianCalendarDate( gregorianStartCalendar.getYear(), gregorianStartCalendar.getMonth(), gregorianStartCalendar.getDay(), DatatypeConstants.FIELD_UNDEFINED );
    startDateTimeComplexType.setDate( startDate );
    startDateTimeComplexType.setTime( startTime );
    headerComplexType.setStartDate( startDateTimeComplexType );

    final GregorianCalendar observationEnd = lastfallCondition.getObservationEnd();
    final XMLGregorianCalendar gregorianEndCalendar = dataTypeFactory.newXMLGregorianCalendar( observationEnd );
    final DateTimeComplexType endDateTimeComplexType = factory.createDateTimeComplexType();

    final XMLGregorianCalendar endTime = datatypeFactory.newXMLGregorianCalendarTime( gregorianEndCalendar.getHour(), gregorianEndCalendar.getMinute(), gregorianEndCalendar.getSecond(), DatatypeConstants.FIELD_UNDEFINED );
    final XMLGregorianCalendar endDate = datatypeFactory.newXMLGregorianCalendarDate( gregorianEndCalendar.getYear(), gregorianEndCalendar.getMonth(), gregorianEndCalendar.getDay(), DatatypeConstants.FIELD_UNDEFINED );
    endDateTimeComplexType.setDate( endDate );
    endDateTimeComplexType.setTime( endTime );
    headerComplexType.setEndDate( endDateTimeComplexType );

    headerComplexType.setLocationId( bnNode.getId() );
    headerComplexType.setStationName( bnNode.getStationName() );

    final TimeStepComplexType timeStepComplexType = factory.createTimeStepComplexType();
    timeStepComplexType.setUnit( TimeStepUnitEnumStringType.NONEQUIDISTANT );
    headerComplexType.setTimeStep( timeStepComplexType );

    final BOUNDARY_TYPE boundaryType = bnNode.getBoundaryType();
    headerComplexType.setParameterId( lookUpModelToPi.get( boundaryType.toString() ) );
    piTimeSerie.setHeader( headerComplexType );

    final IObservation<TupleResult> timeSeriesTupple = lastfallCondition.getTimeSeriesObservation();
    lastfallCondition.getTimeSeriesObservationFeature();
    final TupleResult tupleResult = timeSeriesTupple.getResult();

    IComponent valueComp = null;
    final IComponent dateComp = TupleResultUtilities.findComponentById( tupleResult, "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#DATE" );
    if( boundaryType.equals( BOUNDARY_TYPE.eQ ) )
      valueComp = TupleResultUtilities.findComponentById( tupleResult, "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#Q" );
    else if( boundaryType.equals( BOUNDARY_TYPE.eW ) )
      valueComp = TupleResultUtilities.findComponentById( tupleResult, "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#W" );
    else
      throw new NotImplementedException( "Boundary condition of type " + boundaryType + " can't be transferred by PI format yet." );

    final Iterator<IRecord> itrResult = tupleResult.iterator();
    while( itrResult.hasNext() )
    {
      final IRecord record = itrResult.next();
      final EventComplexType eventComplexType = factory.createEventComplexType();

      final XMLGregorianCalendar gregEventCalendar = (XMLGregorianCalendar) record.getValue( dateComp );
      final XMLGregorianCalendar date = datatypeFactory.newXMLGregorianCalendarDate( gregEventCalendar.getYear(), gregEventCalendar.getMonth(), gregEventCalendar.getDay(), DatatypeConstants.FIELD_UNDEFINED );
      final XMLGregorianCalendar time = datatypeFactory.newXMLGregorianCalendarTime( gregEventCalendar.getHour(), gregEventCalendar.getMinute(), gregEventCalendar.getSecond(), DatatypeConstants.FIELD_UNDEFINED );

      eventComplexType.setDate( date );
      eventComplexType.setTime( time );
      eventComplexType.setValue( Double.valueOf( (Double) record.getValue( valueComp ) ) );

      piTimeSerie.getEvent().add( eventComplexType );
    }

    return piTimeSerie;
  }
}
