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
package org.kalypso.model.wspm.sobek.core.utils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import nl.wldelft.fews.pi.BranchComplexType;
import nl.wldelft.fews.pi.Column;
import nl.wldelft.fews.pi.Columns;
import nl.wldelft.fews.pi.CrossSectionXdataComplexType;
import nl.wldelft.fews.pi.DateTimeComplexType;
import nl.wldelft.fews.pi.EventComplexType;
import nl.wldelft.fews.pi.HeaderComplexType;
import nl.wldelft.fews.pi.Info;
import nl.wldelft.fews.pi.Interpolation;
import nl.wldelft.fews.pi.NodePointComplexType;
import nl.wldelft.fews.pi.ObjectFactory;
import nl.wldelft.fews.pi.Parameter;
import nl.wldelft.fews.pi.Parameters;
import nl.wldelft.fews.pi.Row;
import nl.wldelft.fews.pi.Rows;
import nl.wldelft.fews.pi.Structure;
import nl.wldelft.fews.pi.StructureDefinition;
import nl.wldelft.fews.pi.StructureDefinitions;
import nl.wldelft.fews.pi.Table;
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
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.sobek.core.interfaces.IAbstractConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructCompundStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.model.BoundaryNode;
import org.kalypso.model.wspm.sobek.core.model.LinkageNode;
import org.kalypso.model.wspm.sobek.core.model.SbkStructCompoundStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructDatabaseStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructGeneralStructure;
import org.kalypso.model.wspm.sobek.core.model.SbkStructPump;
import org.kalypso.model.wspm.sobek.core.model.SbkStructRiverWeir;
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

  private static PiSobekModelUtils instance = null;

  public static PiSobekModelUtils getInstance( )
  {
    if( PiSobekModelUtils.instance == null )
      PiSobekModelUtils.instance = new PiSobekModelUtils();
    return PiSobekModelUtils.instance;
  }

  public Map<String, String> lookUpModelToPi = new HashMap<String, String>()
  {
    static final long serialVersionUID = 1L;

    @Override
    public String get( Object key )
    {
      if( super.containsKey( key ) )
        return super.get( key );

      throw new IllegalArgumentException( key.toString() + " can't be found in HashMap lookUpModelToPi." );
    }

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
  };

  private PiSobekModelUtils( )
  {
    lookUpModelToPi.put( BOUNDARY_TYPE.eQ.toString(), "Sobek.Nodes.Bound_Q" );
    lookUpModelToPi.put( BOUNDARY_TYPE.eW.toString(), "Sobek.Nodes.Bound_H" );
    lookUpModelToPi.put( INode.TYPE.eLinkageNode.toString(), "Sobek.Nodes.linkage" );
    lookUpModelToPi.put( INode.TYPE.eConnectionNode.toString(), "Sobek.Nodes.Connection" );

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
    final IRecord[] points = profil.getPoints();

    for( final IRecord point : points )
    {
      final CrossSectionXdataComplexType csData = factory.createCrossSectionXdataComplexType();
      csData.setCsy( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) ) );
      csData.setZ( (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) ) );
// TODO get real Roughness from nofdpIDSSProfile
// csData.setRoughness( point.getValueFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) );
      csData.setRoughness( 2.1 );
      csData.setMark( new BigInteger( "0" ) );

      piCrossSection.getCrossSectionData().add( csData );
    }

    return piCrossSection;
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

  public Structure createStructureFromSbkStruct( final ObjectFactory factory, final ISbkStructure sbkStruct )
  {
    final Structure piStruct = new Structure();
    piStruct.setBranchId( sbkStruct.getLinkToBranch().getId() );
    piStruct.setStructureId( sbkStruct.getId() );
    final String name = sbkStruct.getName();
    if( name != null )
      piStruct.setStructureName( name );
    piStruct.setX( sbkStruct.getLocation().getX() );
    piStruct.setY( sbkStruct.getLocation().getY() );

    final StructureDefinitions structureDefinitions = factory.createStructureDefinitions();

    final List<ISbkStructure> allStructs = new ArrayList<ISbkStructure>();
    if( sbkStruct instanceof SbkStructCompoundStructure )
    {
      final ISbkStructCompundStructure sbkCompoundStruct = (ISbkStructCompundStructure) sbkStruct;
      allStructs.addAll( Arrays.asList( sbkCompoundStruct.getContainedStructures() ) );
    }
    else
      allStructs.add( sbkStruct );

    for( final ISbkStructure sbkStructure : allStructs )
    {
      final StructureDefinition structureDefinition = getStructureDefFromSbkSimpleStruct( factory, sbkStructure );
      structureDefinitions.getStructureDefinition().add( structureDefinition );
    }

    piStruct.setStructureDefinitions( structureDefinitions );
    return piStruct;
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

  private StructureDefinition getStructureDefFromSbkDatabaseStructure( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    final ISbkStructDatabaseStructure sbkDBStruct = (ISbkStructDatabaseStructure) sbkStructure;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    structureDefinition.setCrossSectionType( "Sobek.Structure.CrossSection.None" );
    String name = sbkDBStruct.getName();
    if( name == null )
      name = "";
    structureDefinition.setStructureDefinitionId( sbkDBStruct.getId() + "_dbStruct" );
    structureDefinition.setStructureDefinitionName( name + "_dbStruct" );
    structureDefinition.setStructureDefinitionType( "Sobek.Structures.DatabaseStructure" );

    final Parameters parameters = factory.createParameters();

    Parameter parameter = factory.createParameter();
    parameter.setId( "crestLevel" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkDBStruct.getCrestHeight() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "numberOfGateValues" );
    parameter.setType( "long" );
    parameter.getContent().add( Long.toString( sbkDBStruct.getNumOfGateValues() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "secondAxisValue" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkDBStruct.getSecondAxisValueType() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "interpolationType" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkDBStruct.getInterpolationType() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "database" );
    parameter.setType( "table" );
    final Table innerTableDB = factory.createTable();
    innerTableDB.setName( "database" );

    final Info tableInfoDB = factory.createInfo();

    final Interpolation interpolationDB = factory.createInterpolation();

    interpolationDB.setPeriod( sbkDBStruct.getDatabase().getInterpolationPeriod() );
    interpolationDB.setType( sbkDBStruct.getDatabase().getInterpolationType() );
    interpolationDB.setValue( sbkDBStruct.getDatabase().getInterpolationValue() );

    tableInfoDB.setInterpolation( interpolationDB );

    final Columns columnsDB = factory.createColumns();
    final int colCountDB = 2;
    columnsDB.setCount( ((Number) colCountDB).byteValue() );

    final Column columnDB = factory.createColumn();
    columnDB.setType( "double" );
    for( int i = 0; i < colCountDB; i++ )
      columnsDB.getColumn().add( columnDB );
    tableInfoDB.setColumns( columnsDB );

    innerTableDB.setInfo( tableInfoDB );

    final Rows tableRowsDB = factory.createRows();
    final long rowsCountDB = sbkDBStruct.getDatabase().getRowsCount();
    for( int i = 0; i < rowsCountDB; i++ )
    {
      final Row tableRow = factory.createRow();
      // TODO
      // tableRow.getCell().add( Double.toString( sbkPump.getCapacity() ) );
      // tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelSuctionSide() ) );
      tableRowsDB.getRow().add( tableRow );
    }

    innerTableDB.setRows( tableRowsDB );

    parameter.getContent().add( innerTableDB );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "databaseUsage" );
    parameter.setType( "table" );
    final Table innerTableDBUsage = factory.createTable();
    innerTableDBUsage.setName( "databaseUsage" );

    final Info tableInfoDBUsage = factory.createInfo();

    final Interpolation interpolationDBUsage = factory.createInterpolation();

    interpolationDBUsage.setPeriod( sbkDBStruct.getDatabaseUsage().getInterpolationPeriod() );
    interpolationDBUsage.setType( sbkDBStruct.getDatabaseUsage().getInterpolationType() );
    interpolationDBUsage.setValue( sbkDBStruct.getDatabaseUsage().getInterpolationValue() );

    tableInfoDBUsage.setInterpolation( interpolationDBUsage );

    final Columns columnsDBUsage = factory.createColumns();
    final int colCountDBUsage = 2;
    columnsDBUsage.setCount( ((Number) colCountDBUsage).byteValue() );

    final Column columnDBUsage = factory.createColumn();
    columnDBUsage.setType( "double" );
    for( int i = 0; i < colCountDBUsage; i++ )
      columnsDBUsage.getColumn().add( columnDBUsage );
    tableInfoDBUsage.setColumns( columnsDBUsage );

    innerTableDBUsage.setInfo( tableInfoDBUsage );

    final Rows tableRowsDBUsage = factory.createRows();
    final long rowsCountDBUsage = sbkDBStruct.getDatabaseUsage().getRowsCount();
    for( int i = 0; i < rowsCountDBUsage; i++ )
    {
      final Row tableRow = factory.createRow();
      // TODO
      // tableRow.getCell().add( Double.toString( sbkPump.getCapacity() ) );
      // tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelSuctionSide() ) );
      tableRowsDBUsage.getRow().add( tableRow );
    }

    innerTableDBUsage.setRows( tableRowsDBUsage );

    parameter.getContent().add( innerTableDBUsage );
    parameters.getParameter().add( parameter );

    structureDefinition.setParameters( parameters );
    return structureDefinition;
  }

  private StructureDefinition getStructureDefFromSbkGeneralStructure( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    final ISbkStructGeneralStructure sbkGenStruct = (ISbkStructGeneralStructure) sbkStructure;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    structureDefinition.setCrossSectionType( "Sobek.Structure.CrossSection.None" );
    String name = sbkGenStruct.getName();
    if( name == null )
      name = "";
    structureDefinition.setStructureDefinitionId( sbkGenStruct.getId() + "_genStruct" );
    structureDefinition.setStructureDefinitionName( name + "_genStruct" );
    structureDefinition.setStructureDefinitionType( "Sobek.Structures.GeneralStructure" );

    final Parameters parameters = factory.createParameters();

    Parameter parameter = factory.createParameter();
    parameter.setId( "widthUpstream" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getWidthUpstream() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "widthStructureLeft" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getWidthStructureLeft() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "widthStructure" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getWidthStructure() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "widthStructureRight" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getWidthStructureRight() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "widthDownstream" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getWidthDownstream() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "bedLevelUpstream" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getBedLevelUpstream() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "bedLevelStructureLeft" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getBedLevelStructureLeft() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "bedLevelStructure" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getBedLevelStructure() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "bedLevelStructureRight" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getBedLevelStructureRight() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "bedLevelDownstream" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getBedLevelDownstream() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "gateHeight" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getGateHeight() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveFreeGateFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getPosFreeGateFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveDrownedGateFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getPosDrownedGateFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveFreeWeirFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getPosFreeWeirFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveDrownedWeirFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getPosDrownedWeirFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveContractionCoefficient" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getPosContractionCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeFreeGateFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getNegFreeGateFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeDrownedGateFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getNegDrownedGateFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeFreeWeirFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getNegFreeWeirFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeDrownedWeirFlow" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getNegDrownedWeirFlowCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeContractionCoefficient" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getNegContractionCoeff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "extraResistence" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkGenStruct.getExtraResistence() ) );
    parameters.getParameter().add( parameter );

    structureDefinition.setParameters( parameters );
    return structureDefinition;
  }

  private StructureDefinition getStructureDefFromSbkPump( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    final ISbkStructPump sbkPump = (ISbkStructPump) sbkStructure;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    structureDefinition.setCrossSectionType( "Sobek.Structure.CrossSection.None" );
    String name = sbkPump.getName();
    if( name == null )
      name = "";
    structureDefinition.setStructureDefinitionId( sbkPump.getId() + "_pump" );
    structureDefinition.setStructureDefinitionName( name + "_pump" );
    structureDefinition.setStructureDefinitionType( "Sobek.Structures.Pump" );

    final Parameters parameters = factory.createParameters();

    Parameter parameter = factory.createParameter();
    parameter.setId( "flowDirection" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkPump.getFlowDirection() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "controlPosition" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkPump.getControlPosition() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "reductionType" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkPump.getReductionType() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "reductionConstant" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkPump.getReductionConstant() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "capacitySettings" );
    parameter.setType( "table" );
    final Table innerTable = factory.createTable();
    innerTable.setName( "capacitySettings" );

    final Info tableInfo = factory.createInfo();
    final Columns columns = factory.createColumns();
    final int colCount = 5;
    columns.setCount( ((Number) colCount).byteValue() );

    final Column column = factory.createColumn();
    column.setType( "double" );
    for( int i = 0; i < colCount; i++ )
      columns.getColumn().add( column );
    tableInfo.setColumns( columns );

    innerTable.setInfo( tableInfo );

    final Rows tableRows = factory.createRows();
    final Row tableRow = factory.createRow();
    tableRow.getCell().add( Double.toString( sbkPump.getCapacity() ) );
    tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelSuctionSide() ) );
    tableRow.getCell().add( Double.toString( sbkPump.getSwitchOffLevelSuctionSide() ) );
    tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelPressureSide() ) );
    tableRow.getCell().add( Double.toString( sbkPump.getSwitchOffLevelPressureSide() ) );

    tableRows.getRow().add( tableRow );
    innerTable.setRows( tableRows );

    parameter.getContent().add( innerTable );
    parameters.getParameter().add( parameter );

    structureDefinition.setParameters( parameters );
    return structureDefinition;
  }

  private StructureDefinition getStructureDefFromSbkRiverWeir( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    final ISbkStructRiverWeir sbkRiverWeir = (ISbkStructRiverWeir) sbkStructure;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    structureDefinition.setCrossSectionType( "Sobek.Structure.CrossSection.None" );
    String name = sbkRiverWeir.getName();
    if( name == null )
      name = "";
    structureDefinition.setStructureDefinitionId( sbkRiverWeir.getId() + "_riverWeir" );
    structureDefinition.setStructureDefinitionName( name + "_riverWeir" );
    structureDefinition.setStructureDefinitionType( "Sobek.Structures.RiverWeir" );

    final Parameters parameters = factory.createParameters();

    Parameter parameter = factory.createParameter();
    parameter.setId( "crestLevel" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getCrestLevel() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "crestWidth" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getCrestWidth() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "crestShape" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkRiverWeir.getCrestShape() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveCorrectionCoefficient" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getPosCorrectionCeoff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveSubmergeLimit" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getPosSubmergeLimit() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "positiveReductionFactors" );
    parameter.setType( "table" );
    final Table innerTablePos = factory.createTable();
    innerTablePos.setName( "positiveReductionFactors" );

    final Info tableInfoPos = factory.createInfo();

    final Interpolation interpolationPos = factory.createInterpolation();

    interpolationPos.setPeriod( sbkRiverWeir.getPosReductionFactors().getInterpolationPeriod() );
    interpolationPos.setType( sbkRiverWeir.getPosReductionFactors().getInterpolationType() );
    interpolationPos.setValue( sbkRiverWeir.getPosReductionFactors().getInterpolationValue() );

    tableInfoPos.setInterpolation( interpolationPos );

    final Columns columnsPos = factory.createColumns();
    final int colCountPos = 2;
    columnsPos.setCount( ((Number) colCountPos).byteValue() );

    final Column columnPos = factory.createColumn();
    columnPos.setType( "double" );
    for( int i = 0; i < colCountPos; i++ )
      columnsPos.getColumn().add( columnPos );
    tableInfoPos.setColumns( columnsPos );

    innerTablePos.setInfo( tableInfoPos );

    final TupleResult result = sbkRiverWeir.getPosReductionFactors().getTupleResult();
// Submergeance and Reduction
    // als Konstante festlegen
// <catalog:system systemId="urn:ogc:gml:dict:kalypso:wspm:sobek:sbkRiverWeirObservationDefs"
// uri="dict_sbk_river_weir_observation_defs.gml
// SUBMERGEANCE, REDUCTION

// final IComponent compY = TupleResultUtilities.findComponentById( result, CriterionValueBenefitRating.DICT_ID_RATING
    final Rows tableRowsPos = factory.createRows();
    final long rowsCountPos = sbkRiverWeir.getPosReductionFactors().getRowsCount();
    for( int i = 0; i < rowsCountPos; i++ )
    {
      final Row tableRow = factory.createRow();
      // TODO
      // tableRow.getCell().add( Double.toString( sbkPump.getCapacity() ) );
      // tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelSuctionSide() ) );
      tableRowsPos.getRow().add( tableRow );
    }

    innerTablePos.setRows( tableRowsPos );

    parameter.getContent().add( innerTablePos );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeCorrectionCoefficient" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getNegCorrectionCeoff() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeSubmergeLimit" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkRiverWeir.getNegSubmergeLimit() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "negativeReductionFactors" );
    parameter.setType( "table" );
    final Table innerTableNeg = factory.createTable();
    innerTableNeg.setName( "negativeReductionFactors" );

    final Info tableInfoNeg = factory.createInfo();

    final Interpolation interpolationNeg = factory.createInterpolation();

    interpolationNeg.setPeriod( sbkRiverWeir.getNegReductionFactors().getInterpolationPeriod() );
    interpolationNeg.setType( sbkRiverWeir.getNegReductionFactors().getInterpolationType() );
    interpolationNeg.setValue( sbkRiverWeir.getNegReductionFactors().getInterpolationValue() );

    tableInfoNeg.setInterpolation( interpolationNeg );

    final Columns columnsNeg = factory.createColumns();
    final int colCountNeg = 2;
    columnsNeg.setCount( ((Number) colCountNeg).byteValue() );

    final Column columnNeg = factory.createColumn();
    columnNeg.setType( "double" );
    for( int i = 0; i < colCountNeg; i++ )
      columnsNeg.getColumn().add( columnNeg );
    tableInfoNeg.setColumns( columnsNeg );

    innerTableNeg.setInfo( tableInfoNeg );

    final Rows tableRowsNeg = factory.createRows();
    final long rowsCountNeg = sbkRiverWeir.getNegReductionFactors().getRowsCount();
    for( int i = 0; i < rowsCountNeg; i++ )
    {
      final Row tableRow = factory.createRow();
      // TODO
      // tableRow.getCell().add( Double.toString( sbkPump.getCapacity() ) );
      // tableRow.getCell().add( Double.toString( sbkPump.getSwitchOnLevelSuctionSide() ) );
      tableRowsNeg.getRow().add( tableRow );
    }

    innerTableNeg.setRows( tableRowsNeg );

    parameter.getContent().add( innerTableNeg );
    parameters.getParameter().add( parameter );

    structureDefinition.setParameters( parameters );
    return structureDefinition;
  }

  private StructureDefinition getStructureDefFromSbkSimpleStruct( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    StructureDefinition structureDefinition = null;

    if( sbkStructure instanceof SbkStructRiverWeir )
      structureDefinition = getStructureDefFromSbkRiverWeir( factory, sbkStructure );
    else if( sbkStructure instanceof SbkStructGeneralStructure )
      structureDefinition = getStructureDefFromSbkGeneralStructure( factory, sbkStructure );
    else if( sbkStructure instanceof SbkStructDatabaseStructure )
      structureDefinition = getStructureDefFromSbkDatabaseStructure( factory, sbkStructure );
    else if( sbkStructure instanceof SbkStructWeir )
      structureDefinition = getStructureDefFromSbkWeir( factory, sbkStructure );
    else if( sbkStructure instanceof SbkStructPump )
      structureDefinition = getStructureDefFromSbkPump( factory, sbkStructure );
    else
      throw new NotImplementedException();

    return structureDefinition;
  }

  private StructureDefinition getStructureDefFromSbkWeir( final ObjectFactory factory, final ISbkStructure sbkStructure )
  {
    final ISbkStructWeir sbkWeir = (ISbkStructWeir) sbkStructure;
    final StructureDefinition structureDefinition = factory.createStructureDefinition();

    structureDefinition.setCrossSectionType( "Sobek.Structure.CrossSection.None" );
    String name = sbkWeir.getName();
    if( name == null )
      name = "";
    structureDefinition.setStructureDefinitionId( sbkWeir.getId() + "_weir" );
    structureDefinition.setStructureDefinitionName( name + "_weir" );
    structureDefinition.setStructureDefinitionType( "Sobek.Structures.Weir" );

    final Parameters parameters = factory.createParameters();

    Parameter parameter = factory.createParameter();
    parameter.setId( "flowDirection" );
    parameter.setType( "string" );
    parameter.getContent().add( sbkWeir.getFlowDirection() );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "crestLevel" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getCrestLevel() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "crestWidth" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getCrestWidth() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "dischargeCoef" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getDischargeCoeffCE() ) );
    parameters.getParameter().add( parameter );

    parameter = factory.createParameter();
    parameter.setId( "lateralContractioncCoef" );
    parameter.setType( "double" );
    parameter.getContent().add( Double.toString( sbkWeir.getLateralContractionCoeffCW() ) );
    parameters.getParameter().add( parameter );

    structureDefinition.setParameters( parameters );

    return structureDefinition;
  }
}
