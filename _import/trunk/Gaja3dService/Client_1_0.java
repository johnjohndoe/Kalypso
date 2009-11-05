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
package org.kalypso.gaja3d.service.client;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.apache.axis.client.Stub;
import org.apache.axis.message.MessageElement;
import org.apache.axis.message.addressing.Address;
import org.apache.axis.message.addressing.EndpointReferenceType;
import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.PropertyConfigurator;
import org.globus.axis.gsi.GSIConstants;
import org.globus.wsrf.encoding.ObjectDeserializer;
import org.globus.wsrf.encoding.ObjectSerializer;
import org.globus.wsrf.encoding.SerializationException;
import org.globus.wsrf.impl.security.authorization.SelfAuthorization;
import org.globus.wsrf.security.Constants;
import org.kalypso.gaja3d.service.Gaja3DServiceAddressingLocator;
import org.kalypso.gaja3d.service.factory.Gaja3DResourceFactoryServiceAddressingLocator;
import org.kalypso.gaja3d.service.factory.stubs.CreateResource;
import org.kalypso.gaja3d.service.factory.stubs.CreateResourceResponse;
import org.kalypso.gaja3d.service.factory.stubs.Gaja3DResourceFactoryPortType;
import org.kalypso.gaja3d.service.impl.Gaja3dQNames;
import org.kalypso.gaja3d.service.stubs.Boundary;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.CreateGridParametersType;
import org.kalypso.gaja3d.service.stubs.CreateTinParametersType;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DemPoints;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesParametersType;
import org.kalypso.gaja3d.service.stubs.Gaja3DPortType;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.gaja3d.service.stubs.SmoothFilterMethod;
import org.kalypso.service.wps.utils.MarshallUtilities;
import org.kalypso.service.wps.utils.WPSUtilities.WPS_VERSION;
import org.oasis.wsrf.lifetime.Destroy;
import org.oasis.wsrf.properties.GetMultipleResourcePropertiesResponse;
import org.oasis.wsrf.properties.GetMultipleResourceProperties_Element;
import org.oasis.wsrf.properties.SetResourceProperties_Element;
import org.oasis.wsrf.properties.UpdateType;
import org.xml.sax.InputSource;

import www.opengis.net.ows.stubs.AcceptVersionsType;
import www.opengis.net.ows.stubs.CodeType;
import www.opengis.net.ows.stubs.GetCapabilitiesType;
import www.opengis.net.wps.stubs.DescribeProcess;
import www.opengis.net.wps.stubs.ProcessBriefType;
import www.opengis.net.wps.stubs.ProcessDescriptions;
import www.opengis.net.wps.stubs.ProcessOfferings;
import www.opengis.net.wps.stubs.WPSCapabilitiesType;

/**
 * This client creates a new Gaja3dService instance through a Gaja3dResourceFactoryService or by using the file
 * "test.epr" as EndPointReference. The service's GetCapabilities, DescribeProcess, CreateGrid, DetectBreaklines and
 * CreateTin operations can be called in order.
 */
public class Client_1_0
{

// private static final String SERVICE_FACTORY_URI =
  // "http://gdi-kalypso1.gridlab.uni-hannover.de:8080/services/Gaja3dResourceFactoryService";

  private static final String SERVICE_FACTORY_URI = "http://localhost:8080/services/Gaja3dResourceFactoryService";

  private static final String EPR_FILENAME = "test.epr";

  private static final boolean SAVE_EPR = true;

  private static final boolean USE_SAVED_EPR = false;

  public static final String BOUNDARY_FILENAME = "resources/boundary1.zip";

  public static final String DEMPOINTS_FILENAME = "resources/allpoints.zip";

  /*
   * for skipping grid creation
   */
  public static final String DEMGRID_FILENAME = "resources/DemGrid.asc";

  /*
   * for skipping breakline creation
   */
  public static final String BREAKLINES_FILENAME = "resources/breaklines.zip";

  public static final String MODEL_TIN_FILENAME = "resources/ModelTin.zip";

  private static Log logger = LogFactory.getLog( Client_1_0.class.getName() );

  public static void main( String[] args ) throws RemoteException, ServiceException
  {
    PropertyConfigurator.configure( Client_1_0.class.getResource( "log4j.properties" ) );

    final EndpointReferenceType instanceEPR;

    long last = System.currentTimeMillis();

    if( USE_SAVED_EPR )
    {
      try
      {
        final InputSource inputSource = new InputSource( new FileInputStream( EPR_FILENAME ) );
        instanceEPR = (EndpointReferenceType) ObjectDeserializer.deserialize( inputSource, EndpointReferenceType.class );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        return;
      }
    }
    else
    {
      final Gaja3DResourceFactoryServiceAddressingLocator factoryLocator = new Gaja3DResourceFactoryServiceAddressingLocator();
      final EndpointReferenceType factoryEPR = new EndpointReferenceType();
      try
      {
        final Address address = new Address( SERVICE_FACTORY_URI );
        factoryEPR.setAddress( address );
      }
      catch( final MalformedURIException e )
      {
        e.printStackTrace();
      }

      Gaja3DResourceFactoryPortType gaja3dFactory;
      try
      {
        gaja3dFactory = factoryLocator.getGaja3dResourceFactoryPortTypePort( factoryEPR );
      }
      catch( final ServiceException e )
      {
        e.printStackTrace();
        return;
      }

      // set security
      setCreateResourceSecurity( (Stub) gaja3dFactory );

      // create instance
      final CreateResource request = new CreateResource();
      final CreateResourceResponse createResponse = gaja3dFactory.createResource( request );
      instanceEPR = createResponse.getEndpointReference();
    }

    // save epr if desired
    if( SAVE_EPR )
      writeEPRtoFile( instanceEPR );

    // retrieve instance
    final Gaja3DServiceAddressingLocator instanceLocator = new Gaja3DServiceAddressingLocator();
    Gaja3DPortType gaja3d = instanceLocator.getGaja3dPortTypePort( instanceEPR );

    logger.debug( "Retrieved service instance." );
    long now = System.currentTimeMillis();
    logger.info( "Time used: " + (now - last) );
    last = now;

    try
    {
      gaja3d = instanceLocator.getGaja3dPortTypePort( instanceEPR );

      // set security and timeout
      final Stub stub = (Stub) gaja3d;
      stub.setTimeout( 60 * 1000 * 60 );
      stub._setProperty( Constants.GSI_SEC_CONV, Constants.ENCRYPTION );
      stub._setProperty( Constants.AUTHORIZATION, SelfAuthorization.getInstance() );
      stub._setProperty( GSIConstants.GSI_MODE, GSIConstants.GSI_MODE_FULL_DELEG );

      // call GetCapabilities
      final WPSCapabilitiesType capabilities = callGetCapabilities( gaja3d );
// call DescribeProcess for all offered processes
      callDescribeProcess( gaja3d, capabilities );

      // set Boundary RP
      final SetResourceProperties_Element setResourcePropertiesRequest = buildSetBoundaryResourceProperty();
      gaja3d.setResourceProperties( setResourcePropertiesRequest );
      logger.debug( "Finished setting boundaries." );
      now = System.currentTimeMillis();
      logger.info( "Time used: " + (now - last) );
      last = now;

      // call Execute_createGrid
      final CreateGridParametersType execute_createGrid = buildExecuteCreateGrid();
      gaja3d.execute_createGrid( execute_createGrid );
      logger.debug( "Finished creating grid." );
      now = System.currentTimeMillis();
      logger.info( "Time used: " + (now - last) );
      last = now;

      // call Execute_detectBreaklines
      final DetectBreaklinesParametersType detectBreaklinesParameters = buildExecuteDetectBreaklines();
      gaja3d.execute_detectBreaklines( detectBreaklinesParameters );
      logger.debug( "Finished detecting breaklines." );
      now = System.currentTimeMillis();
      logger.info( "Time used: " + (now - last) );
      last = now;

      // call Execute_createTin
      final CreateTinParametersType createTinParameters = buildExecuteCreateTin();
      gaja3d.execute_createTin( createTinParameters );
      logger.debug( "Finished creating tin." );
      now = System.currentTimeMillis();
      logger.info( "Time used: " + (now - last) );
      last = now;

      printResourceProperties( gaja3d );

      now = System.currentTimeMillis();
      logger.info( "Time used: " + (now - last) );
      last = now;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      logger.error( e );
    }
    finally
    {
      if( !SAVE_EPR )
      {
        // call Destroy
        final Destroy destroyRequest = new Destroy();
        gaja3d.destroy( destroyRequest );
        logger.debug( "Destroyed instance." );
      }
    }

  }

  private static CreateTinParametersType buildExecuteCreateTin( )
  {
    final CreateTinParametersType execute_createTin = new CreateTinParametersType();
    /*
     * remove comment when skipping breakline detection
     */
    // try {
    //
    // final DemGrid[] demGrids = getDemGrids();
    // execute_createTin.setDemGrid(demGrids);
    //
    // final Breaklines[] getBreaklines = getBreaklines();
    // execute_createTin.setBreaklines(getBreaklines);
    //
    // } catch (final MalformedURIException e) {
    // e.printStackTrace();
    // }
    final MinAngle minAngle = new MinAngle();
    minAngle.setAngle( 22 );
    execute_createTin.setMinAngle( minAngle );

    final MaxArea maxArea = new MaxArea();
    maxArea.setArea( 10000 );
    execute_createTin.setMaxArea( maxArea );

    return execute_createTin;
  }

  private static Breaklines[] getBreaklines( ) throws MalformedURIException
  {
    final Breaklines breaklines1 = new Breaklines();
    final URI breaklinesHref1 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/Breaklines_0001.zip" );
    breaklines1.setRef( breaklinesHref1 );

    final Breaklines breaklines2 = new Breaklines();
    final URI breaklinesHref2 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/Breaklines_0002.zip" );
    breaklines2.setRef( breaklinesHref2 );

    final Breaklines breaklines3 = new Breaklines();
    final URI breaklinesHref3 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/Breaklines_0003.zip" );
    breaklines3.setRef( breaklinesHref3 );

    final Breaklines breaklines4 = new Breaklines();
    final URI breaklinesHref4 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/Breaklines_0004.zip" );
    breaklines4.setRef( breaklinesHref4 );

    final Breaklines[] getBreaklines = new Breaklines[] { breaklines1, breaklines2, breaklines3, breaklines4 };
    return getBreaklines;
  }

  private static DetectBreaklinesParametersType buildExecuteDetectBreaklines( )
  {
    final DetectBreaklinesParametersType execute_detectBreaklines = new DetectBreaklinesParametersType();
    /*
     * remove comment for skipping grid creation
     */
    // try {
    //
    // final DemGrid[] demGrids = getDemGrids();
    // execute_detectBreaklines.setDemGrid(demGrids);
    //
    // } catch (final MalformedURIException e) {
    // e.printStackTrace();
    // }
    final SmoothFilter smoothFilter = new SmoothFilter();
    final SmoothFilterMethod method = SmoothFilterMethod.gauss;
    smoothFilter.setMethod( method );
    smoothFilter.setSmooth( 20 );

    execute_detectBreaklines.setSmoothFilter( smoothFilter );

    return execute_detectBreaklines;
  }

  private static DemGrid[] getDemGrids( ) throws MalformedURIException
  {
    final DemGrid demGrid1 = new DemGrid();
    final URI demGridHref1 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/DemGrid_0001.asc" );
    demGrid1.setRef( demGridHref1 );

    final DemGrid demGrid2 = new DemGrid();
    final URI demGridHref2 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/DemGrid_0002.asc" );
    demGrid2.setRef( demGridHref2 );

    final DemGrid demGrid3 = new DemGrid();
    final URI demGridHref3 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/DemGrid_0003.asc" );
    demGrid3.setRef( demGridHref3 );

    final DemGrid demGrid4 = new DemGrid();
    final URI demGridHref4 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/DemGrid_0004.asc" );
    demGrid4.setRef( demGridHref4 );

    final DemGrid[] demGrids = new DemGrid[] { demGrid1, demGrid2, demGrid3, demGrid4 };
    return demGrids;
  }

  private static CreateGridParametersType buildExecuteCreateGrid( )
  {
    final CreateGridParametersType execute_createGrid = new CreateGridParametersType();

    final DemPoints demPoints = new DemPoints();

    try
    {

      final URI demPointsHref = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/allpoints.zip" );
      demPoints.setRef( demPointsHref );

    }
    catch( final MalformedURIException e )
    {
      e.printStackTrace();
    }
    execute_createGrid.setDemPoints( demPoints );

    final GridX gridX = new GridX();
    gridX.setDx( 100 );
    execute_createGrid.setGridX( gridX );

    final GridY gridY = new GridY();
    gridY.setDy( 100 );
    execute_createGrid.setGridY( gridY );

    return execute_createGrid;
  }

  private static SetResourceProperties_Element buildSetBoundaryResourceProperty( )
  {
    final SetResourceProperties_Element setResourcePropertiesRequest = new SetResourceProperties_Element();
    final UpdateType update = new UpdateType();

    try
    {

      final Boundary[] boundaries = getBoundaries();
      update.set_any( new MessageElement[] { new MessageElement( Gaja3dQNames.RP_BOUNDARY, boundaries[0] ), new MessageElement( Gaja3dQNames.RP_BOUNDARY, boundaries[1] ),
          new MessageElement( Gaja3dQNames.RP_BOUNDARY, boundaries[2] ), new MessageElement( Gaja3dQNames.RP_BOUNDARY, boundaries[3] ) } );

      setResourcePropertiesRequest.setUpdate( update );
    }
    catch( final MalformedURIException e )
    {
      e.printStackTrace();
    }
    return setResourcePropertiesRequest;
  }

  private static Boundary[] getBoundaries( ) throws MalformedURIException
  {
    final Boundary boundary1 = new Boundary();
    final URI boundaryHref1 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/boundary1.zip" );
    boundary1.setRef( boundaryHref1 );

    final Boundary boundary2 = new Boundary();
    final URI boundaryHref2 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/boundary2.zip" );
    boundary2.setRef( boundaryHref2 );

    final Boundary boundary3 = new Boundary();
    final URI boundaryHref3 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/boundary3.zip" );
    boundary3.setRef( boundaryHref3 );

    final Boundary boundary4 = new Boundary();
    final URI boundaryHref4 = new URI( "gridftp://gramd1.gridlab.uni-hannover.de/opt/d-grid-users/gdigrid/gaja3d/boundary4.zip" );
    boundary4.setRef( boundaryHref4 );

    final Boundary[] boundaries = new Boundary[] { boundary1, boundary2, boundary3, boundary4 };
    return boundaries;
  }

  private static void callDescribeProcess( final Gaja3DPortType gaja3d, final WPSCapabilitiesType capabilities ) throws RemoteException
  {
    final DescribeProcess describeProcess = buildDescribeProcess( capabilities );
    final ProcessDescriptions processDescriptions = gaja3d.describeProcess( describeProcess );
    try
    {
      final String pds = ObjectSerializer.toString( processDescriptions, new javax.xml.namespace.QName( "http://www.opengeospatial.net/wps", "ProcessDescriptions" ) );
      logger.debug( "Got process descriptions:" );
      // don't know how to pretty print the ProcessDescriptions using
      // GT4 ObjectSerializer, so we use KalypsoWPS JAXB
      final Object fromKalypsoWPS = MarshallUtilities.unmarshall( pds );
      logger.debug( MarshallUtilities.marshall( fromKalypsoWPS, WPS_VERSION.V040 ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private static WPSCapabilitiesType callGetCapabilities( final Gaja3DPortType gaja3d ) throws RemoteException
  {
    final GetCapabilitiesType getCapabilities = buildGetCapabilities();
    final WPSCapabilitiesType capabilities = gaja3d.getCapabilities( getCapabilities );
    logger.debug( "Got process capabilities." );
    return capabilities;
  }

  private static void setCreateResourceSecurity( final Stub stub )
  {
    stub._setProperty( Constants.GSI_SEC_CONV, Constants.ENCRYPTION );
    stub._setProperty( Constants.AUTHORIZATION, SelfAuthorization.getInstance() );
// stub._setProperty( Constants.AUTHORIZATION,
    // org.globus.wsrf.impl.security.authorization.HostAuthorization.getInstance() );
    stub._setProperty( GSIConstants.GSI_MODE, GSIConstants.GSI_MODE_FULL_DELEG );
  }

  private static DescribeProcess buildDescribeProcess( WPSCapabilitiesType capabilities )
  {
    final ProcessOfferings processOfferings = capabilities.getProcessOfferings();
    final ProcessBriefType[] processes = processOfferings.getProcess();
    final CodeType[] processIds = new CodeType[processes.length];
    int i = 0;
    for( final ProcessBriefType processBriefType : processes )
    {
      processIds[i++] = processBriefType.getIdentifier();
    }
    final DescribeProcess describeProcess = new DescribeProcess();
    describeProcess.setService( "WPS" );
    describeProcess.setVersion( "0.4" );
    describeProcess.setIdentifier( processIds );
    return describeProcess;
  }

  private static GetCapabilitiesType buildGetCapabilities( )
  {
    final GetCapabilitiesType getCapabilities = new GetCapabilitiesType();
    final AcceptVersionsType acceptVersions = new AcceptVersionsType();
    acceptVersions.setVersion( new String[] { "0.4.0" } );
    getCapabilities.setAcceptVersions( acceptVersions );
    return getCapabilities;
  }

  private static void writeEPRtoFile( final EndpointReferenceType instanceEPR )
  {
    try
    {
      final String endpointString = ObjectSerializer.toString( instanceEPR, Gaja3dQNames.RESOURCE_REFERENCE );
      final FileWriter fileWriter = new FileWriter( EPR_FILENAME );
      final BufferedWriter bfWriter = new BufferedWriter( fileWriter );
      bfWriter.write( endpointString );
      bfWriter.close();
      logger.debug( "Endpoint reference written to file " + EPR_FILENAME );
    }
    catch( final SerializationException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  /*
   * This method prints out Gaja3dService's resource properties by using the GetResourceProperty operation.
   */
  private static void printResourceProperties( final Gaja3DPortType gaja3d ) throws RemoteException
  {
    final GetMultipleResourceProperties_Element getMultipleResourcePropertiesRequest = new GetMultipleResourceProperties_Element();
    getMultipleResourcePropertiesRequest.setResourceProperty( Gaja3dQNames.ALL_RPS );
    final GetMultipleResourcePropertiesResponse response = gaja3d.getMultipleResourceProperties( getMultipleResourcePropertiesRequest );

    try
    {
      final MessageElement[] get_any = response.get_any();
      for( final MessageElement messageElement : get_any )
      {
        final String value = messageElement.getAsString();
        logger.debug( String.format( "Value of RP %s: %s", messageElement.getQName(), value ) );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

}