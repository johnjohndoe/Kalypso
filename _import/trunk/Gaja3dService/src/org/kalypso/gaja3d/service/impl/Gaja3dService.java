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
package org.kalypso.gaja3d.service.impl;

import java.io.StringReader;
import java.rmi.RemoteException;

import javax.security.auth.Subject;

import net.opengeospatial.ows.stubs.GetCapabilitiesType;
import net.opengeospatial.wps.stubs.Capabilities;
import net.opengeospatial.wps.stubs.DescribeProcess;
import net.opengeospatial.wps.stubs.Execute;
import net.opengeospatial.wps.stubs.ExecuteResponseType;
import net.opengeospatial.wps.stubs.ProcessDescriptions;

import org.apache.axis.message.MessageElement;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.globus.gsi.jaas.JaasSubject;
import org.globus.wsrf.Resource;
import org.globus.wsrf.ResourceContext;
import org.globus.wsrf.encoding.ObjectDeserializer;
import org.globus.wsrf.security.SecurityManager;
import org.kalypso.gaja3d.service.stubs.Boundary;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.CreateGridParametersType;
import org.kalypso.gaja3d.service.stubs.CreateGridResponseType;
import org.kalypso.gaja3d.service.stubs.CreateTinResponseType;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DemPoints;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesParametersType;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesResponseType;
import org.kalypso.gaja3d.service.stubs.DistanceTolerance;
import org.kalypso.gaja3d.service.stubs.EdgeFilter;
import org.kalypso.gaja3d.service.stubs.FeatureDetector;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.ModelTin;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.service.wps.utils.WPSUtilities;
import org.xml.sax.InputSource;

public class Gaja3dService
{

  private static final Log LOGGER = LogFactory.getLog( Gaja3dService.class.getName() );

  /**
   * Gets a reference to the resource specified in the endpoint reference.
   */
  private Gaja3dResource getResource( ) throws RemoteException
  {
    Object resource = null;
    try
    {
      resource = ResourceContext.getResourceContext().getResource();
    }
    catch( final Exception e )
    {
      throw new RemoteException( "Could not find Gaja3d resource.", e );
    }
    return (Gaja3dResource) resource;
  }

  /**
   * This standard web method is automatically generated to give a full description of this operation. The response will
   * be an embedded WPS XML GetCapabilities Response. Please consult the OGC Web Site for the WPS 1.0.0 specification.
   */
  public Capabilities getCapabilities( final GetCapabilitiesType parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "getCapabilities", resource );

    try
    {
      // extract request as string
      final MessageElement getCapabilities = new MessageElement( WPSQNames_0_4.GET_CAPABILITIES );
      getCapabilities.setObjectValue( parameters );
      final String requestString = getCapabilities.toString();

      // call service
      final String endpoint = System.getProperty( WPSRequest.SYSTEM_PROP_WPS_ENDPOINT );
      final String responseString = WPSUtilities.send( requestString, endpoint );

      System.out.println(responseString);
      
      // create response
      final InputSource is = new InputSource( new StringReader( responseString ) );
      final Capabilities capabilities = (Capabilities) ObjectDeserializer.deserialize( is, Capabilities.class );
      return capabilities;
    }
    catch( final Exception e )
    {
      throw new RemoteException( "Could not get capabilities.", e );
    }
  }

  /**
   * This standard web method is automatically generated to give a full description of all processes provided by this
   * operation. The response will be an embedded WPS XML DescribeProcess Response. Please consult the OGC Web Site for
   * the WPS 0.4.0 specification.
   */
  public ProcessDescriptions describeProcess( final DescribeProcess parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "describeProcess", resource );

    try
    {
      // extract request as string
      final MessageElement describeProcess = new MessageElement( WPSQNames_0_4.DESCRIBE_PROCESS );
      describeProcess.setObjectValue( parameters );
      final String requestString = describeProcess.toString();

      // call service
      final String endpoint = System.getProperty( WPSRequest.SYSTEM_PROP_WPS_ENDPOINT );
      final String responseString = WPSUtilities.send( requestString, endpoint );

      // create response
      final InputSource is = new InputSource( new StringReader( responseString ) );
      final ProcessDescriptions processDescriptions = (ProcessDescriptions) ObjectDeserializer.deserialize( is, ProcessDescriptions.class );
      return processDescriptions;
    }
    catch( final Exception e )
    {
      throw new RemoteException( "Could not get process descriptions.", e );
    }
  }

  /**
   * This is the generic Execute operation. As WPS does not currently support security, this call will fail if the
   * current security context is needed for subsequent actions (e.g. job submission) by the called WPS
   */
  public ExecuteResponseType execute( final Execute parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "execute", resource );

    try
    {
      // extract request as string
      final MessageElement execute = new MessageElement( WPSQNames_0_4.EXECUTE );
      execute.setObjectValue( parameters );
      final String requestString = execute.toString();

      // call service
      final String endpoint = System.getProperty( WPSRequest.SYSTEM_PROP_WPS_ENDPOINT );
      final String responseString = WPSUtilities.send( requestString, endpoint );

      // create response
      final InputSource is = new InputSource( new StringReader( responseString ) );
      final ExecuteResponseType executeResponse = (ExecuteResponseType) ObjectDeserializer.deserialize( is, ExecuteResponseType.class );
      return executeResponse;
    }
    catch( final Exception e )
    {
      throw new RemoteException( "Problem during execution", e );
    }
  }

  /**
   * @param parameters
   * @return
   * @throws RemoteException
   */
  public CreateGridResponseType execute_createGrid( final CreateGridParametersType parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "execute_createGrid", resource );

    // required
    final Boundary[] boundary = parameters.getBoundary();
    if( boundary != null )
      resource.setBoundaries( boundary );

    // required
    final DemPoints demPoints = parameters.getDemPoints();
    if( demPoints != null )
      resource.setDemPoints( demPoints );

    // required
    final GridX gridX = parameters.getGridX();
    if( gridX != null )
      resource.setGridX( gridX );

    // required
    final GridY gridY = parameters.getGridY();
    if( gridY != null )
      resource.setGridY( gridY );

    resource.createGrid();

    final CreateGridResponseType createGridResponseType = new CreateGridResponseType();
    final DemGrid[] demGrid = resource.getDemGrid();
    createGridResponseType.setDemGrid( demGrid );
    return createGridResponseType;
  }

  /**
   * @param parameters
   * @return
   * @throws java.rmi.RemoteException
   */
  public DetectBreaklinesResponseType execute_detectBreaklines( final DetectBreaklinesParametersType parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "execute_detectBreaklines", resource );

    // required
    final Boundary[] boundary = parameters.getBoundary();
    if( boundary != null )
      resource.setBoundaries( boundary );

    // required
    final DemGrid[] demGrid = parameters.getDemGrid();
    if( demGrid != null )
      resource.setDemGrid( demGrid );

    // optional
    final EdgeFilter edgeFilter = parameters.getEdgeFilter();
    if( edgeFilter != null )
      resource.setEdgeFilter( edgeFilter );

    // optional
    final SmoothFilter smoothFilter = parameters.getSmoothFilter();
    if( smoothFilter != null )
      resource.setSmoothFilter( smoothFilter );

    // optional
    final FeatureDetector featureDetector = parameters.getFeatureDetector();
    if( featureDetector != null )
      resource.setFeatureDetector( featureDetector );

    // optional
    final DistanceTolerance distanceTolerance = parameters.getDistanceTolerance();
    if( distanceTolerance != null )
      resource.setDistanceTolerance( distanceTolerance );

    resource.detectBreaklines();

    final DetectBreaklinesResponseType detectBreaklinesResponseType = new DetectBreaklinesResponseType();
    final Breaklines[] breaklines = resource.getBreaklines();
    detectBreaklinesResponseType.setBreaklines( breaklines );
    return detectBreaklinesResponseType;
  }

  /**
   * @param parameters
   * @return
   * @throws java.rmi.RemoteException
   */
  public org.kalypso.gaja3d.service.stubs.CreateTinResponseType execute_createTin( final org.kalypso.gaja3d.service.stubs.CreateTinParametersType parameters ) throws RemoteException
  {
    final Gaja3dResource resource = getResource();
    logSecurityInfo( "execute_createTin", resource );

    // required
    final Boundary[] boundary = parameters.getBoundary();
    if( boundary != null )
      resource.setBoundaries( boundary );

    // optional
    final DemGrid[] demGrid = parameters.getDemGrid();
    if( demGrid != null )
      resource.setDemGrid( demGrid );

    // required
    final Breaklines[] breaklines = parameters.getBreaklines();
    if( breaklines != null )
      resource.setBreaklines( breaklines );

    // optional
    final MaxArea maxArea = parameters.getMaxArea();
    if( maxArea != null )
      resource.setMaxArea( maxArea );

    // optional
    final MinAngle minAngle = parameters.getMinAngle();
    if( minAngle != null )
      resource.setMinAngle( minAngle );

    resource.createTin();

    final CreateTinResponseType createTinResponseType = new CreateTinResponseType();
    final ModelTin modelTin = resource.getModelTin();
    createTinResponseType.setModelTin( modelTin );
    return createTinResponseType;
  }

  private void logSecurityInfo( final String methodName, final Resource resource )
  {
    LOGGER.debug( "SECURITY INFO FOR METHOD '" + methodName + "'" );

    // Print out the caller
    final String identity = SecurityManager.getManager().getCaller();
    LOGGER.debug( "The caller is:" + identity );

    // Print out the invocation subject
    Subject subject = JaasSubject.getCurrentSubject();
    LOGGER.debug( "INVOCATION SUBJECT" );
    LOGGER.debug( subject == null ? "NULL" : subject.toString() );

    try
    {
      // Print out service subject
      LOGGER.debug( "SERVICE SUBJECT" );
      subject = SecurityManager.getManager().getServiceSubject();
      LOGGER.debug( subject == null ? "NULL" : subject.toString() );
    }
    catch( final Exception e )
    {
      LOGGER.debug( "Unable to obtain service subject" );
    }

    try
    {
      // Print out system subject
      LOGGER.debug( "SYSTEM SUBJECT" );
      subject = SecurityManager.getManager().getSystemSubject();
      LOGGER.debug( subject == null ? "NULL" : subject.toString() );
    }
    catch( final Exception e )
    {
      LOGGER.debug( "Unable to obtain system subject" );
    }

    LOGGER.debug( "RESOURCE SUBJECT" );
    if( resource == null )
    {
      LOGGER.debug( "No resource" );
    }
    else
    {
      try
      {
        subject = SecurityManager.getManager().getSubject( resource );
        LOGGER.debug( subject == null ? "NULL" : subject.toString() );
      }
      catch( final Exception e )
      {
        LOGGER.debug( "Unable to obtain resource subject" );
      }
    }
  }
}