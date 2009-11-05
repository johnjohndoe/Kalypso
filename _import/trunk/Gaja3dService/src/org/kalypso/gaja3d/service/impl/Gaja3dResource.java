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

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.concurrent.Semaphore;

import javax.xml.namespace.QName;

import org.apache.axis.AxisFault;
import org.apache.axis.components.uuid.UUIDGen;
import org.apache.axis.components.uuid.UUIDGenFactory;
import org.apache.axis.message.addressing.EndpointReference;
import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.globus.wsrf.RemoveCallback;
import org.globus.wsrf.ResourceException;
import org.globus.wsrf.ResourceIdentifier;
import org.globus.wsrf.ResourceLifetime;
import org.globus.wsrf.ResourceProperties;
import org.globus.wsrf.ResourceProperty;
import org.globus.wsrf.ResourcePropertyMetaData;
import org.globus.wsrf.ResourcePropertySet;
import org.globus.wsrf.TopicList;
import org.globus.wsrf.TopicListAccessor;
import org.globus.wsrf.config.ConfigException;
import org.globus.wsrf.impl.PersistentReflectionResource;
import org.globus.wsrf.impl.ResourcePropertyTopic;
import org.globus.wsrf.impl.SimpleTopicList;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityConfig;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityDescriptor;
import org.globus.wsrf.security.SecureResource;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.gaja3d.service.internal.strategy.CreateGridStrategy;
import org.kalypso.gaja3d.service.internal.strategy.CreateTinStrategy;
import org.kalypso.gaja3d.service.internal.strategy.DetectBreaklinesStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSCreateGridStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSCreateTinStrategy;
import org.kalypso.gaja3d.service.internal.strategy.WPSDetectBreaklinesStrategy;
import org.kalypso.gaja3d.service.stubs.Boundary;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DemPoints;
import org.kalypso.gaja3d.service.stubs.DistanceTolerance;
import org.kalypso.gaja3d.service.stubs.EdgeFilter;
import org.kalypso.gaja3d.service.stubs.EdgeFilterMethod;
import org.kalypso.gaja3d.service.stubs.FeatureDetector;
import org.kalypso.gaja3d.service.stubs.FeatureDetectorMethod;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourceProperties;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.ModelTin;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.gaja3d.service.stubs.SmoothFilterMethod;
import org.osgi.framework.Bundle;

/**
 * A Gaja3d resource encapsulates the three service operations as well as the WPS inputs and outputs for them. It also
 * dynamically insert inputs where required by an operation. The operations are performed by internally calling a WPS
 * (using WPSRequest.SERVICE_LOCAL as endpoint). Gaja3dResource is a persistent resource that saves its resource
 * properties bean to disk everytime a property is set.
 * 
 * @author kurzbach
 */
public class Gaja3dResource extends PersistentReflectionResource implements SecureResource, ResourceIdentifier, ResourceProperties, ResourceLifetime, TopicListAccessor, RemoveCallback
{

  /* UUID generator to generate unique resource key */
  private static final UUIDGen UUID_GEN = UUIDGenFactory.getUUIDGen();

  /* My logger */
  private static final Log LOGGER = LogFactory.getLog( Gaja3dResource.class.getName() );

  /* Notification */
  private TopicList m_topicList;

  /* Resource properties */
  private ResourcePropertySet m_resourceProperties;

  /* Security config (from file) */
  private ResourceSecurityConfig m_securityConfig;

  /* Lifetime */
  // private Calendar m_terminationTime;
  /* Strategy for grid creation */
  private final CreateGridStrategy m_createGridStrategy;

  /* Strategy for breakline detection */
  private final DetectBreaklinesStrategy m_detectBreaklinesStrategy;

  /* Strategy for tin creation */
  private final CreateTinStrategy m_createTinStrategy;

  /* Synchronize access to this resource */
  private final Semaphore m_lock = new Semaphore( 1 );

  /* If true the resource has been removed. */
  private boolean m_destroyed = false;

  /**
   * Creates a new Gaja3d resource
   */
  public Gaja3dResource( )
  {
    m_createGridStrategy = new WPSCreateGridStrategy();
    m_detectBreaklinesStrategy = new WPSDetectBreaklinesStrategy();
    m_createTinStrategy = new WPSCreateTinStrategy();
  }

  /* Needed by PersistentReflectionResource */
  @Override
  @SuppressWarnings("unchecked")
  protected Class getResourceBeanClass( )
  {
    return Gaja3DResourceProperties.class;
  }

  /** Initializes RPs and returns a unique identifier for this resource */
  public Object initialize( ) throws ConfigException
  {
    final Gaja3DResourceProperties resourceBean = new Gaja3DResourceProperties();
    final String key = UUID_GEN.nextUUID();

    try
    {
      super.initialize( resourceBean, Gaja3dQNames.RESOURCE_PROPERTIES, key );
    }
    catch( final ResourceException e )
    {
      throw new ConfigException( e );
    }

    m_resourceProperties = super.getResourcePropertySet();

    // initialize security descriptor
    try
    {
      final Bundle gaja3dBundle = Platform.getBundle( "org.kalypso.gaja3d.service" );
      final Path pathToSecDesc = new Path( "service/security-config-resource.xml" );
      final URL secDescBundleURL = FileLocator.find( gaja3dBundle, pathToSecDesc, null );
      final URL secDescFileURL = FileLocator.toFileURL( secDescBundleURL );
      m_securityConfig = new ResourceSecurityConfig( secDescFileURL.getFile() );
      m_securityConfig.init();
    }
    catch( final Exception e )
    {
      throw new ConfigException( "Could not load resource security descriptor.", e );
    }
    return key;
  }

  /*
   * (non-Javadoc)
   * @see org.globus.wsrf.impl.ReflectionResource#createNewResourceProperty(org .globus.wsrf.ResourcePropertyMetaData,
   * java.lang.Object)
   */
  @Override
  protected ResourceProperty createNewResourceProperty( final ResourcePropertyMetaData metaData, final Object resourceBean ) throws Exception
  {
    final ResourceProperty property = super.createNewResourceProperty( metaData, resourceBean );

    // lazily create topic list
    if( m_topicList == null )
      m_topicList = new SimpleTopicList( this );

    final ResourcePropertyTopic topic = new ResourcePropertyTopic( property );
    topic.setSendOldValue( true );
    m_topicList.addTopic( topic );
    return topic;
  }

  /* accessors for resource properties */
  public Boundary[] getBoundaries( )
  {
    return Arrays.castArray( (Object[]) getRPinternal( Gaja3dQNames.RP_BOUNDARY, false ), new Boundary[0] );
  }

  public void setBoundaries( final Boundary... boundary )
  {
    setDemPoints( null );
    final QName qName = Gaja3dQNames.RP_BOUNDARY;
    setRPinternal( qName, false, (Object[]) boundary );
  }

  public DemPoints getDemPoints( )
  {
    return (DemPoints) getRPinternal( Gaja3dQNames.RP_DEM_POINTS, true );
  }

  public void setDemPoints( final DemPoints demPoints )
  {
    setDemGrid( new DemGrid[0] );
    final QName qName = Gaja3dQNames.RP_DEM_POINTS;
    setRPinternal( qName, true, demPoints );
  }

  public DemGrid[] getDemGrid( )
  {
    return Arrays.castArray( (Object[]) getRPinternal( Gaja3dQNames.RP_DEM_GRID, false ), new DemGrid[0] );
  }

  public void setDemGrid( final DemGrid... demGrid )
  {
    setBreaklines( new Breaklines[0] );
    final QName qName = Gaja3dQNames.RP_DEM_GRID;
    setRPinternal( qName, false, (Object[]) demGrid );
  }

  public Breaklines[] getBreaklines( )
  {
    return Arrays.castArray( (Object[]) getRPinternal( Gaja3dQNames.RP_BREAKLINES, false ), new Breaklines[0] );
  }

  public void setBreaklines( final Breaklines... breaklines )
  {
    setModelTin( null );
    final QName qName = Gaja3dQNames.RP_BREAKLINES;
    setRPinternal( qName, false, (Object[]) breaklines );
  }

  public ModelTin getModelTin( )
  {
    return (ModelTin) getRPinternal( Gaja3dQNames.RP_MODEL_TIN, true );
  }

  public void setModelTin( final ModelTin modelTin )
  {
    final QName qName = Gaja3dQNames.RP_MODEL_TIN;
    setRPinternal( qName, true, modelTin );
  }

  public GridX getGridX( )
  {
    return (GridX) getRPinternal( Gaja3dQNames.RP_GRID_X, true );
  }

  public void setGridX( final GridX gridX )
  {
    final QName qName = Gaja3dQNames.RP_GRID_X;
    setRPinternal( qName, true, gridX );
  }

  public GridY getGridY( )
  {
    return (GridY) getRPinternal( Gaja3dQNames.RP_GRID_Y, true );
  }

  public void setGridY( final GridY gridY )
  {
    final QName qName = Gaja3dQNames.RP_GRID_Y;
    setRPinternal( qName, true, gridY );
  }

  public MinAngle getMinAngle( )
  {
    return (MinAngle) getRPinternal( Gaja3dQNames.RP_MIN_ANGLE, true );
  }

  public void setMinAngle( final MinAngle minAngle )
  {
    final QName qName = Gaja3dQNames.RP_MIN_ANGLE;
    setRPinternal( qName, true, minAngle );
  }

  public MaxArea getMaxArea( )
  {
    return (MaxArea) getRPinternal( Gaja3dQNames.RP_MAX_AREA, true );
  }

  public void setMaxArea( final MaxArea maxArea )
  {
    final QName qName = Gaja3dQNames.RP_MAX_AREA;
    setRPinternal( qName, true, maxArea );
  }

  public EdgeFilter getEdgeFilter( )
  {
    return (EdgeFilter) getRPinternal( Gaja3dQNames.RP_EDGE_FILTER, true );
  }

  public void setEdgeFilter( final EdgeFilter edgeFilter )
  {
    final QName qName = Gaja3dQNames.RP_EDGE_FILTER;
    setRPinternal( qName, true, edgeFilter );
  }

  public SmoothFilter getSmoothFilter( )
  {
    return (SmoothFilter) getRPinternal( Gaja3dQNames.RP_SMOOTH_FILTER, true );
  }

  public void setSmoothFilter( final SmoothFilter smoothFilter )
  {
    final QName qName = Gaja3dQNames.RP_SMOOTH_FILTER;
    setRPinternal( qName, true, smoothFilter );
  }

  public FeatureDetector getFeatureDetector( )
  {
    return (FeatureDetector) getRPinternal( Gaja3dQNames.RP_FEATURE_DETECTOR, true );
  }

  public void setFeatureDetector( final FeatureDetector featureDetector )
  {
    final QName qName = Gaja3dQNames.RP_FEATURE_DETECTOR;
    setRPinternal( qName, true, featureDetector );
  }

  public DistanceTolerance getDistanceTolerance( )
  {
    return (DistanceTolerance) getRPinternal( Gaja3dQNames.RP_DISTANCE_TOLERANCE, true );
  }

  public void setDistanceTolerance( final DistanceTolerance distanceTolerance )
  {
    final QName qName = Gaja3dQNames.RP_DISTANCE_TOLERANCE;
    setRPinternal( qName, true, distanceTolerance );
  }

  public EndpointReference getGramEndpointReference( )
  {
    return (EndpointReference) getRPinternal( Gaja3dQNames.RP_GRAM_ENDPOINT_REFERENCE, true );
  }

  public void setGramEndpointReference( final EndpointReference endpointReference )
  {
    final QName qName = Gaja3dQNames.RP_GRAM_ENDPOINT_REFERENCE;
    setRPinternal( qName, true, endpointReference );
  }

  /* Implementations of 3D Terrain Discretization Service methods */

  public void createGrid( ) throws RemoteException
  {
    try
    {
      m_lock.acquire();
    }
    catch( final InterruptedException e )
    {
      LOGGER.error( "Thread was interrupted before tin creation." );
    }

    // required
    final Boundary[] boundaries = getBoundaries();
    if( boundaries.length == 0 )
    {
      throw new AxisFault( "A boundary is required for grid creation." );
    }

    final DemGrid[] demGrids = new DemGrid[boundaries.length];
    try
    {
      final java.net.URI[] boundaryLocations = new java.net.URI[boundaries.length];
      for( int i = 0; i < boundaries.length; i++ )
      {
        final Boundary boundary = boundaries[i];
        final URI boundaryURI = boundary.get_value();
        final java.net.URI boundaryLocation = new java.net.URI( boundaryURI.toString() );
        boundaryLocations[i] = boundaryLocation;
      }

      // required
      final DemPoints demPoints = getDemPoints();
      if( demPoints == null )
      {
        throw new AxisFault( "A point cloud is required for grid creation." );
      }
      final URI demPointsURI = demPoints.get_value();
      final java.net.URI demPointsLocation = new java.net.URI( demPointsURI.toString() );

      // required
      final GridX gridX = getGridX();
      if( gridX == null )
      {
        throw new AxisFault( "Grid resolution (X) is required for grid creation." );
      }
      final double dx = gridX.getDx();

      // required
      final GridY gridY = getGridY();
      if( gridY == null )
      {
        throw new AxisFault( "Grid resolution (Y) is required for grid creation." );
      }
      final double dy = gridY.getDy();

      // do the actual work
      final java.net.URI[] demGridLocations = m_createGridStrategy.createGrid( boundaryLocations, demPointsLocation, dx, dy );

      for( int i = 0; i < demGridLocations.length; i++ )
      {
        final java.net.URI demGridLocation = demGridLocations[i];
        final DemGrid demGrid = new DemGrid();
        demGrid.set_value( new URI( demGridLocation.toString() ) );
        demGrids[i] = demGrid;
      }
    }
    catch( final MalformedURIException e )
    {
      throw AxisFault.makeFault( e );
    }
    catch( final URISyntaxException e )
    {
      throw AxisFault.makeFault( e );
    }
    finally
    {
      m_lock.release();
    }
    setDemGrid( demGrids );
  }

  public void detectBreaklines( ) throws RemoteException
  {
    try
    {
      m_lock.acquire();
    }
    catch( final InterruptedException e )
    {
      LOGGER.error( "Thread was interrupted before tin creation." );
    }

    // required
    final Boundary[] boundaries = getBoundaries();
    if( boundaries.length == 0 )
    {
      throw new AxisFault( "A boundary is required for breakline detection." );
    }

    // required
    final DemGrid[] demGrids = getDemGrid();
    if( demGrids.length != boundaries.length )
    {
      throw new AxisFault( "A grid is required for each boundary for breakline detection." );
    }

    final Breaklines[] breaklinesSet = new Breaklines[boundaries.length];
    try
    {
      // optional
      final EdgeFilter edgeFilter = getEdgeFilter();
      if( edgeFilter != null )
      {
        final EdgeFilterMethod method = edgeFilter.getMethod();
        final String edgeMethod = method.getValue();
        m_detectBreaklinesStrategy.setEdgeMethod( edgeMethod );
      }

      // optional
      final SmoothFilter smoothFilter = getSmoothFilter();
      if( smoothFilter != null )
      {
        final SmoothFilterMethod method = smoothFilter.getMethod();
        final String smoothMethod = method.getValue();
        final int smooth = smoothFilter.getSmooth();
        m_detectBreaklinesStrategy.setSmoothMethod( smoothMethod );
        m_detectBreaklinesStrategy.setSmooth( smooth );
      }

      // optional
      final FeatureDetector featureDetector = getFeatureDetector();
      if( featureDetector != null )
      {
        final FeatureDetectorMethod method = featureDetector.getMethod();
        final String featureMethod = method.getValue();
        final double lowThresh = featureDetector.getLowThresh();
        final double highThresh = featureDetector.getHighThresh();
        m_detectBreaklinesStrategy.setFeatureMethod( featureMethod );
        m_detectBreaklinesStrategy.setLowThresh( lowThresh );
        m_detectBreaklinesStrategy.setHighThresh( highThresh );
      }

      // optional
      final DistanceTolerance distanceToleranceParameter = getDistanceTolerance();
      if( distanceToleranceParameter != null )
      {
        final double distanceTolerance = distanceToleranceParameter.getTolerance();
        m_detectBreaklinesStrategy.setDistanceTolerance( distanceTolerance );
      }

      final java.net.URI[] boundaryLocations = new java.net.URI[boundaries.length];
      for( int i = 0; i < boundaries.length; i++ )
      {
        final Boundary boundary = boundaries[i];
        final URI boundaryURI = boundary.get_value();
        final java.net.URI boundaryLocation = new java.net.URI( boundaryURI.toString() );
        boundaryLocations[i] = boundaryLocation;
      }

      final java.net.URI[] demGridLocations = new java.net.URI[boundaries.length];
      for( int i = 0; i < boundaries.length; i++ )
      {
        final DemGrid demGrid = demGrids[i];
        final URI demGridURI = demGrid.get_value();
        final java.net.URI demGridLocation = new java.net.URI( demGridURI.toString() );
        demGridLocations[i] = demGridLocation;
      }

      // do the actual work
      final java.net.URI[] breaklinesLocations = m_detectBreaklinesStrategy.detectBreaklines( boundaryLocations, demGridLocations );

      for( int i = 0; i < breaklinesLocations.length; i++ )
      {
        final java.net.URI breaklinesLocation = breaklinesLocations[i];
        final Breaklines breaklines = new Breaklines();
        breaklines.set_value( new URI( breaklinesLocation.toString() ) );
        breaklinesSet[i] = breaklines;
      }
    }
    catch( final MalformedURIException e )
    {
      throw AxisFault.makeFault( e );
    }
    catch( final IOException e )
    {
      throw AxisFault.makeFault( e );
    }
    catch( final URISyntaxException e )
    {
      throw AxisFault.makeFault( e );
    }
    finally
    {
      m_lock.release();
    }
    setBreaklines( breaklinesSet );
  }

  public void createTin( ) throws RemoteException
  {
    try
    {
      m_lock.acquire();
    }
    catch( final InterruptedException e )
    {
      LOGGER.error( "Thread was interrupted before tin creation." );
    }

    final ModelTin modelTin = new ModelTin();

    // required
    final Boundary[] boundaries = getBoundaries();
    if( boundaries.length == 0 )
    {
      throw new AxisFault( "At least one boundary is required for tin creation." );
    }

    // optional
    final DemGrid[] demGrids = getDemGrid();

    // optional
    final Breaklines[] breakliness = getBreaklines();

    try
    {
      // optional
      final MaxArea maxArea = getMaxArea();
      if( maxArea != null )
      {
        final double area = maxArea.getArea();
        m_createTinStrategy.setMaxArea( area );
      }

      // optional
      final MinAngle minAngle = getMinAngle();
      if( minAngle != null )
      {
        final double angle = minAngle.getAngle();
        m_createTinStrategy.setMinAngle( angle );
      }

      final java.net.URI[] boundaryLocations = new java.net.URI[boundaries.length];
      for( int i = 0; i < boundaries.length; i++ )
      {
        final Boundary boundary = boundaries[i];
        final URI boundaryURI = boundary.get_value();
        final java.net.URI boundaryLocation = new java.net.URI( boundaryURI.toString() );
        boundaryLocations[i] = boundaryLocation;
      }

      if( demGrids.length > 0 )
      {
        final java.net.URI[] demGridLocations = new java.net.URI[demGrids.length];
        for( int i = 0; i < demGrids.length; i++ )
        {
          final DemGrid demGrid = demGrids[i];
          final URI demGridURI = demGrid.get_value();
          final java.net.URI demGridLocation = new java.net.URI( demGridURI.toString() );
          demGridLocations[i] = demGridLocation;
        }
        m_createTinStrategy.setDemGridLocation( demGridLocations );
      }

      if( breakliness.length > 0 )
      {
        final java.net.URI[] breaklinesLocations = new java.net.URI[demGrids.length];
        for( int i = 0; i < breakliness.length; i++ )
        {
          final Breaklines breaklines = breakliness[i];
          final URI breaklinesURI = breaklines.get_value();
          final java.net.URI breaklinesLocation = new java.net.URI( breaklinesURI.toString() );
          breaklinesLocations[i] = breaklinesLocation;
        }
        m_createTinStrategy.setBreaklinesLocation( breaklinesLocations );
      }

      final java.net.URI modelTinLocation = m_createTinStrategy.createTin( boundaryLocations );

      modelTin.set_value( new URI( modelTinLocation.toString() ) );
    }
    catch( final MalformedURIException e )
    {
      throw AxisFault.makeFault( e );
    }
    catch( final URISyntaxException e )
    {
      throw AxisFault.makeFault( e );
    }
    finally
    {
      m_lock.release();
    }
    setModelTin( modelTin );
  }

  /* Required by interface TopicListAccessor */
  public TopicList getTopicList( )
  {
    return m_topicList;
  }

  /* Required by interface SecureResource */
  public ResourceSecurityDescriptor getSecurityDescriptor( )
  {
    return m_securityConfig.getSecurityDescriptor();
  }

  /* Required by interface RemoveCallback */
  @Override
  public void remove( ) throws ResourceException
  {
    try
    {
      m_lock.acquire();
    }
    catch( final InterruptedException e )
    {
      LOGGER.error( "Thread was interrupted before removing resource.", e );
    }

    try
    {
      super.remove();
    }
    finally
    {
      m_lock.release();
      m_destroyed = true;
    }
  }

  /*
   * (non-Javadoc)
   * @see org.globus.wsrf.impl.PersistentReflectionResource#store()
   */
  @Override
  public synchronized void store( ) throws ResourceException
  {
    // check if this resource has been destroyed
    if( !m_destroyed )
    {
      super.store();
    }
    else
    {
      LOGGER.warn( "Tried to store resource that has already been destroyed." );
    }
  }

  /* Private methods */
  private Object getRPinternal( final QName qName, final boolean isSimple )
  {
    final ResourceProperty property = m_resourceProperties.get( qName );
    if( isSimple )
    {
      return property.get( 0 );
    }

    final Object[] list = new Object[property.size()];
    for( int i = 0; i < property.size(); i++ )
    {
      list[i] = property.get( i );
    }
    return list;
  }

  private void setRPinternal( final QName qName, final boolean isSimple, final Object... newValue )
  {
    try
    {
      m_lock.acquire();
    }
    catch( final InterruptedException e )
    {
      LOGGER.error( "Thread was interrupted before setting resource property " + qName, e );
    }

    try
    {
      final ResourceProperty property = m_resourceProperties.get( qName );
      property.clear();
      setDirty( true );
      if( isSimple )
        property.set( 0, newValue[0] );
      else
      {
        for( final Object object : newValue )
        {
          property.add( object );
        }
      }
      store();
    }
    catch( final ResourceException e )
    {
      LOGGER.error( "Could not store resource.", e );
    }
    finally
    {
      m_lock.release();
    }
  }
}