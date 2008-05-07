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
package org.kalypso.ogc.gml.wms.deegree;

import java.awt.Point;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.deegree.datatypes.QualifiedName;
import org.deegree.framework.xml.XMLFragment;
import org.deegree.ogcwebservices.wms.capabilities.Layer;
import org.deegree.ogcwebservices.wms.capabilities.LayerBoundingBox;
import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.deegree.ogcwebservices.wms.operation.GetMap;
import org.deegree.owscommon_new.DCP;
import org.deegree.owscommon_new.HTTP;
import org.deegree.owscommon_new.Operation;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.net.Proxy;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.wms.deegree.document.KalypsoWMSCapabilitiesDocument;
import org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader;
import org.kalypso.transformation.GeoTransformer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class provides functions for dealing with the WMS client from degree.
 * 
 * @author Holger Albert
 */
public class DeegreeWMSUtilities
{
  /**
   * The constructor.
   */
  private DeegreeWMSUtilities( )
  {
  }

  /**
   * This function should load the capabilites for the given service.
   * 
   * @param loader
   *            This loader will load the capabilities.
   * @param monitor
   *            A progress monitor.
   */
  public static WMSCapabilities loadCapabilities( ICapabilitiesLoader loader, IProgressMonitor monitor ) throws CoreException
  {
    /* The input stream. */
    InputStream inputStream = null;

    try
    {
      /* FIXME: HACK: Initialize once, to init the java-proxy settings, in case they are not set. */
      Proxy.getProxyService( KalypsoGisPlugin.getDefault() );

      /* Get the input stream of the capabilities. */
      inputStream = loader.getCapabilitiesStream( monitor );

      /* This is a capabilities document from deegree, which was overwritten by Kalypso. */
      KalypsoWMSCapabilitiesDocument doc = new KalypsoWMSCapabilitiesDocument();

      /* Load the capabilities xml. */
      doc.load( new InputStreamReader( inputStream ), XMLFragment.DEFAULT_URL );

      /* Create the capabilities. */
      WMSCapabilities capabilities = (WMSCapabilities) doc.parseCapabilities();
      if( capabilities == null )
        throw new Exception( Messages.getString("org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities.0") ); //$NON-NLS-1$

      return capabilities;
    }
    catch( Exception ex )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( ex ) );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * This function creates the get feature info request.
   * 
   * @param capabilities
   *            The wms capabilities.
   * @param layers
   *            The layers.
   * @param pointOfInterest
   *            The point of interest.
   * @param format
   *            The format.
   * @return The get feature info request.
   */
  public static HashMap<String, String> createGetFeatureinfoRequest( WMSCapabilities capabilities, String layers, Point pointOfInterest, String format ) throws CoreException
  {
    // TODO: does not work at the moment
    HashMap<String, String> parameterMap = prepareRequestParameters( capabilities, "GetFeatureInfo" ); //$NON-NLS-1$
    parameterMap.put( "QUERY_LAYERS", layers ); //$NON-NLS-1$

    if( format != null && format.length() > 0 )
      parameterMap.put( "INFO_FORMAT", format ); //$NON-NLS-1$

    parameterMap.put( "X", Integer.toString( pointOfInterest.x ) ); //$NON-NLS-1$
    parameterMap.put( "Y", Integer.toString( pointOfInterest.y ) ); //$NON-NLS-1$

    parameterMap.put( "LAYERS", "dummy" ); //$NON-NLS-1$ //$NON-NLS-2$
    parameterMap.put( "FORMAT", "image/gif" ); //$NON-NLS-1$ //$NON-NLS-2$
    parameterMap.put( "WIDTH", "100" ); //$NON-NLS-1$ //$NON-NLS-2$
    parameterMap.put( "HEIGHT", "100" ); //$NON-NLS-1$ //$NON-NLS-2$
    parameterMap.put( "SRS", "100" ); //$NON-NLS-1$ //$NON-NLS-2$
    parameterMap.put( "BBOX", "100" ); //$NON-NLS-1$ //$NON-NLS-2$

    return parameterMap;
  }

  /**
   * This function creates the get map request.
   * 
   * @param capabilities
   *            The wms capabilities.
   * @param negotiatedSRS
   *            The negotiated srs.
   * @param themeName
   *            The theme name.
   * @param layers
   *            The layers.
   * @param styles
   *            The styles.
   * @param width
   *            The requested width.
   * @param height
   *            The requested height.
   * @param requestedEnvLocalSRS
   *            The requested envelope in the local coordinate system.
   * @param localSRS
   *            The local coordinate system.
   * @return The get map request.
   */
  public static GetMap createGetMapRequest( WMSCapabilities capabilities, String negotiatedSRS, String themeName, String layers, String styles, int width, int height, GM_Envelope requestedEnvLocalSRS, String localSRS ) throws CoreException
  {
    try
    {
      HashMap<String, String> wmsParameter = prepareRequestParameters( capabilities, "GetMap" ); //$NON-NLS-1$

      wmsParameter.put( "LAYERS", layers ); //$NON-NLS-1$
      if( styles != null )
        wmsParameter.put( "STYLES", styles ); //$NON-NLS-1$

      // some WMS-themes use style name="" and when deegree makes "STYLES=default" out of this, this does not work
      // I think style name="" is also not valid (can we be flexible ?)
      // ask me ( v.doemming@tuhh.de )
      wmsParameter.put( "FORMAT", "image/png" ); //$NON-NLS-1$ //$NON-NLS-2$
      wmsParameter.put( "TRANSPARENT", "TRUE" ); //$NON-NLS-1$ //$NON-NLS-2$
      wmsParameter.put( "WIDTH", "" + width ); //$NON-NLS-1$ //$NON-NLS-2$
      wmsParameter.put( "HEIGHT", "" + height ); //$NON-NLS-1$ //$NON-NLS-2$
      wmsParameter.put( "SRS", negotiatedSRS ); //$NON-NLS-1$

      GeoTransformer gt = new GeoTransformer( negotiatedSRS );
      GM_Envelope targetEnvRemoteSRS = gt.transformEnvelope( requestedEnvLocalSRS, localSRS );

      if( targetEnvRemoteSRS.getMax().getX() - targetEnvRemoteSRS.getMin().getX() <= 0 )
        throw new Exception( "invalid bbox" ); //$NON-NLS-1$

      if( targetEnvRemoteSRS.getMax().getY() - targetEnvRemoteSRS.getMin().getY() <= 0 )
        throw new Exception( "invalid bbox" ); //$NON-NLS-1$

      String targetEnvRemoteSRSstring = DeegreeWMSUtilities.env2bboxString( targetEnvRemoteSRS );
      wmsParameter.put( "BBOX", targetEnvRemoteSRSstring ); //$NON-NLS-1$

      /* Add the ID parameter to them. */
      wmsParameter.put( "ID", "KalypsoWMSRequest" + themeName + new Date().getTime() ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Create the GetMap request. */
      GetMap request = GetMap.create( wmsParameter );

      return request;
    }
    catch( Exception ex )
    {
      /* Create the error status. */
      IStatus status = StatusUtilities.statusFromThrowable( ex, Messages.getString("org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities.35") ); //$NON-NLS-1$

      throw new CoreException( status );
    }
  }

  /**
   * This function prepares the request parameter.
   * 
   * @param capabilities
   *            The wms capabilities.
   * @param name
   *            The name of the operation.
   * @return The request parameter.
   */
  private static HashMap<String, String> prepareRequestParameters( WMSCapabilities capabilities, String operationName ) throws CoreException
  {
    final HashMap<String, String> wmsParameter = new HashMap<String, String>();

    /* HACK: in order to keep any existing query parts, add existing query parts from base url. */
    Operation operation = checkOperation( capabilities, operationName );
    List<DCP> types = operation.getDCP();

    for( DCP type : types )
    {
      if( type instanceof HTTP )
      {
        final HTTP httpProtocol = (HTTP) type;
        List<URL> getOnlineResources = httpProtocol.getGetOnlineResources();
        for( URL url : getOnlineResources )
        {
          if( url != null )
          {
            String query = url.getQuery();
            if( query != null && query.length() > 0 )
            {
              /* The base URL may already contain a query part, we do not want to delete it Quotation from WMS-Spec: */
              /* "An OGC Web Service shall be prepared to encounter parameters that are not part of this specification." */
              String[] requestParts = query.split( "&" ); //$NON-NLS-1$
              for( String requestPart : requestParts )
              {
                String[] queryParts = requestPart.split( "=" ); //$NON-NLS-1$
                if( queryParts.length != 2 )
                  continue;

                wmsParameter.put( queryParts[0], queryParts[1] );
              }
            }

            /* The first valid url is enough. */
            break;
          }

          /* The first http-protocol is enough. */
          break;
        }
      }
    }

    wmsParameter.put( "SERVICE", "WMS" ); //$NON-NLS-1$ //$NON-NLS-2$
    wmsParameter.put( "VERSION", capabilities.getVersion() ); //$NON-NLS-1$
    wmsParameter.put( "REQUEST", operationName ); //$NON-NLS-1$
    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" ); //$NON-NLS-1$ //$NON-NLS-2$

    return wmsParameter;
  }

  /**
   * Tries to find the operation.
   * 
   * @param capabilities
   *            The wms capabilities.
   * @param name
   *            The name of the operation.
   * @return The operation.
   * @throws CoreException
   *             If this service does not supports this operation.
   */
  private static Operation checkOperation( WMSCapabilities capabilities, String name ) throws CoreException
  {
    Operation operation = capabilities.getOperationMetadata().getOperation( new QualifiedName( name ) );
    if( operation == null )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities.44") + name ) ); //$NON-NLS-1$

    return operation;
  }

  /**
   * This method tries to find a common spatial reference system (srs) for a given set of layers. If all layers
   * coorespond to the local crs the local crs is returned, otherwise the srs of the top layer is returned and the
   * client must choose one to transform it to the local coordinate system
   * 
   * @param localCRS
   *            The local spatial reference system.
   * @param capabilities
   *            The capabilites document of the web map service.
   * @param layerNames
   *            The layers that have to be matched to the local srs.
   * @return An array of possible coordiante systems.
   */
  public static String[] negotiateCRS( String localCRS, WMSCapabilities capabilities, String[] layerNames )
  {
    Layer topLayer = capabilities.getLayer();
    String crs = matchCrs( topLayer, layerNames, localCRS );
    if( crs != null )
      return new String[] { localCRS };

    /* Get crs from top layer. */
    String[] topLayerSRS = topLayer.getSrs();

    return topLayerSRS;
  }

  /**
   * This method tries to match the local coordinate system to a given layer selection.
   * 
   * @param topLayer
   *            The top layer of the layer structur of a web map service.
   * @param layerSelection
   *            Layers to be matched.
   * @param localCRS
   *            The local coordinate system.
   * @return Null, if one element of the layers to be matched is not available in the local coordinate system, otherwise
   *         it returns the local crs.
   */
  private static String matchCrs( Layer topLayer, String[] layerSelection, String localCRS )
  {
    HashSet<Layer> collector = new HashSet<Layer>();

    collect( collector, topLayer, layerSelection );

    for( Layer layer : collector )
    {
      String[] layerSRS = layer.getSrs();
      if( contains( layerSRS, localCRS ) )
        continue;

      return null;
    }

    return localCRS;
  }

  /**
   * This method collects all layers (or the specified layers) from the top layer of a WMSCapabilites document. If the
   * parameter layerSeletion is empty or null the method collects all layers, otherwise returns all layers with the same
   * name as in the layerSelection.
   * 
   * @param collector
   *            The set that collects the layers found.
   * @param layer
   *            the top layer of the wms capabilites document.
   * @param layerSelection
   *            An array of layer names to search for.
   */
  private static void collect( Set<Layer> collector, Layer layer, String[] layerSelection )
  {
    final Layer[] layerTree = layer.getLayer();
    for( final Layer newLayer : layerTree )
    {
      if( newLayer.getLayer().length > 0 ) // it is a layer container
        collect( collector, newLayer, layerSelection );
      else if( layerSelection != null )
      {
        if( contains( layerSelection, newLayer.getName() ) )
          collector.add( newLayer );
      }
      else
        collector.add( newLayer );
    }
  }

  /**
   * This method checks an array of Strings for a given String to match.
   * 
   * @param array
   *            Strings to check for a match.
   * @param toMatch
   *            The string to match.
   * @return True, if the String is the array, false otherwise.
   */
  private static boolean contains( final String[] array, final String toMatch )
  {
    for( final String element : array )
      if( element.equals( toMatch ) )
        return true;
    return false;
  }

  /**
   * This method gets the max bounding box of a wms layer.
   * 
   * @param layers
   *            The layers in the map in an array.
   * @return The max bounding box of a wms layer.
   */
  public static GM_Envelope getMaxExtent( String[] layers, WMSCapabilities capabilites, String srs ) throws Exception
  {
    GeoTransformer geoTransformer = new GeoTransformer( srs );

    Layer topLayer = capabilites.getLayer();
    HashSet<Layer> layerCollector = new HashSet<Layer>();

    collect( layerCollector, topLayer, layers );

    GM_Envelope resultEnvelope = null;
    for( Layer layer : layerCollector )
    {
      LayerBoundingBox[] bbox = layer.getBoundingBoxes();
      for( LayerBoundingBox env : bbox )
      {
        GM_Envelope kalypsoEnv = GeometryFactory.createGM_Envelope( env.getMin().getX(), env.getMin().getY(), env.getMax().getX(), env.getMax().getY(), env.getSRS() );
        GM_Envelope kalypsoEnvTransformed = null;

        boolean transformNeeded = !env.getSRS().equals( srs );
        if( transformNeeded )
          kalypsoEnvTransformed = geoTransformer.transformEnvelope( kalypsoEnv, env.getSRS() );
        else
          kalypsoEnvTransformed = kalypsoEnv;

        /* Merge into result envelope */
        resultEnvelope = resultEnvelope == null ? kalypsoEnv : resultEnvelope.getMerged( kalypsoEnvTransformed );
      }
    }

    if( resultEnvelope != null )
      return resultEnvelope;

    /* Use env from toplayer. */
    if( topLayer.getLatLonBoundingBox() == null )
      return null;

    /* Convert top layer env to request srs. */
    GM_Envelope envLatLon = GeometryFactory.createGM_Envelope( topLayer.getLatLonBoundingBox().getMin().getX(), topLayer.getLatLonBoundingBox().getMin().getY(), topLayer.getLatLonBoundingBox().getMax().getX(), topLayer.getLatLonBoundingBox().getMax().getY(), topLayer.getLatLonBoundingBox().getCoordinateSystem().getIdentifier() );
    String latlonSRS = "EPSG:4326"; //$NON-NLS-1$

    return geoTransformer.transformEnvelope( envLatLon, latlonSRS );
  }

  /**
   * This method collects all layers from a capabilites document.
   * 
   * @param capabilites
   *            WMS capabilites document.
   * @param set
   *            The Set, where the layers are collected in.
   */
  public static void getAllLayers( WMSCapabilities capabilites, Set<Layer> set )
  {
    try
    {
      Layer topLayer = capabilites.getLayer();
      collect( set, topLayer, null );
    }
    catch( Exception ex )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( ex ) );
    }
  }

  /**
   * This function returns an transformed envelope.
   * 
   * @param serverEnv
   *            The server envelope.
   * @param serverCRS
   *            The server coordinate system.
   * @param local
   *            The local coordinate system.
   * @return The transformed envelope.
   */
  public static GM_Envelope getTransformedEnvelope( GM_Envelope serverEnv, String serverCRS, String local )
  {
    try
    {
      GeoTransformer gt = new GeoTransformer( local );

      return gt.transformEnvelope( serverEnv, serverCRS );
    }
    catch( final Exception ex )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( ex ) );

      return null;
    }
  }

  /**
   * This function converts an envelope to a string representation.
   * 
   * @param envelope
   *            The envelope.
   * @return The string representation of the envelope.
   */
  public static String env2bboxString( GM_Envelope env )
  {
    return env.getMin().getX() + "," + env.getMin().getY() + "," + env.getMax().getX() + "," + env.getMax().getY(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }
}