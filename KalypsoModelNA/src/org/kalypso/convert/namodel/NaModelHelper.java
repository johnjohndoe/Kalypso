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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileFilter;
import java.util.Iterator;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

public class NaModelHelper
{
  // Elements
  public static final String CATCHMENT_ELEMENT_NAME = "Catchment"; //$NON-NLS-1$

  public static final String NODE_ELEMENT_NAME = "Node"; //$NON-NLS-1$

  public static final String V_CHANNEL_ELEMENT_NAME = "VirtualChannel"; //$NON-NLS-1$

  public static final String KM_CHANNEL_ELEMENT_NAME = "KMChannel"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_ELEMENT_NAME = "StorageChannel"; //$NON-NLS-1$

  // Collection props

  // Link Properties
  public static final String LINK_CATCHMENT_CHANNEL = "entwaesserungsStrangMember"; //$NON-NLS-1$

  public static final String LINK_CHANNEL_DOWNSTREAMNODE = "downStreamNodeMember"; //$NON-NLS-1$

  public static final String LINK_NODE_DOWNSTREAMCHANNEL = "downStreamChannelMember"; //$NON-NLS-1$

  // Geometrie Properties of Model
  public static final String NODE_GEOM_PROP = "Ort"; //$NON-NLS-1$

  public static final String CHANNEL_GEOM_PROP = "Ort"; //$NON-NLS-1$

  public static final String CATCHMENT_GEOM_PROP = "Ort"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_ZMLINLINE_PROP = "hvvsqd"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_VMAX_PROP = "vmax"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_VMIN_PROP = "vmin"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_SV_PROP = "sv"; //$NON-NLS-1$

  public static final String STORAGE_CHANNEL_C_PROP = "c"; //$NON-NLS-1$

  // Measure Props
  private static final String RETENSION_PROP_SLOPE = "bankslope"; //$NON-NLS-1$

  private static final String RETENSION_PROP_DEPTH = "depth"; //$NON-NLS-1$

  // general Props
  public static final String GENERATE_RESULT_PROP = "generateResult"; //$NON-NLS-1$

  // return constants
  public static final int OPERATION_FAILED = -1;

  public static final int OPERATION_SUCCEEDED = 0;

  private static final double m_factorHecto = 1 / 100d;

  public static final String EXECUTABLES_FOLDER = "bin";

  public static final String EXECUTABLES_FILE_TEMPLATE = "na-kalypso_%s.exe";

  public static final String EXECUTABLES_FILE_PATTERN = "na-kalypso_(.+)\\.exe";

  // public static File getResultFile( URL context, Feature nodeFE ) throws MalformedURLException
  // {
  // final TimeseriesLink link = (TimeseriesLink)nodeFE.getProperty( "qberechnetZR" );
  // if( link == null )
  // return null;
  // final String href = link.getHref().replaceAll( "\\?.*", "" ); // optionen
  // // loeschen
  //
  // IUrlResolver res = new UrlUtilities();
  // final URL url = res.resolveURL( context, href );
  //
  // return new File( url.getFile() );
  // }
  //
  // public static boolean resultExists( GMLWorkspace modellWorkspace, Feature nodeFE )
  // {
  // try
  // {
  // final File resultFile = NaModelHelper.getResultFile( modellWorkspace.getContext(), nodeFE );
  // if( resultFile == null )
  // return false;
  // return resultFile.exists();
  // }
  // catch( MalformedURLException e )
  // {
  // return false;
  // }
  // }

  public static int addRHBinCatchment( final GMLWorkspace modelworkspace, final Feature catchment, final IFeatureType rhbFT, final Feature measureRhbFE )
  {
    // throw new UnsupportedOperationException( "still a TODO, not upgraded after java5.0/eclipse3.1" );
    // create new Properties for Storage
    try
    {
      // create Observation (WQV-relation) for storage channel
      final GM_Object measuerRhbGEOM = measureRhbFE.getDefaultGeometryProperty();
      final Double slope = (Double) measureRhbFE.getProperty( new QName( "http://schema.kalypso.wb.tu-harburg.de/measure/rhb", RETENSION_PROP_SLOPE ) ); //$NON-NLS-1$
      final Double depth = (Double) measureRhbFE.getProperty( new QName( "http://schema.kalypso.wb.tu-harburg.de/measure/rhb", RETENSION_PROP_DEPTH ) ); //$NON-NLS-1$
      final Double min = new Double( 0 );
      final int max = 10;
      final Double intervall = new Double( depth.doubleValue() / max );
      final Geometry geometry = JTSAdapter.export( measuerRhbGEOM );
      final double area = geometry.getArea();
      final double lo = Math.sqrt( area );
      // final double lu = lo - 2 * slope.doubleValue() * depth.doubleValue();
      /** Das Volumen muss immer in hm^3 sein, deshalb wird hier durch hundert geteilt !!!! */
      // final double maxVol = depth.doubleValue() / 3 * m_factorHecto * (Math.pow( lo, 2d ) + Math.pow( lu, 2d ) +
      // Math.sqrt( Math.pow( lo, 2d ) * Math.pow( lu, 2d ) ));
      final ZmlInlineTypeHandler typeHandler = (ZmlInlineTypeHandler) MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( new QName( "inline.zml.kalypso.org", "ZmlInlineWVQType" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final IAxis[] axis = TimeserieUtils.createDefaultAxes( typeHandler.getAxisTypes(), true );
      final Object[][] values = new Object[10][axis.length];
      final Iterator iterator = new ValueIterator( min, intervall, max );
      for( int row = 0; row < max; row++ )
      {
        values[row][0] = iterator.next();
        final double sumHeigth = ((Double) values[row][0]).doubleValue();
        final Double vol = getVolume( sumHeigth, lo, slope.doubleValue(), depth.doubleValue(), m_factorHecto );
        final Double discharge = getDischarge( sumHeigth, 0.65, 9.91, 0.5, m_factorHecto );
        values[row][1] = vol;
        values[row][2] = discharge;
      }
      // final ITuppleModel model = new SimpleTuppleModel( axis, values );
      // IObservation obs = new SimpleObservation( null, null, "RHB WVQ-Bezeihung", true, null, new MetadataList(),
      // axis, model );
      // set properties for new storage channel
      // Map propteries = new HashMap();

      // propteries.put( FeatureFactory.createFeatureTypeProperty( STORAGE_CHANNEL_ZMLINLINE_PROP,
      // NaModelConstants.NS_NAMODELL, String.class.getName(), true, null ), obs );
      // propteries.put( FeatureFactory.createFeatureTypeProperty( STORAGE_CHANNEL_VMAX_PROP,
      // NaModelConstants.NS_NAMODELL, Double.class.getName(), true, null ), new Double( maxVol ) );
      // propteries.put( FeatureFactory.createFeatureTypeProperty( STORAGE_CHANNEL_VMIN_PROP,
      // NaModelConstants.NS_NAMODELL, Double.class.getName(), true, null ), new Double( 0 ) );
      // propteries.put( FeatureFactory.createFeatureTypeProperty( STORAGE_CHANNEL_C_PROP, NaModelConstants.NS_NAMODELL,
      // Double.class.getName(), true, null ), new Double( 0 ) );
      // propteries.put( FeatureFactory.createFeatureTypeProperty( STORAGE_CHANNEL_SV_PROP,
      // NaModelConstants.NS_NAMODELL, Double.class.getName(), true, null ), new Double( 0 ) );
      // // get linked features
      // final Feature originalDischargChannel = modelworkspace.resolveLink( catchment, LINK_CATCHMENT_CHANNEL );
      // final Feature downStreamNode = modelworkspace.resolveLink( originalDischargChannel, LINK_CHANNEL_DOWNSTREAMNODE
      // );
      // // create new node at position
      // int posNode = 0;
      // final IFeatureType nodeFT = modelworkspace.getFeatureType( NODE_ELEMENT_NAME );
      // final Feature parentFeatureNode = (Feature) modelworkspace.getFeatureFromPath( NODE_COLLECTION_NAME );
      // final AddFeatureCommand addNode = new AddFeatureCommand( modelworkspace, nodeFT, parentFeatureNode,
      // NODE_COLLECTION_MEMBER, posNode, null, null );
      // modelworkspace.postCommand( addNode );
      //
      // // old:
      // // Catchment#10 -> Channel#11
      // //
      // // new:
      // // Catchment#10 (existing)
      // // V
      // // new VChannel#2 < new Node#1
      // // V
      // // new Node#3
      // // V
      // // new RHBChannel#4
      // // V
      // // new Node#5
      // // V
      // // Channel#11 (existing)
      // //
      //
      // final Feature[] nodes = modelworkspace.getFeatures( nodeFT );
      // // create new storage channel
      // int posChannel = 0;
      // final Feature parentFeatureChannel = (Feature) modelworkspace.getFeatureFromPath( CHANNEL_COLLECTION_NAME );
      // final AddFeatureCommand addChannel = new AddFeatureCommand( modelworkspace, rhbFT, parentFeatureChannel,
      // CHANNEL_COLLECTION_MEMBER, posChannel, propteries, null );
      // modelworkspace.postCommand( addChannel );
      // final Feature[] channels = modelworkspace.getFeatures( rhbFT );
      // // remove old links from channel to node
      // final RemoveRelationCommand removeChannelNodeLink = new RemoveRelationCommand( modelworkspace,
      // originalDischargChannel, LINK_CHANNEL_DOWNSTREAMNODE, downStreamNode );
      // modelworkspace.postCommand( removeChannelNodeLink );
      // // Insert new links
      // final AddRelationCommand addStorgeChannelNodeLink = new AddRelationCommand( modelworkspace,
      // channels[posChannel], LINK_CHANNEL_DOWNSTREAMNODE, 0, downStreamNode );
      // modelworkspace.postCommand( addStorgeChannelNodeLink );
      //
      // final AddRelationCommand addNodeChannelLink = new AddRelationCommand( modelworkspace, nodes[posNode],
      // LINK_NODE_DOWNSTREAMCHANNEL, 0, channels[posChannel] );
      // modelworkspace.postCommand( addNodeChannelLink );
      //
      // final AddRelationCommand addChannelNodeLink = new AddRelationCommand( modelworkspace, originalDischargChannel,
      // LINK_CHANNEL_DOWNSTREAMNODE, 0, nodes[posNode] );
      // modelworkspace.postCommand( addChannelNodeLink );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return OPERATION_FAILED;
    }

    return OPERATION_SUCCEEDED;
  }

  /**
   * @param sumHeigth
   * @param mu
   * @param g
   * @param diameter
   * @param factorHecto
   * @return
   */
  private static Double getDischarge( final double sumHeigth, final double mu, final double g, final double diameter, final double factorHecto )
  {
    final double area = Math.pow( diameter, 2d ) * Math.PI / 4;
    final double q = mu * area * Math.pow( 2 * g * sumHeigth, 2d ) * factorHecto;
    return new Double( q );
  }

  /**
   * @param factorHecto
   * @param double1
   * @return
   */
  private static Double getVolume( final double waterlevel, final double lenght, final double slope, final double depth, final double factorHecto )
  {
    final double value = (4 / 3 * Math.pow( slope, 2d ) * (Math.pow( waterlevel, 2d ) - 3 * depth * Math.pow( waterlevel, 2d ) + 3 * Math.pow( depth, 2d ) * waterlevel) + 2 * slope * lenght
        * (Math.pow( waterlevel, 2d ) - 2 * depth * waterlevel) + Math.pow( lenght, 2d ) * waterlevel)
        * factorHecto;
    return new Double( value * factorHecto );
  }

  public static File findExecutable( final String version )
  {
    /*
     * for backward compatibility, any string that is not the correct version identifier will be considered as the
     * "latest version" request
     */
    if( version == null || version.length() == 0 )
      return getLatestExecutable();

    // REMARK: This is OS dependent; we use should use a pattern according to OS
    final String exeName = String.format( EXECUTABLES_FILE_TEMPLATE, version );

    final File exeFile = new File( getExecutablesDirectory(), exeName );
    if( exeFile != null && exeFile.exists() && exeFile.isFile() )
      return exeFile;
    return getLatestExecutable();
  }

  public static File getLatestExecutable( )
  {
    /*
     * we will assume that the latest executable is the one with the most recent file modification date
     */
    final File[] executables = getExecutablesDirectory().listFiles( new FileFilter()
    {
      @Override
      public boolean accept( final File pathname )
      {
        return Pattern.matches( EXECUTABLES_FILE_PATTERN, pathname.getName() );
      }
    } );
    if( executables == null || executables.length == 0 )
      return null;
    if( executables.length == 1 )
      return executables[0];
    File latest = executables[0];
    for( int i = 1; i < executables.length; i++ )
    {
      if( executables[i].lastModified() > latest.lastModified() )
        latest = executables[i];
    }
    return latest;
  }

  public static File getExecutablesDirectory( )
  {
    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    final File exeDir = new File( installDir, EXECUTABLES_FOLDER );
    return exeDir;
  }

}