/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class ChannelManager extends AbstractManager
{
  public static final int VIRTUALCHANNEL = 0;

  private static final int KMCHANNEL = 1;

  private static final int STORAGECHANNEL = 2;

  private static final QName KMParameterpropName = NaModelConstants.KM_CHANNEL_PARAMETER_MEMBER;

  private final IFeatureType m_virtualChannelFT;

  private final IFeatureType m_storageChannelFT;

  private final IFeatureType m_kmChannelFT;

  private IFeatureType m_kmParameterFT;

  private final NAConfiguration m_conf;

  public ChannelManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getChannelFormatURL() );
    m_conf = conf;
    m_virtualChannelFT = schema.getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT);
    m_storageChannelFT = schema.getFeatureType(  NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT);
    m_kmChannelFT = schema.getFeatureType(  NaModelConstants.KM_CHANNEL_ELEMENT_FT);
    m_kmParameterFT = schema.getFeatureType( NaModelConstants.KM_CHANNEL_PARAMETER_FT);
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    List<Feature> result = new ArrayList<Feature>();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // FileReader(
    // file
    // ) );
    Feature fe = null;
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 0-1
    for( int i = 0; i <= 1; i++ )
    {
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( i + ": " + line );
      createProperties( propCollector, line, i );
    }
    // FeatureProperty idProp = (FeatureProperty)propCollector.get( "inum" );
    // FeatureProperty artProp = (FeatureProperty)propCollector.get( "iart" );

    int asciiID = Integer.parseInt( propCollector.get( "name" ) );
    int art = Integer.parseInt( propCollector.get( "iart" ) );
    Feature feature;
    switch( art )
    {
      case VIRTUALCHANNEL:
        feature = getFeature( asciiID, m_virtualChannelFT );
        break;
      case KMCHANNEL:
        feature = getFeature( asciiID, m_kmChannelFT );
        line = reader.readLine();
        System.out.println( 2 + ": " + line );
        createProperties( propCollector, line, 2 );
        // parse kalinin-miljukov-parameter
        HashMap<String, String> kmPropCollector = new HashMap<String, String>();

        for( int i = 0; i < 5; i++ )
        {
          Feature kmParameterFeature = createFeature( m_kmParameterFT );
          line = reader.readLine();
          System.out.println( " km(" + i + "): " + line );
          createProperties( kmPropCollector, line, 3 );
          // final Collection collection = kmPropCollector.values();
          setParsedProperties( kmParameterFeature, kmPropCollector, null );
          final IPropertyType pt = feature.getFeatureType().getProperty( KMParameterpropName );
          FeatureHelper.addProperty( feature, pt, kmParameterFeature );
        }
        break;
      case STORAGECHANNEL:
        feature = getFeature( asciiID, m_storageChannelFT );
        break;
      default:
        throw new UnsupportedOperationException( "ChannelType " + art + " is not supported" );
    }
    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace ) throws Exception
  {
    final List<Feature> channelList = new ArrayList<Feature>();
    channelList.addAll( Arrays.asList( workspace.getFeatures( m_virtualChannelFT ) ) );
    channelList.addAll( Arrays.asList( workspace.getFeatures( m_kmChannelFT ) ) );
    channelList.addAll( Arrays.asList( workspace.getFeatures( m_storageChannelFT ) ) );
    final Iterator<Feature> iter = channelList.iterator();
    while( iter.hasNext() )
    {
      final Feature channelFE = iter.next();
      if( asciiBuffer.writeFeature( channelFE ) )
        writeFeature( asciiBuffer, channelFE, workspace );
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, final Feature feature, GMLWorkspace workspace ) throws Exception
  {
    IDManager idManager = m_conf.getIdManager();
    asciiBuffer.getChannelBuffer().append( idManager.getAsciiID( feature ) + "\n" );
    // asciiBuffer.getChannelBuffer().append( toAscci( feature, 0 ) + "\n" );
    IFeatureType ft = feature.getFeatureType();
    if( "VirtualChannel".equals( ft.getQName().getLocalPart() ) )
      asciiBuffer.getChannelBuffer().append( VIRTUALCHANNEL + "\n" );
    else if( "KMChannel".equals( ft.getQName().getLocalPart() ) )
    {
      asciiBuffer.getChannelBuffer().append( KMCHANNEL + "\n" );
      List kmFeatures = (List) feature.getProperty( KMParameterpropName );

      for( int i = 0; i < kmFeatures.size(); i++ )
      {
        Feature kmFE = (Feature) kmFeatures.get( i );
        asciiBuffer.getChannelBuffer().append( toAscci( kmFE, 3 ) + "\n" );
      }

    }
    else if( "StorageChannel".equals( ft.getQName().getLocalPart() ) )
    {
      asciiBuffer.getChannelBuffer().append( STORAGECHANNEL + "\n" );

      // (txt,a8)(inum,i8)(iknot,i8)(c,f6.2-dummy)
      // RHB 5-7
      asciiBuffer.getRhbBuffer().append( "SPEICHER" + FortranFormatHelper.printf( idManager.getAsciiID( feature ), "i8" ) );
      // Ueberlaufknoten optional
      final IRelationType rt2 = (IRelationType) feature.getFeatureType().getProperty( NaModelConstants.IKNOT_MEMBER_PROP );
      Feature nodeFE = workspace.resolveLink( feature, rt2 );
      final IRelationType rt = (IRelationType) feature.getFeatureType().getProperty( NaModelConstants.DOWNSTREAM_NODE_MEMBER_PROP );
      final Feature dnodeFE = workspace.resolveLink( feature, rt );
      if( nodeFE == null || nodeFE == dnodeFE )
        asciiBuffer.getRhbBuffer().append( "       0" );
      else
        asciiBuffer.getRhbBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( nodeFE ), "i8" ) );
      asciiBuffer.getRhbBuffer().append( "  0.00" + "\n" );
      // (itext,a80)
      // RHB 8
      asciiBuffer.getRhbBuffer().append( toAscci( feature, 8 ) + "\n" );
      // (lfs,i4)_(nams,a10)(sv,f10.6)(vmax,f10.6)(vmin,f10.6)(jev,i4)(itxts,a10)
      // RHB 9-10
      asciiBuffer.getRhbBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( dnodeFE ), "i4" ) );
      Double sv = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_SV_PROP )) / 1000000;
      Double vmax = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_VMAX_PROP )) / 1000000;
      Double vmin = ((Double) feature.getProperty( NaModelConstants.STORAGE_CHANNEL_VMIN_PROP)) / 1000000;
      asciiBuffer.getRhbBuffer().append( " " + " FUNKTION " + FortranFormatHelper.printf( sv, "f9.6" ) + " " + FortranFormatHelper.printf( vmax, "f9.6" ) + " "
          + FortranFormatHelper.printf( vmin, "f9.6" ) );
      // asciiBuffer.getRhbBuffer().append( " " + " FUNKTION " + toAscci( feature, 10 ) );

      Object wvqProp = feature.getProperty(NaModelConstants.STORAGE_CHANNEL_HVVSQD_PROP  );
      if( wvqProp instanceof IObservation )
      {
        int size = (((IObservation) wvqProp).getValues( null )).getCount();
        asciiBuffer.getRhbBuffer().append( FortranFormatHelper.printf( size, "i4" ) + "\n" );
        if( size > 24 )
          throw new Exception( "Fehler!!! NA-Modell: Anzahl Wertetripel WVQ-Beziehung > maximale Anzahl (24), Rückhaltebecken: #" + FeatureHelper.getAsString( feature, "name" ) );
        // ____(hv,f8.2)________(vs,f9.6)______(qd,f8.3)
        writeWVQ( (IObservation) wvqProp, asciiBuffer.getRhbBuffer() );
      }
      else
      {
        System.out.println( "Es existiert keine (gültige) WQV-Beziehung für das Rückhaltebecken, ID: " + idManager.getAsciiID( feature ) );
      }

      // Kommentar Ende Speicher
      // RHB 12
      asciiBuffer.getRhbBuffer().append( "ENDE" + "\n" );

    }
    else
      throw new UnsupportedOperationException( "can not write Feature to ascii" + feature.toString() );
  }

  /**
   * @param observation
   * @param rhbBuffer
   * @throws SensorException
   */
  private void writeWVQ( IObservation observation, StringBuffer rhbBuffer ) throws SensorException
  {
    IAxis[] axisList = observation.getAxisList();
    IAxis waterTableAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_NORMNULL );
    IAxis volumeAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_VOLUME );
    IAxis dischargeAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_RUNOFF );
    ITuppleModel values = observation.getValues( null );
    int count = values.getCount();
    for( int row = 0; row < count; row++ )
    {
      Double w = (Double) values.getElement( row, waterTableAxis );
      Double v = ((Double) values.getElement( row, volumeAxis )) / 1000000;
      Double q = (Double) values.getElement( row, dischargeAxis );
      rhbBuffer.append( "    " + FortranFormatHelper.printf( w, "f8.2" ) );
      rhbBuffer.append( "        " + FortranFormatHelper.printf( v, "f9.6" ) );
      rhbBuffer.append( "      " + FortranFormatHelper.printf( q, "f8.3" ) + "\n" );
    }
  }

  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }
}