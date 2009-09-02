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

package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author huebsch
 */
public class BodentypManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  private final IFeatureType m_bodentypFT;

  private final IFeatureType m_bodenartFT;

  // private static final String BodArtParameterPropName = "soilLayerParameterMember";

  public BodentypManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_conf = conf;
    m_bodentypFT = parameterSchema.getFeatureType( NaModelConstants.PARA_Soiltype_FT );
    m_bodenartFT = parameterSchema.getFeatureType( NaModelConstants.PARA_SoilLayerParameter_FT );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( int id, IFeatureType ft )
  {
    // return ft.getName() + id;
    return ft.getQName().getLocalPart() + id;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    List<Feature> result = new ArrayList<Feature>();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    // Kommentarzeilen
    for( int i = 0; i <= 2; i++ )
    {
      String line;
      line = reader.readLine();
      if( line == null )
        return null;

      System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    }
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );

  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 1
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 1 );

    // generate id:
    String asciiStringId = propCollector.get( "name" ); //$NON-NLS-1$
    final Feature feature = getFeature( asciiStringId, m_bodentypFT );

    int ianz = Integer.parseInt( propCollector.get( "ianz" ) ); //$NON-NLS-1$
    HashMap<String, String> bodArtPropCollector = new HashMap<String, String>();
    // BodArtParameterMember
    for( int i = 0; i < ianz; i++ )
    {
      Feature bodArtParameterFeature = createFeature( m_bodenartFT );
      line = reader.readLine();
      System.out.println( Messages.getString("org.kalypso.convert.namodel.manager.BodentypManager.4", i , line)); //$NON-NLS-1$ 
      createProperties( bodArtPropCollector, line, 2 );
      // BodArtLink
      // final FeatureProperty BodArtNameProp = (FeatureProperty)bodArtPropCollector.get( "name" );
      String asciiBodArtId = bodArtPropCollector.get( "name" ); //$NON-NLS-1$
      final Feature BodArtFE = getFeature( asciiBodArtId, m_conf.getBodartFT() );

      bodArtPropCollector.put( "soilLayerLink", BodArtFE.getId() ); //$NON-NLS-1$
      bodArtParameterFeature.setProperty( NaModelConstants.PARA_SOIL_LAYER_LINK, BodArtFE.getId() );

      // Collection collection = bodArtPropCollector.values();
      setParsedProperties( bodArtParameterFeature, bodArtPropCollector, null );

      final IPropertyType pt = m_bodentypFT.getProperty( NaModelConstants.PARA_SOIL_LAYER_PARAMETER_MEMBER );
      FeatureHelper.addProperty( feature, pt, bodArtParameterFeature );
    }

    // continue reading
    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    List list = (List) rootFeature.getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );
    asciiBuffer.getBodtypBuffer().append( "/Bodentypen:\n" ); //$NON-NLS-1$
    asciiBuffer.getBodtypBuffer().append( "/\n" ); //$NON-NLS-1$
    asciiBuffer.getBodtypBuffer().append( "/Typ       Tiefe[dm]\n" ); //$NON-NLS-1$
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {

      final Feature bodentypFE = (Feature) iter.next();
      // TODO: nur die schreiben, die auch in Hydrotopdatei vorkommen
      // if( asciiBuffer.writeFeature( bodentypFE ) )
      writeFeature( asciiBuffer, paraWorkspace, bodentypFE );
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace, Feature feature ) throws Exception
  {
    // 1
    List bodartList = (List) feature.getProperty( NaModelConstants.PARA_SOIL_LAYER_PARAMETER_MEMBER );
    asciiBuffer.getBodtypBuffer().append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "name" ), "a10" ) + FortranFormatHelper.printf( Integer.toString( bodartList.size() ), "i4" )  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
        + "\n" ); //$NON-NLS-1$
    // 2
    Iterator iter = bodartList.iterator();
    while( iter.hasNext() )
    {
      Feature fe = (Feature) iter.next();

      Feature BodArtLink = paraWorkspace.resolveLink( fe, (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) );
      if( BodArtLink != null )
      {
        Boolean xretProp = (Boolean) fe.getProperty( NaModelConstants.PARA_PROP_XRET );
        if( xretProp )
        {
          asciiBuffer.getBodtypBuffer().append( FortranFormatHelper.printf( FeatureHelper.getAsString( BodArtLink, "name" ), "a8" )  //$NON-NLS-1$//$NON-NLS-2$
              + FortranFormatHelper.printf( FeatureHelper.getAsString( fe, "xtief" ), "*" ) + " " + "1.0" + "\n" );  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        }
        else
        {
          asciiBuffer.getBodtypBuffer().append( FortranFormatHelper.printf( FeatureHelper.getAsString( BodArtLink, "name" ), "a8" )  //$NON-NLS-1$//$NON-NLS-2$
              + FortranFormatHelper.printf( FeatureHelper.getAsString( fe, "xtief" ), "*" ) + " " + "0.0" + "\n" );  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        }
      }
      else
      {
        // TODO use logger
        System.out.println( Messages.getString("org.kalypso.convert.namodel.manager.BodentypManager.29", feature.getId(),fe.getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
    }

  }

}
