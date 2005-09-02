package org.kalypsodeegree_impl.graphics.sld;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.logging.Logger;

import org.deegree.services.wms.StyleNotDefinedException;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

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

/**
 * This Factory is intended to supply default styles as SLD (Styled Layer Discribtor). When the factoy is created it is
 * initialized with the physical location to save the generated SLDs to. The Factory creates default styles for a
 * specific feature type.The styles are registerd as key value pair. The key is namespace plus feature type name.
 * 
 * @author Christoph Küpferle, Technische Universität Hamburg-Harburg
 */
public class DefaultStyleFactory
{

  private static DefaultStyleFactory m_factory = null;

  private final String DEFAULT_STYLE_DRECTORY;

  //  private static FeatureTypeProperty m_GeomProperty;

  private HashMap m_defalultStyles = new HashMap();

  private static final Logger LOGGER = Logger.getLogger( DefaultStyleFactory.class.getName() );

  private DefaultStyleFactory( String dir )
  {
    DEFAULT_STYLE_DRECTORY = dir;
    LOGGER.info( "SLD-Default-Katalog initialisiert mit DIR=" + dir );
  }

  public static DefaultStyleFactory getFactory( String defaultStyleDirectory ) throws IOException
  {
    if( defaultStyleDirectory != null || !defaultStyleDirectory.equals( "" ) )
    {
      if( m_factory == null )
      {
        return m_factory = new DefaultStyleFactory( defaultStyleDirectory );
      }
      return m_factory;
    }
    throw new IOException( "The name for the default style directory is not a valid path." );
  }

  private String getStyle( FeatureType featureType, String styleName ) throws StyleNotDefinedException
  {
    StyledLayerDescriptor sld = createDefaultStyle( featureType, styleName );
    return ( (StyledLayerDescriptor_Impl)sld ).exportAsXML();
  }

  /**
   * This method creats a default style and writes this style in the common default style directory ( defined when the
   * factory is created). At this time only symbolizers for geometries are supported.
   * 
   * @param featureType
   *          the featureType for which a default style needs to be genearted
   * @return returns the location (absolute path to local file system ) where the style is saved in a file
   *  
   */
  public URL getDefaultStyle( FeatureType featureType, String styleName ) throws StyleNotDefinedException
  {
    String myStyleName = styleName;
    if( myStyleName == null )
    {
      myStyleName = featureType.getName();
    }
    try
    {
      //check if style already exists
      if( m_defalultStyles.containsKey( generateKey( featureType ) ) )
        return new URL( (String)m_defalultStyles.get( generateKey( featureType ) ) );
      //write style to default style location
      File tempFile = null;
      tempFile = new File( DEFAULT_STYLE_DRECTORY, myStyleName + ".sld.default" );
      FileWriter writer = new FileWriter( tempFile );
      writer.write( getStyle( featureType, myStyleName ) );
      writer.close();
      m_defalultStyles.put( generateKey( featureType ), tempFile.toURL().toString() );
      return tempFile.toURL();
    }
    catch( IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private StyledLayerDescriptor createDefaultStyle( FeatureType featureType, String styleName )
      throws StyleNotDefinedException
  {
    ArrayList symbolizer = new ArrayList();
    if( featureType.getName().equals( "RectifiedGridCoverage" ) )
    {
      symbolizer.add( createRasterSymbolizer() );
    }
    FeatureTypeProperty[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      FeatureTypeProperty property = properties[i];
      //      if( property instanceof FeatureAssociationTypeProperty )
      //      {
      //        FeatureType associationFeatureType = ((FeatureAssociationTypeProperty)property).getAssociationFeatureType();
      //        FeatureTypeProperty[] properties2 = associationFeatureType.getProperties();
      //        for( int j = 0; j < properties2.length; j++ )
      //        {
      //          FeatureTypeProperty property2 = properties2[j];
      //          if( property2.isGeometryProperty() )
      //            symbolizer.add( createGeometrySymbolizer( property2));
      //        }
      //      }
      if( GeometryUtilities.isGeometry(property) )
      {
        //        m_GeomProperty = property;
        symbolizer.add( createGeometrySymbolizer( property ) );
      }
      //      if( m_GeomProperty == null )
      //      {
      //        FeatureTypeProperty[] vProperty = featureType
      //            .getVirtuelFeatureTypeProperty();
      //        //check virtual properties
      //        if( vProperty != null )
      //        {
      //          for( int j = 0; j < vProperty.length; j++ )
      //          {
      //            FeatureTypeProperty vp = vProperty[i];
      //            if( vp.isGeometryProperty() )
      //              symbolizer.add( createGeometrySymbolizer( vp ) );
      //          }
      //        }
      //      }
    }

    FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( styleName, (Symbolizer[])symbolizer
        .toArray( new Symbolizer[symbolizer.size()] ) );

    StyledLayerDescriptor sld = null;

    Style[] styles = new Style[]
    { (UserStyle_Impl)StyleFactory.createStyle( styleName, styleName, "empty Abstract", featureTypeStyle ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0.0" );

    return sld;

  }

  public static StyledLayerDescriptor createDefaultStyle( String styleName, Symbolizer[] symbolizer )
  {
    FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( styleName, symbolizer );

    StyledLayerDescriptor sld = null;

    Style[] styles = new Style[]
    { (UserStyle_Impl)StyleFactory.createStyle( styleName, styleName, "no Abstract", featureTypeStyle ) };
    org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[]
    { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0.0" );

    return sld;
  }

  //  private Symbolizer createTextSymbolizer( FeatureTypeProperty property ) throws StyleNotDefinedException
  //  {
  //    String type = property.getType();
  //    if( type.equals( String.class.getName() ) )
  //    {
  //      return StyleFactory.createTextSymbolizer( m_GeomProperty.getType(), property.getName(), StyleFactory
  //          .createLabelPlacement( StyleFactory.createPointPlacement() ) );
  //    }
  //    throw new StyleNotDefinedException( "Error while creating TextSymbolizer from string type. Property name: "
  //        + property.getName() );
  //  }

  private Symbolizer createGeometrySymbolizer( FeatureTypeProperty ftp ) throws StyleNotDefinedException
  {
    if( GeometryUtilities.isPointGeometry( ftp ) )
      return StyleFactory.createPointSymbolizer();
    else if( GeometryUtilities.isLineStringGeometry( ftp ) )
      return StyleFactory.createLineSymbolizer();
    else if( GeometryUtilities.isPolygonGeometry( ftp ) )
      return StyleFactory.createPolygonSymbolizer();
    else
      throw new StyleNotDefinedException( "This geometry type: " + ftp.getType() + " has no default style available" );
  }

  private Symbolizer createRasterSymbolizer()
  {
    return StyleFactory.createRasterSymbolizer();
  }

  private Integer generateKey( FeatureType ft )
  {

    String key = ft.getNamespace() + " " + ft.getName();
    return new Integer( key.hashCode() );
  }

  /**
   * 
   * This Method clears the default style directory
   *  
   */

  public void clear()
  {
    File dir = new File( DEFAULT_STYLE_DRECTORY );
    if( dir.exists() )
    {
      String[] files = dir.list();
      for( int i = 0; i < files.length; i++ )
      {
        File file = new File( DEFAULT_STYLE_DRECTORY, files[i] );
        if( file.exists() )
          file.delete();
      }
    }

  }

}