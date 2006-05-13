/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.Mapper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeaturePropertyVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeRegistry;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * This factory offers methods for creating Features, FeatureCollection and all direct related classes/interfaces that
 * are part of the org.kalypsodeegree.model.feature package.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class FeatureFactory
{
  /**
   * creates an instance of a Feature from its IFeatureType and an array of Objects that represents it properties. It is
   * assumed that the order of the properties is identical to the order of the FeatureTypeProperties of the the
   * IFeatureType.
   * 
   * @param id
   *          unique id of the <CODE>Feature</CODE>
   * @param featureType
   *          <CODE>IFeatureType</CODE> of the <CODE>Feature</CODE>
   * @param properties
   *          properties (content) of the <CODE>Feature</CODE>
   * @return instance of a <CODE>Feature</CODE>
   */
  public static Feature createFeature( Feature parent, String id, IFeatureType featureType, Object[] properties )
  {
    return new Feature_Impl( parent, featureType, id, properties );
  }

  /**
   * @param initializeWithDefaults
   *          set <code>true</code> to generate default properties (e.g. when generating from UserInterface) <br>
   *          set <code>false</code> to not generate default properties ( e.g. when reading from GML or so.)
   */
  public static Feature createFeature( final Feature parent, final String id, final IFeatureType featureType, final boolean initializeWithDefaults )
  {
    return new Feature_Impl( parent, featureType, id, initializeWithDefaults );
  }

  /** Creates default feature, used by LegendView */
  public static Feature createDefaultFeature( final Feature parent, final String id, final IFeatureType ft, final boolean createGeometry )
  {
    final IPropertyType[] propTypes = ft.getProperties();
    final Map<IPropertyType, Object> props = createDefaultFeatureProperty( propTypes, createGeometry );

    final Feature result = createFeature( parent, id, ft, false );
    FeatureHelper.setProperties( result, props );

    return result;
  }

  /** Creates default FeatureProperties, used by LegendView */
  public static Map<IPropertyType, Object> createDefaultFeatureProperty( final IPropertyType[] propTypes, final boolean createGeometry )
  {
    // TODO handle occurency here and generate empty List or FeatureList as
    // default
    final Map<IPropertyType, Object> results = new LinkedHashMap<IPropertyType, Object>();
    for( int i = 0; i < propTypes.length; i++ )
    {
      final IPropertyType ftp = propTypes[i];

      if( (ftp instanceof IValuePropertyType) )
      {
        final IValuePropertyType vpt = (IValuePropertyType) ftp;
        final Object value = Mapper.defaultValueforJavaType( vpt, createGeometry );
        results.put( ftp, value );
      }
    }
    return results;
  }

  public static FeatureList createFeatureList( final Feature parentFeature, final IRelationType parentFTP, final List list )
  {
    final SplitSort result = new SplitSort( parentFeature, parentFTP );
    result.addAll( list );
    return result;
  }

  public static FeatureList createFeatureList( final Feature parentFeature, final IRelationType parentFTP, final Feature[] features )
  {
    return createFeatureList( parentFeature, parentFTP, Arrays.asList( features ) );
  }

  public static FeatureList createFeatureList( final Feature parentFeature, final IRelationType parentFTP )
  {
    return new SplitSort( parentFeature, parentFTP );
  }

  public static FeatureList createFeatureList( final Feature parentFeature, final IRelationType parentFTP, final GM_Envelope env )
  {
    return new SplitSort( parentFeature, parentFTP, env );
  }

  public static IPropertyType[] createVirtualFeatureTypeProperties( IFeatureType realFeatureType )
  {
    final List<IPropertyType> result = new ArrayList<IPropertyType>();
    final VirtualFeatureTypeRegistry registry = VirtualFeatureTypeRegistry.getInstance();
    final IPropertyType[] properties = realFeatureType.getProperties();

    for( int i = 0; i < properties.length; i++ )
    {
      IPropertyType ftp = properties[i];
      IPropertyType[] newFtp = registry.getVirtualFeatureTypePropertiesFor( ftp );
      for( int j = 0; j < newFtp.length; j++ )
        result.add( newFtp[j] );
    }

    final IPropertyType[] vftp = registry.getVirtualFeatureTypePropertiesFor( realFeatureType );
    for( int i = 0; i < vftp.length; i++ )
      result.add( vftp[i] );

    return result.toArray( new IPropertyType[result.size()] );
  }

  public static GMLWorkspace createGMLWorkspace( GMLSchema schema, Feature rootFeature, URL context, String schemaLocation )
  {
    IFeatureType[] featureTypes = schema.getAllFeatureTypes();
    // final URL url = schema.getUrl();
    // final String href = url == null ? null : url.toExternalForm();
    return new GMLWorkspace_Impl( schema, featureTypes, rootFeature, context, schemaLocation );
  }

  public static void accept( final IFeaturePropertyVisitor visitor, int position, final IPropertyType ftp, final Object property )
  {
    if( ftp instanceof IRelationType )
    {
      if( property instanceof Feature )
        visitor.visit( position, ftp, (Feature) property );
      else if( property instanceof String )
        visitor.visit( position, ftp, (String) property );
      else if( property instanceof List )
        visitor.visit( position, ftp, (List) property );

      // Andreas: we dont know anything about associations with null-data; how to solve this?
    }
    else
      visitor.visit( position, ftp, property );
  }

}