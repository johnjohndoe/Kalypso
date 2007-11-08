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

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.xml.Mapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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
   *            unique id of the <CODE>Feature</CODE>
   * @param featureType
   *            <CODE>IFeatureType</CODE> of the <CODE>Feature</CODE>
   * @param properties
   *            properties (content) of the <CODE>Feature</CODE>
   * @return instance of a <CODE>Feature</CODE>
   */
  public static Feature createFeature( final Feature parent, final IRelationType parentRelation, final String id, final IFeatureType featureType, final Object[] properties )
  {
    return new Feature_Impl( parent, parentRelation, featureType, id, properties );
  }

  /**
   * Same as {@link #createFeature(Feature, String, IFeatureType, boolean, false)}.
   */
  public static Feature createFeature( final Feature parent, final IRelationType parentRelation, final String id, final IFeatureType featureType, final boolean initializeWithDefaults )
  {
    return createFeature( parent, parentRelation, id, featureType, initializeWithDefaults, 0 );
  }

  /**
   * Erzeugt ein Feature mit gesetzter ID und füllt das Feature mit Standardwerten.
   * 
   * @param initializeWithDefaults
   *            set <code>true</code> to generate default properties (e.g. when generating from UserInterface) <br>
   *            set <code>false</code> to not generate default properties (e.g. when reading from GML or so.)
   */
  public static Feature createFeature( final Feature parent, final IRelationType parentRelation, final String id, final IFeatureType featureType, final boolean initializeWithDefaults, final int depth )
  {
    if( featureType == null )
      throw new IllegalArgumentException( "must provide a featuretype" );

    final IPropertyType[] ftp = featureType.getProperties();

    final Feature feature = new Feature_Impl( parent, parentRelation, featureType, id, new Object[ftp.length] );

    // TODO: shouldn't we move this to the Feature_Impl constructor?
    for( final IPropertyType pt : ftp )
    {
      if( pt.isList() )
      {
        if( pt instanceof IRelationType )
          feature.setProperty( pt, FeatureFactory.createFeatureList( feature, (IRelationType) pt ) );
        else
          feature.setProperty( pt, new ArrayList() );
      }
      else
      {
        // leave it null
      }
    }

    if( initializeWithDefaults )
    {
      final Map<IPropertyType, Object> properties = FeatureFactory.createDefaultFeatureProperty( feature, depth );
      FeatureHelper.setProperties( feature, properties );
    }

    return feature;
  }

  /** Creates default FeatureProperties, used by LegendView */
  public static Map<IPropertyType, Object> createDefaultFeatureProperty( final Feature feature, final int depth )
  {
    final IPropertyType[] propTypes = feature.getFeatureType().getProperties();

    final Map<IPropertyType, Object> results = new LinkedHashMap<IPropertyType, Object>( propTypes.length );
    for( final IPropertyType ftp : propTypes )
    {
      final Object value = createDefaultFeatureProperty( feature, ftp, depth );
      if( value != null )
        results.put( ftp, value );
    }
    return results;
  }

  private static Object createDefaultFeatureProperty( final Feature feature, final IPropertyType ftp, final int depth )
  {
    final int minOccurs = ftp.getMinOccurs();
    // maybe also look at 'nillable'?
    final boolean isOptional = minOccurs == 0;

    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;

      // get default value from schema if possible
      final String defaultValue;
      if( vpt.hasDefault() )
        defaultValue = vpt.getDefault();
      else if( vpt.isFixed() )
        defaultValue = vpt.getFixed();
      else
        defaultValue = null;

      // Only fill non optional values with default value set
      if( isOptional || defaultValue == null )
        return null;

      final IMarshallingTypeHandler typeHandler = vpt.getTypeHandler();
      if( typeHandler != null && defaultValue != null )
      {
        try
        {
          return typeHandler.parseType( defaultValue );
        }
        catch( final ParseException e )
        {
          e.printStackTrace();
          return null;
        }
      }

      return Mapper.defaultValueforJavaType( vpt );
    }
    else if( ftp instanceof IRelationType )
    {
      final IRelationType rt = (IRelationType) ftp;
      if( ftp.isList() )
        return FeatureFactory.createFeatureList( feature, rt );

      if( depth == 0 || minOccurs == 0 || !rt.isInlineAble() || rt.isLinkAble() )
        return null;

      // we have a single, non-optional, inlinable, not-linkable feature here: create inner feature
      final GMLWorkspace workspace = feature.getWorkspace();
      if( workspace == null )
        return null;

      final int subDepth = depth == -1 ? -1 : depth - 1;
      return workspace.createFeature( feature, rt, rt.getTargetFeatureType(), subDepth );
    }

    return null;
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

  public static IPropertyType[] createVirtualFeatureTypeProperties( final IFeatureType realFeatureType )
  {
    final List<IPropertyType> result = new ArrayList<IPropertyType>();
    final VirtualFeatureTypeRegistry registry = VirtualFeatureTypeRegistry.getInstance();
    final IPropertyType[] properties = realFeatureType.getProperties();

    for( final IPropertyType ftp : properties )
    {
      final IPropertyType[] newFtp = registry.getVirtualFeatureTypePropertiesFor( ftp );
      for( final IPropertyType element : newFtp )
        result.add( element );
    }

    final IPropertyType[] vftp = registry.getVirtualFeatureTypePropertiesFor( realFeatureType );
    for( final IPropertyType element : vftp )
      result.add( element );

    return result.toArray( new IPropertyType[result.size()] );
  }

  public static GMLWorkspace createGMLWorkspace( final IGMLSchema schema, final Feature rootFeature, final URL context, final String schemaLocation, final IFeatureProviderFactory factory, NamespaceContext namespaceContext )
  {
    final IFeatureType[] featureTypes = schema.getAllFeatureTypes();
    return new GMLWorkspace_Impl( schema, featureTypes, rootFeature, context, null, schemaLocation, factory );
  }

  /**
   * create a new GMLWorkspace with a root feature for the given feature type
   */
  public static GMLWorkspace createGMLWorkspace( final QName rootFeatureQName, final URL context, final IFeatureProviderFactory factory ) throws InvocationTargetException
  {
    return createGMLWorkspace( rootFeatureQName, null, context, factory );
  }

  /**
   * create a new GMLWorkspace with a root feature for the given feature type
   */
  public static GMLWorkspace createGMLWorkspace( final QName rootFeatureQName, final String gmlVersion, final URL context, final IFeatureProviderFactory factory ) throws InvocationTargetException
  {
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final IGMLSchema schema = schemaCatalog.getSchema( rootFeatureQName.getNamespaceURI(), gmlVersion );
    final IFeatureType rootFeatureType = schema.getFeatureType( rootFeatureQName );
    return createGMLWorkspace( rootFeatureType, context, factory );
  }

  /**
   * create a new GMLWorkspace with a root feature for the given feature type
   */
  public static GMLWorkspace createGMLWorkspace( final IFeatureType rootFeatureType, final URL context, final IFeatureProviderFactory factory )
  {
    final IGMLSchema schema = rootFeatureType.getGMLSchema();
    final String schemaLocation = null;
    final Feature rootFeature = FeatureFactory.createFeature( null, null, "root", rootFeatureType, true );
    return FeatureFactory.createGMLWorkspace( schema, rootFeature, context, schemaLocation, factory, null );
  }
}