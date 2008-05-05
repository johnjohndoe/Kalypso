package org.kalypso.grid.processes.raster2vector;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.grid.processes.raster2vector.collector.CollectorDataProvider;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface für die verschiedenen Ergebnisse des Raster2Lines algorythmus
 * 
 * @author belger
 */
public interface SegmentCollector
{
  public static final FeatureFactory FF = new FeatureFactory();

  public static final String FEATURE_BASE_ID = SegmentCollector.class.getName();

  public static final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

  public static final IMarshallingTypeHandler GM_OBJECT_HANDLER = registry.getTypeHandlerForClassName( GM_Object.class );

  public static final QName QNAME_SHAPE_FEATURE = new QName( FEATURE_BASE_ID, "FeatureTypeID" );

  public static final IValuePropertyType SHAPE_PROP = GMLSchemaFactory.createValuePropertyType( QNAME_SHAPE_FEATURE, new QName( FEATURE_BASE_ID, "Shape" ), GM_OBJECT_HANDLER, 0, 1, false );

  public static final IMarshallingTypeHandler DOUBLE_HANDLER = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );

  public static final IValuePropertyType ID_PROP = GMLSchemaFactory.createValuePropertyType( QNAME_SHAPE_FEATURE, new QName( "ID" ), DOUBLE_HANDLER, 0, 1, false );

  public static final IMarshallingTypeHandler STRING_HANDLER = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );

  public static final IValuePropertyType BEZ_PROP = GMLSchemaFactory.createValuePropertyType( QNAME_SHAPE_FEATURE, new QName( FEATURE_BASE_ID, "Name" ), STRING_HANDLER, 0, 1, false );

  public void addSegment( final int index, final LinkedCoordinate lc0, final LinkedCoordinate lc1, final Coordinate nearC0, final Coordinate nearC1 ) throws LinkedCoordinateException;

  public CollectorDataProvider[] getData( );
}