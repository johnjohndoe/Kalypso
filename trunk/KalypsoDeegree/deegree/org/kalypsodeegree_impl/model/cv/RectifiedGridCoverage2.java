package org.kalypsodeegree_impl.model.cv;

import java.lang.reflect.InvocationTargetException;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * TODO: add setters/getters for the coverage-function
 * 
 * @author Dejan Antanaskovic, Gernot Belger
 */
public class RectifiedGridCoverage2 extends AbstractFeatureBinder
{
  public static final QName QNAME_RECTIFIED_GRID_COVERAGE = new QName( NS.GML3, "RectifiedGridCoverage" );

  private static final QName QNAME_PROP_GRID_DOMAIN = new QName( NS.GML3, "rectifiedGridDomain" );

  private static final QName QNAME_PROP_RANGE_SET = new QName( NS.GML3, "rangeSet" );

  public static RectifiedGridCoverage2 createRectifiedGridCoverage( ) throws InvocationTargetException
  {
    final GMLSchema schema = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog().getSchema( NS.GML3, "3.1" );
    final IFeatureType featureType = schema.getFeatureType( RectifiedGridCoverage2.QNAME_RECTIFIED_GRID_COVERAGE );
    final Feature feature = FeatureFactory.createFeature( null, null, null, featureType, true );

    return new RectifiedGridCoverage2( feature );
  }

  public static RectifiedGridCoverage2 createRectifiedGridCoverage( final RectifiedGridDomain gridDomain, final RangeSetType rangeSet ) throws InvocationTargetException
  {
    final RectifiedGridCoverage2 coverage = RectifiedGridCoverage2.createRectifiedGridCoverage();
    coverage.setGridDomain( gridDomain );
    coverage.setRangeSet( rangeSet );
    return coverage;
  }

  public RectifiedGridCoverage2( final Feature feature )
  {
    super( feature, RectifiedGridCoverage2.QNAME_RECTIFIED_GRID_COVERAGE );
  }

  public static String getNameStatic( )
  {
    return "RectifiedGridCoverage2";
  }

  /**
   * @return Returns the gridDomain.
   */
  public RectifiedGridDomain getGridDomain( )
  {
    return (RectifiedGridDomain) getFeature().getProperty( RectifiedGridCoverage2.QNAME_PROP_GRID_DOMAIN );
  }

  /**
   * @param gridDomain
   *            The gridDomain to set.
   */
  public void setGridDomain( final RectifiedGridDomain gridDomain )
  {
    getFeature().setProperty( RectifiedGridCoverage2.QNAME_PROP_GRID_DOMAIN, gridDomain );
  }

  /**
   * @return Returns the rangeSet.
   */
  public RangeSetType getRangeSet( )
  {
    return (RangeSetType) getFeature().getProperty( RectifiedGridCoverage2.QNAME_PROP_RANGE_SET );
  }

  /**
   * @param rangeSet
   *            The rangeSet to set.
   */
  public void setRangeSet( final RangeSetType rangeSet )
  {
    getFeature().setProperty( RectifiedGridCoverage2.QNAME_PROP_RANGE_SET, rangeSet );
  }

}