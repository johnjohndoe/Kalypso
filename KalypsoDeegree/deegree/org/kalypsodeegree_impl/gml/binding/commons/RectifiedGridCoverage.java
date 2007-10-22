package org.kalypsodeegree_impl.gml.binding.commons;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * TODO: add setters/getters for the coverage-function
 * 
 * @author Dejan Antanaskovic, Gernot Belger
 */
public class RectifiedGridCoverage extends AbstractFeatureBinder implements ICoverage
{
  public static final QName QNAME = new QName( NS.GML3, "RectifiedGridCoverage" );

  private static final QName QNAME_PROP_GRID_DOMAIN = new QName( NS.GML3, "rectifiedGridDomain" );

  private static final QName QNAME_PROP_RANGE_SET = new QName( NS.GML3, "rangeSet" );

  public RectifiedGridCoverage( final Feature feature )
  {
    super( feature, RectifiedGridCoverage.QNAME );
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
    return (RectifiedGridDomain) getFeature().getProperty( RectifiedGridCoverage.QNAME_PROP_GRID_DOMAIN );
  }

  /**
   * Sets the grid domain, also updates the boundedBy property.
   * 
   * @param gridDomain
   *            The gridDomain to set.
   */
  public void setGridDomain( final RectifiedGridDomain gridDomain )
  {
    getFeature().setProperty( RectifiedGridCoverage.QNAME_PROP_GRID_DOMAIN, gridDomain );

    try
    {
      final GM_Envelope envelope = gridDomain.getGM_Envelope( gridDomain.getCoordinateSystem() );
      getFeature().setProperty( new QName( NS.GML3, "boundedBy" ), envelope );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @return Returns the rangeSet.
   */
  public RangeSetType getRangeSet( )
  {
    return (RangeSetType) getFeature().getProperty( RectifiedGridCoverage.QNAME_PROP_RANGE_SET );
  }

  /**
   * @param rangeSet
   *            The rangeSet to set.
   */
  public void setRangeSet( final RangeSetType rangeSet )
  {
    getFeature().setProperty( RectifiedGridCoverage.QNAME_PROP_RANGE_SET, rangeSet );
  }
}